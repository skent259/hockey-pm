library(hockeyR)
library(tidyverse)

#' Not in operator
`%ni%` <- Negate(`%in%`)

#' Get all unique values across the specified columns
#'
#' @param df A data-frame
#' @param .cols A vector of columns, or a vector of tidy-select values like
#'   `starts_with()`.
#'   
#' @return A vector of unique values
unique_across <- function(df, .cols) {
  df %>% 
    summarize(
      across(all_of(.cols), ~list(unique(.x)))
    ) %>% 
    unlist() %>% 
    unique()
}

#' Get all players from a data frame
#' 
#' @param df A data-frame with structure based on `hockeyR::load_pbp()`.
#' 
#' @return A vector of player names contained in `df`. 
get_all_players <- function(df) {
  x <- df %>% unique_across(c(starts_with("home_on_"), 
                              starts_with("away_on_"), 
                              ends_with("_goalie")))
  x[!is.na(x)]
}

#' Get non-goalie players from a data frame 
#' 
#' @inheritParams get_all_players
#' 
#' @return A vector of player names contained in `df`. 
get_nongoalie_players <- function(df) {
  x <- df %>% unique_across(c(starts_with("home_on_"), 
                              starts_with("away_on_")))
  x[!is.na(x)]
}

#' Get all goalies from a data frame
#' 
#' @param df A data-frame with structure based on `hockeyR::load_pbp()`.
#' 
#' @return A vector of goalie names contained in `df`. 
get_all_goalies <- function(df) {
  x <- df %>% unique_across(c(ends_with("_goalie")))
  x[!is.na(x)]
}

#' Get all teams from a data frame
#' 
#' @param df A data-frame with structure based on `hockeyR::load_pbp()`.
#' 
#' @return A vector of team abbreviations contained in `df`. 
get_all_teams <- function(df) {
  unique_across(df, c("home_abbreviation", "away_abbreviation"))
}

#' Build model data
#'
#' This function provides the main interface to build the model data for a
#' plus-minus metric based on goals from the specified hockey `season`.  It
#' returns a sparse matrix where each row is a goal and each column is either
#' 1.) the outcome, 2.) a flag for the teams involved, or 3.) a flag for the
#' players involved.  Each entry of the sparse matrix is 0, -1, or +1.  See the
#' formulation in Gramacy et al. (2013) "Estimating Player Contribution in
#' Hockey with Regularized Logistic Regression".
#'
#' @param season The season(s) of interest, as passed to `hockeyR::load_pbp()`.
#' @param method How to construct the data, with options `"combined"` for a
#'   marginal plus-minus statistic, `"off-def"` for separate offensive and
#'   defensive estimates, and `'off-def-ng"` for separate estimates but no
#'   goalie estimates. 
#'
#' @return A sparse matrix for modeling
build_model_data <- function(season, method = c("combined", "off-def", "off-def-ng")) {
  method <- match.arg(method, c("combined", "off-def", "off-def-ng"))
  
  goals <- hockeyR::load_pbp(season) %>% 
    filter(event_type == "GOAL")
  
  # Exclusions 
  goals <- goals %>% 
    filter(
      strength_state == "5v5", 
      strength_code %ni% c("PP", "SH"),
      empty_net == "FALSE",
      !is.na(away_goalie), 
      !is.na(home_goalie), 
      # !is.na(strength_code),
      period <= 3
    )
  
  # Reduce to necessary columns 
  goals <- goals %>% 
    select(
      event_team_type, 
      starts_with("home_on_"), 
      starts_with("away_on_"), 
      home_goalie,
      away_goalie, 
      home_abbreviation, 
      away_abbreviation
    )  
  
  # Convert to data-frame for filling sparse matrix
  sparse_df_from_row <- function(row, method) {
    # For each goal, specify the row, column, and value to fill the sparse
    # matrix according to the style of Gramacy et al. 2013.  
    # Columns will be "y", the teams involved, and the players involved. 
    
    i <- row$i
    if (method == "off-def-ng") {
      home_cols <- c(paste0("home_on_", 1:5))
      away_cols <- c(paste0("away_on_", 1:5))
    } else {
      home_cols <- c(paste0("home_on_", 1:5), "home_goalie")
      away_cols <- c(paste0("away_on_", 1:5), "away_goalie")
    }
    n_p <- length(home_cols)
    
    cols <- unlist(c(
      "y", # y
      row$home_abbreviation, row$away_abbreviation, # teams
      row[home_cols], row[away_cols] # players
    ))
    
    rows <- rep(i, length(cols))
    
    values <- c(
      ifelse(row$event_team_type == "home", 1, -1), # y
      1, -1, # teams 
      rep(1, n_p), rep(-1, n_p) # players
    )
    
    # home team is on offense if values[1] == 1, defense otherwise 
    def <- c(
      rep(NA, 3), # y, teams
      rep(values[1] == -1, n_p), rep(values[1] == 1, n_p) # players
    )
    
    return(tibble::tibble(rows, cols, values, def))
  }
  
  sparse_df_spec <- goals %>% 
    mutate(i = row_number()) %>% 
    transpose() %>% 
    map(~sparse_df_from_row(.x, method)) %>% 
    bind_rows()
  
  players <- switch(
    method,
    "off-def-ng" = get_nongoalie_players(goals),
    get_all_players(goals) # default 
  )
  col_names <- c("y", get_all_teams(goals), players)
  
  sparse_df_spec <- sparse_df_spec %>% 
    mutate(col_ind = match(cols, col_names)) 
  
  if (method %in% c("off-def", "off-def-ng")) {
    # Need to push the column_index for defensive players to a separate set
    sparse_df_spec <- sparse_df_spec %>% 
      mutate(across(col_ind, ~if_else(def, .x + length(players), .x, missing = .x)))
    
    col_names <- c(col_names, players)
  }
  
  missing_col <- max(sparse_df_spec$col_ind) < length(col_names)
  if (missing_col) {
    sparse_df_spec <- sparse_df_spec %>% 
      tibble::add_row(rows = 1, values = 0, col_ind = length(col_names))
  }
  
  mat <- Matrix::sparseMatrix(
    sparse_df_spec$rows, 
    sparse_df_spec$col_ind, 
    x = sparse_df_spec$values
  )
  
  colnames(mat) <- col_names
  
  return(mat)
}

#' Run tests on model data
#'
#' Checks the model data output for a few conditions, including:
#' - Player rows should sum to 0
#' - Player rows should have 12 non-zero entries
#' - Team rows should sum to 0
#' - Team rows should have 2 non-zero entries
#' 
#' This function will print the errors if it sees any.  
#'
#' Note: this is probably better to have in a testing framework, but this works
#' for now.  
#' 
#' @param mat A matrix output from `build_model_data()`.
#' @inheritParams build_model_data
#' 
#' @return Nothing
check_model_data <- function(mat, method = c("combined", "off-def", "off-def-ng")) {
  method <- match.arg(method, c("combined", "off-def", "off-def-ng"))
  X_teams <- mat[, 2:32]
  X_play <- mat[, 33:ncol(mat)]
  
  # Player rows should sum to 0
  check <- sum(Matrix::rowSums(X_play) != 0)
  if (check != 0) {
    print("Some player rows are non-zero in sum")
  }
  
  # Player rows should have 12 non-zero entries
  n_nonzero <- switch(method, "off-def-ng" = 10, 12)
  check <- sum(Matrix::rowSums(abs(X_play)) != n_nonzero) 
  if (check != 0) {
    print("Some player rows don't have 12 non-zero entries")
  }
  
  # Team rows should sum to 0
  check <- sum(Matrix::rowSums(X_teams) != 0)
  if (check != 0) {
    print("Some team rows are non-zero in sum")
  }
  
  # Team rows should have 2 non-zero entries
  check <- sum(Matrix::rowSums(abs(X_teams)) != 2)
  if (check != 0) {
    print("Some team rows don't have 2 non-zero entries")
  }
  
}


#' Count events in between changes
#' @param df A data frame of play-by-play hockey information, from
#'   `hockeyR::load_pbp()`.
#' @param event The event type to count.
#' @return A vector of counts, with length equal to the number of changes.
count_events <- function(df, event = "GOAL", team = "home") {
  ev <- df$event_type  
  ev_team <- df$event_team_type
  changes <- which(df$event_type == "CHANGE")
  counts <- numeric(length(changes))
  cnt <- 0
  j <- 1 
  for (i in 1:nrow(df)) {
    if (ev[i] == "CHANGE") {
      counts[j-1] <- cnt
      cnt <- 0
      j <- j + 1
    } else if (ev[i] == event & ev_team[i] == team) {
      cnt <- cnt + 1
    }
  }
  return(counts)
}

#' Build shots-on-goal data
#'
#' This function creates a data frame where each row represents a 'shift' in
#' hockey. For each shift, information is computed about the number of goals,
#' shots, missed shots, and blocked shots for both the home and away team.
#' Additional columns provide information on which players were on the ice for
#' that shift for both the home and away teams, and the team that they belong
#' to.
#'
#' Shifts are excluded if they take up no time (e.g. at the start of a game, or
#' between periods) of if they do not occur during a regular time, 5v5 scenario.
#' This excludes power plays, 4v4, empty net, and all overtime shifts.
#' Play-by-play data is pulled using `hockeyR::load_pbp()`.
#'
#' @param season The season(s) of interest, as passed to `hockeyR::load_pbp()`.
#' @return A data frame with event counts and shift length for all changes
build_sog_data <- function(season) {
  pbp <- hockeyR::load_pbp(season) 
  
  changes <- which(pbp$event_type == "CHANGE")
  
  change_df <- pbp[changes, ] %>% 
    dplyr::select(
      event_team_type, 
      starts_with("home_on_"), 
      starts_with("away_on_"), 
      home_goalie,
      away_goalie, 
      home_abbreviation, 
      away_abbreviation,
      strength_state,
      strength_code, 
      period
    )
  
  chng_times <- pbp$game_seconds[changes]
  change_df$shift_time <- dplyr::lead(chng_times) - chng_times
  
  change_df$n_goal_home <- count_events(pbp, "GOAL", "home")
  change_df$n_goal_away <- count_events(pbp, "GOAL", "away")
  change_df$n_shot_home <- count_events(pbp, "SHOT", "home")
  change_df$n_shot_away <- count_events(pbp, "SHOT", "away")
  change_df$n_missshot_home <- count_events(pbp, "MISSED_SHOT", "home")
  change_df$n_missshot_away <- count_events(pbp, "MISSED_SHOT", "away")
  change_df$n_blkdshot_home <- count_events(pbp, "BLOCKED_SHOT", "home")
  change_df$n_blkdshot_away <- count_events(pbp, "BLOCKED_SHOT", "away")
  
  # Exclusions
  # remove double changes, end of game, beginning of game 
  change_df <- dplyr::filter(change_df, shift_time > 0)
  # remove any shift that wasn't 5v5 in regular time 
  change_df <- change_df %>% 
    dplyr::filter(
      strength_state == "5v5", 
      strength_code %ni% c("PP", "SH"),
      !is.na(away_goalie), 
      !is.na(home_goalie), 
      period <= 3
    )
  
  change_df <- change_df %>% 
    dplyr::select(
      shift_time, 
      starts_with("n_"), 
      everything(),
      -strength_state, 
      -strength_code, 
      -period,
      -event_team_type
    )
  
  return(change_df)
}

#' Build shots-on-goal model data
#'
#' This function creates a data frame where each row represents a 'shift' in
#' hockey. For each shift, information is recorded about an outcome (i.e. shots
#' on goal), and the teams and players involved on the ice during that shift. It
#' returns a sparse matrix where columns can be used for modeling: 1.) the
#' outcome, 2.) shift time, 3.) offensive and defensive team flags, 4.)
#' offensive and defensive players flags, where goalies only have defensive
#' flags.  Entries besides shift time come from 0, -1, or +1.  More details on
#' the formulation are available in `data_explanation.Rmd`.
#'
#' Shifts are excluded if they take up no time (e.g. at the start of a game, or
#' between periods) of if they do not occur during a regular time, 5v5 scenario.
#' This excludes power plays, 4v4, empty net, and all overtime shifts.
#' Play-by-play data is pulled using `hockeyR::load_pbp()`.
#'
#' Note: the shift-level data is much larger than goal-level, and takes a few
#' minutes to process. 
#'
#' @param season The season(s) of interest, as passed to `hockeyR::load_pbp()`.
#' @param method How to construct the data, with options `"off-def"` for
#'   separate offensive and defensive estimates.
#' @param outcome A character of vector indicating the outcome(s) to use.  If a
#'   vector is used, the total outcome will be calculated.  Options follow from
#'   the output of `build_sog_data()`, which include `c('n_goal', 'n_shot',
#'   'n_missshot', 'n_blkdshot')` or any combination thereof.
#'
#' @return A sparse matrix for modeling
build_sog_model_data <- function(season, method = c("off-def"), outcome = "n_shot") {
  method <- match.arg(method, c("off-def"))
  outcome <- match.arg(
    outcome,
    c("n_goal", "n_shot", "n_missshot", "n_blkdshot"),
    several.ok = TRUE
  )
  
  change_df <- build_sog_data(season)
  
  # Convert to data-frame for filling sparse matrix
  sparse_df_from_row <- function(row, method) {
    # For each shift, specify the row, column, and value to fill the sparse 
    # matrix such that we have
    # - y: shots on goal by team A in shift i
    # - team effects (off & def): +1 on off effect of team A, -1 on def effect of their opponent
    # - player effects (off): +1 on off effect of all players on team A on ice
    # - player effects (def): -1 on def effect of all players on opponent of team A
    
    i <- row$i
    home_cols <- c(paste0("home_on_", 1:5))
    away_cols <- c(paste0("away_on_", 1:5))
    home_outc <- paste0(outcome, "_home")
    away_outc <- paste0(outcome, "_away")
    
    n_p <- length(home_cols)
    
    cols <- unlist(c(
      "y", # y
      "shift_time", # shift time
      paste0(row$home_abbreviation, c("_O", "_D")), # teams 
      paste0(row$away_abbreviation, c("_O", "_D")), # teams
      row[home_cols], row[away_cols], # players
      row$home_goalie, row$away_goalie # goalie
    ))
    cols <- c(cols, cols)
    
    rows <- c(
      rep(2*i-1, length(cols) / 2), 
      rep(2*i, length(cols) / 2)
    )
    
    values <- c(
      ## shots on goal by home team
      sum(unlist(row[home_outc])), # y
      row$shift_time, # shift time
      1, 0, # teams (home off, home def)
      0, -1, # teams (away off, away def)
      rep(1, n_p), rep(-1, n_p), # players (home, away)
      0, -1, # goalie (home, away)
      
      ## shots on goal by away team
      sum(unlist(row[away_outc])), # y
      row$shift_time, # shift time
      0, -1, # teams (home off, home def)
      1, 0, # teams (away off, away def)
      rep(-1, n_p), rep(1, n_p), # players (home, away)
      -1, 0 # goalie (home, away)
    )
    
    # home team is on offense in first row, defense otherwise 
    # note: goalies only have one effect, will be at end so don't need "def"
    def <- c(
      rep(NA, 6), # y, shift time, teams
      rep(FALSE, n_p), rep(TRUE, n_p), # players (home, away)
      rep(FALSE, 2), # goalie (home, away)
      rep(NA, 6),
      rep(TRUE, n_p), rep(FALSE, n_p), # players (home, away)
      rep(FALSE, 2) # goalie (home, away)
    )
    
    return(tibble::tibble(rows, cols, values, def))
  }
  
  sparse_df_spec <- change_df %>% 
    mutate(i = row_number()) %>% 
    transpose() %>% 
    map(~sparse_df_from_row(.x, method)) %>% 
    bind_rows()
  
  players <- get_nongoalie_players(change_df)
  goalies <- get_all_goalies(change_df)
  teams <- get_all_teams(change_df)
  col_names <- c(
    "y", "shift_time", 
    as.vector(t(outer(teams, c("_O", "_D"), FUN = paste0))),
    rep(players, 2), goalies
  )
  
  sparse_df_spec <- sparse_df_spec %>% 
    mutate(col_ind = match(cols, col_names))  %>% 
    mutate(across(col_ind,
                  ~if_else(def, .x + length(players), .x, missing = .x)))
  
  missing_col <- max(sparse_df_spec$col_ind) < length(col_names)
  if (missing_col) {
    sparse_df_spec <- sparse_df_spec %>% 
      tibble::add_row(rows = 1, values = 0, col_ind = length(col_names))
  }
  
  mat <- Matrix::sparseMatrix(
    sparse_df_spec$rows, 
    sparse_df_spec$col_ind, 
    x = sparse_df_spec$values
  )
  
  colnames(mat) <- col_names
  return(mat)
}


#' Run tests on shots-on-goal model data
#'
#' Checks the model data output for a few conditions, including:
#' - Player rows should sum to 0
#' - Player rows should have 12 non-zero entries
#' - Team rows should sum to 0
#' - Team rows should have 2 non-zero entries
#' 
#' This function will print the errors if it sees any.  
#'
#' Note: this is probably better to have in a testing framework, but this works
#' for now.  
#' 
#' @param mat A matrix output from `build_sog_model_data()`.
#' @inheritParams build_sog_model_data
#' 
#' @return Nothing
check_sog_model_data <- function(mat, method = c("off-def")) {
  method <- match.arg(method, c("off-def"))
  X_teams <- mat[, 3:64] # 2 cols for each team 
  X_play <- mat[, 65:ncol(mat)]
  
  # Sum across team rows should be 0
  check <- sum(Matrix::rowSums(X_teams)) != 0
  if (check != 0) {
    print("Team rows don't cancel out")
  }
  
  # Player rows should sum to -1
  check <- sum(Matrix::rowSums(X_play) != -1)
  if (check != 0) {
    print("Some player rows are not -1 in sum")
  }
  
  # Player rows should have 11 non-zero entries
  n_nonzero <- 11
  check <- sum(Matrix::rowSums(abs(X_play)) != n_nonzero) 
  if (check != 0) {
    print("Some player rows don't have 11 non-zero entries")
  }
  
  # Team rows should have 2 non-zero entries
  check <- sum(Matrix::rowSums(abs(X_teams)) != 2)
  if (check != 0) {
    print("Some team rows don't have 2 non-zero entries")
  }
  
  # Team columns (in pairs) should cancel out
  check <- all(colSums(matrix(Matrix::colSums(X_teams), nrow = 2)) != 0)
  if (check != 0) {
    print("Some team shifts may be missing, offensive and defensive numbers don't cancel")
  }
  
  n_nongoalie <- ncol(X_play) - length(unique(colnames(X_play)))
  player_col_sums <- Matrix::colSums(X_play[, seq_len(2*n_nongoalie)])
  check <- all(rowSums(matrix(player_col_sums, ncol = 2)) != 0)
  if (check != 0) {
    print("Some players don't have equal number of offensive and defensive shifts")
  }
  
}


#' Shorten the outcomes string
#' 
#' @param x The argument `outcome` in `build_sog_model_data()`. 
#' @return A short string 
short_outcome <- function(x) {
  shorten <- function(x) {
    x %>% 
      str_remove_all("n_") %>% 
      str_trunc(width = 2, ellipsis = "")
  }
  
  x <- map_chr(x, shorten)
  paste0(x, collapse = "-")
}
