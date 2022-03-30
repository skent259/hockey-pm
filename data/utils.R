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
