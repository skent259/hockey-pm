library(hockeyR)
library(tidyverse)

#' Not in operator
`%ni%` <- Negate(`%in%`)

#' Get all players from a data frame
#' 
#' @param df A data-frame with structure based on `hockeyR::load_pbp()`.
#' 
#' @return A vector of player names contained in `df`. 
get_all_players <- function(df) {
  
  x <- df %>% summarize(
    across(c(starts_with("home_on_"), 
             starts_with("away_on_"), 
             ends_with("_goalie")),
           ~list(unique(.x)))
  ) %>% 
    unlist() %>% 
    unique()
  
  x[!is.na(x)]
}

#' Get all teams from a data frame
#' 
#' @param df A data-frame with structure based on `hockeyR::load_pbp()`.
#' 
#' @return A vector of team abbreviations contained in `df`. 
get_all_teams <- function(df) {
  df %>% 
    summarize(across(c(home_abbreviation, away_abbreviation),
                     ~list(unique(.x)))) %>% 
    unlist() %>% 
    unique()
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
#'   marginal plus-minus statistic, or `"off-def"` for separate offensive and
#'   defensive estimates.  
#'
#' @return A sparse matrix for modeling
build_model_data <- function(season, method = c("combined", "off-def")) {
  method <- match.arg(method, c("combined", "off-def"))
  
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
  sparse_df_from_row <- function(row) {
    # For each goal, specify the row, column, and value to fill the sparse
    # matrix according to the style of Gramacy et al. 2013.  
    # Columns will be "y", the teams involved, and the players involved. 
    
    i <- row$i
    home_cols <- c(paste0("home_on_", 1:5), "home_goalie")
    away_cols <- c(paste0("away_on_", 1:5), "away_goalie")
    
    cols <- unlist(c(
      "y", # y
      row$home_abbreviation, row$away_abbreviation, # teams
      row[home_cols], row[away_cols] # players
    ))
    
    rows <- rep(i, length(cols))
    
    values <- c(
      ifelse(row$event_team_type == "home", 1, -1), # y
      1, -1, # teams 
      rep(1, 6), rep(-1, 6) # players
    )
    
    # home team is on offense if values[1] == 1, defense otherwise 
    def <- c(
      rep(NA, 3), # y, teams
      rep(values[1] == -1, 6), rep(values[1] == 1, 6) # players
    )
    
    return(tibble(rows, cols, values, def))
  }
  
  sparse_df_spec <- goals %>% 
    mutate(i = row_number()) %>% 
    transpose() %>% 
    map(sparse_df_from_row) %>% 
    bind_rows()
  
  players <- get_all_players(goals)
  col_names <- c("y", get_all_teams(goals), players)
  
  sparse_df_spec <- sparse_df_spec %>% 
    mutate(col_ind = match(cols, col_names)) 
  
  if (method == "off-def") {
    # Need to push the column_index for defensive players to a separate set
    sparse_df_spec <- sparse_df_spec %>% 
      mutate(across(col_ind, ~if_else(def, .x + length(players), .x, missing = .x)))
    
    col_names <- c(col_names, players)
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
#' 
#' @return Nothing
check_model_data <- function(mat) {
  y <- mat[, 1]
  X_teams <- mat[, 2:32]
  X_play <- mat[, 33:ncol(mat)]
  
  # Player rows should sum to 0
  check <- sum(Matrix::rowSums(X_play) != 0)
  if (check != 0) {
    print("Some player rows are non-zero in sum")
  }
  
  # Player rows should have 12 non-zero entries
  check <- sum(Matrix::rowSums(abs(X_play)) != 12) 
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






