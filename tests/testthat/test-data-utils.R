library(here)
library(testthat)

source(here("data/utils.R"))
season <- c("2021")

## name --------------------------------------------------------------#
test_that("`get_` methods work", {
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
  
  p <- get_all_players(goals)
  p_ng <- get_nongoalie_players(goals)
  t <- get_all_teams(goals)
  
  expect_equal(length(p), 972)
  expect_equal(length(p_ng), 878)
  expect_equal(length(t), 31)
})


test_that("`build_model_data()` passes checks", {
  for (method in c("combined", "off-def", "off-def-ng")) {
    out <- build_model_data(season, method)
    expect_silent(check_model_data(out, method))
  }
})

