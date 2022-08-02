#' Pull Seasons from file name 
#' 
#' @param fname The data file name 
#' @return A string like "s'19'20'21" indicating seasons
pull_seasons <- function(fname) {
  # Seasons start with 's' and contain '
  s <- tools::file_path_sans_ext(basename(fname)) 
  s <- stringr::str_split(s, "_")[[1]]
     
  s[which(stringr::str_detect(s, "s'"))]
}

#' Pull model from file name 
#' 
#' @param fname The data file name 
#' @return A string like "ppool_off-def" indicating model
pull_modname <- function(fname) {
  # Seasons start with 's' and contain '
  s <- tools::file_path_sans_ext(basename(fname)) 
  s <- stringr::str_split(s, "_")[[1]]
  ind <- which(grepl("s\'", s))
  paste0(s[1:(ind-1)], collapse = "_")
}

#' Pull outcome from file name 
#' 
#' Only tested for files like 'sog-model-data_o-mi-bl_s'21_2022-04-25.rds'
#' 
#' @param fname The data file name 
#' @return A string like "mi-bl" indicating model
pull_outcome <- function(fname) {
  # Seasons start with 's' and contain '
  s <- tools::file_path_sans_ext(basename(fname)) 
  s <- stringr::str_split(s, "_")[[1]]
  ind <- which(grepl("o-", s))
  stringr::str_remove_all(s[ind], "o-")
}

# Combine player effect and team effect samples with matrix multiplication.
#
# I use the get_rosters function from the HockeyR package to obtain players'
# team memberships. The function get_player_stats_hr from the HockeyR package
# would be an alternative to getting players' team memberships, but I've found
# that it's buggy. Note that if a player is traded during the season, they
# appear on both teams' rosters for that season so there will be players (e.g.
# Gustav Nyquist in 2019) who get multiple columns ("Gustav.Nyquist:SJS" and
# "Gustav.Nyquist:DET") in the output
#' @param pm_fit An object containing player and team effect MCMC draws, either
#'   the Stan model fit or a data frame
#' @param season An integer giving the season in which want to check players'
#'   team memberships.
#' @param team_names Vector of team abbreviations (Note that Vegas should be
#'   "VEG" instead of "VGK" for it to work with get_rosters function)
#' @param player_names Vector of player names
#'   
#' @return A matrix with a column of posterior samples for each player-team
#'   combination
get_ppt_draws <- function(pm_fit, season, team_names, player_names) {
  
  #Obtain data frame of posterior samples 
  if (!is.data.frame(pm_fit)) {
    df_of_draws <- posterior::as_draws_df(pm_fit) %>%
      select(starts_with(c("alfa", "beta")))
  } else {
    df_of_draws = pm_fit
  }
  
  if(length(player_names)+length(team_names) != ncol(df_of_draws)) {
    rlang::warn(message = "`team_names` and `player_names` should match alpha columns and beta columns of pm_fit")
  }
  
  np = length(player_names); nt=length(team_names)
  X = matrix(0, nrow = np+nt, ncol = 1.2*np) #Extra room in case there are players on multiple teams
  colnames(X) = seq_len(ncol(X))
  t = 1 #Counter for iteration through teams
  p = 1 #Counter for iteration through players
  
  
  roster <- here("data/roster-data_s'18'19'20'21_2022-07-30.rds") %>% 
    readRDS() %>% 
    select(-season) %>% 
    filter(season_short == season)
  
  for (team in team_names) {
    team_players <- roster %>% filter(team_abbr == team) %>% pull(player_match)
    ms <- match(team_players, player_names)
    
    for (m in ms[!is.na(ms)]) {
      X[c(t, nt+m) , p] = 1
      colnames(X)[p] = paste(player_names[m], team_names[t], sep=":") 
      p = p+1
    }  
    t = t+1
  }
  
  ppt_draws = as.matrix(df_of_draws) %*% X[,1:(p-1)]
  return(ppt_draws)  
}



#' Plot posterior intervals for parameters
#'
#' Compare to `bayesplot::mcmc_intervals()`, but uses a slightly different
#' aesthetic. It also orders the parameters
#'
#' @param fit An object containing MCMC draws, i.e. the Stan model fit.
#' @param pars A character vector of parameter names, or a tidy parameter
#'   selection from `vars(...)`.
#' @param names Names that correspond to the paramters and will be used for the
#'   y axis in plotting.
#' @param ... Further arguments passed to `bayesplot::mcmc_intervals_data()`. In
#'   particular this allows for changing the confidence interval size with
#'   `prob` and `prob_outer` from the defaults of `0.5` and `0.9`, respectively.
#'   
#' @return A ggplot object. 
plot_post_parameter <- function(fit, pars, names, ..., top = NULL, roster_position = NULL) {
  
  plot_data <- bayesplot::mcmc_intervals_data(fit, pars = pars, ...)
  plot_data$name <- names
  
  if (nrow(plot_data) != length(names)) {
    rlang::warn("`names` should be the same length as parameters selected.")
  }
  
  if (!is.null(roster_position)) {
    plot_data <- plot_data %>% 
      left_join(roster_position, by = c("name" = "player_match")) %>% 
      filter(!is.na(position))
  }

  if (!is.null(top)) {
    if (top > 0) {
      plot_data <- slice_max(plot_data, order_by = m, n = top)  
    } else if (top < 0) {
      plot_data <- slice_min(plot_data, order_by = m, n = abs(top))
    }
  }
  plot_data$name <- fct_reorder(plot_data$name, plot_data$m)
  
  p <- ggplot2::ggplot(plot_data,
                  ggplot2::aes(y = name, yend = name)) +
    # Plot the interval as three separate geoms: segment, segment, and errorbar
    ggplot2::geom_segment(
      ggplot2::aes(x = ll, xend = hh),
      size = 0.5, color = "grey40"
    ) + 
    ggplot2::geom_segment(
      ggplot2::aes(x = l, xend = h),
      size = 1.5, color = "grey30"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = m, xmin = m, xmax = m),
      width = 1, color = "white"
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey80") + 
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = element_blank()) + 
    ggplot2::labs(x = "Estimate", y = NULL)
  
  if (!is.null(roster_position)) {
    p <- p + 
      facet_grid(rows = vars(position), 
                 labeller = "label_both",
                 scales = "free",
                 space = "free")
  }
  
  return(p)
}


#' Plot Coefficients Density by Position
#'
#' For each position, randomly select a player, then randomly select a
#' coefficient from them from a chain/iteration. Returns a plot of the 
#' 
#' @inheritParams plot_post_parameter
#' @param roster The roster for the season indicating players and positions
#' @param n_samp The number of samples to use for each position
#' @return A ggplot object. 
plot_density_by_position <- function(fit, vars, names, roster, n_samp = 10000) {
  
  draws <- posterior::as_draws_df(fit) %>% 
    select(!!!vars) %>% 
    suppressWarnings()
  
  plot_data <- tibble()
  for (pos in c("W", "D", "C", "G")) {
    pos_players <- roster %>% filter(position == pos) %>% pull(player_match)
    pos_players <- intersect(pos_players, names)
    
    if (length(pos_players) > 0) {
      rs <- tibble(
        position = pos, 
        player = sample(pos_players, size = n_samp, replace = TRUE),
        row = sample(1:4000, size = n_samp, replace = TRUE)
      ) %>% 
        mutate(
          col = match(player, names),
          val = map2_dbl(row, col, ~draws[[.x, .y]])
        )
      
      plot_data <- bind_rows(plot_data, rs)
    }
  }
  
  ggplot2::ggplot(plot_data, 
                  ggplot2::aes(val, color = position)) +
    ggplot2::geom_density() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Estimate", y = "Density")
}

#' Plot Rhat for multiple parameters
#'
#' This is really just a convenience function around `bayesplot::mcmc_rhat()`
#' and `patchwork::wrap_plots()`.
#'
#' @inheritParams plot_post_parameter
#' @param par_list A vector of parameter names passed to `bayesplot::rhat()` to
#'   pull the corresponding rhat values.  
#'   
#' @return A ggplot2 and patchwork object 
plot_rhat_multi <- function(fit, par_list) {
  one_plot <- function(par) {
    rhats <- bayesplot::rhat(fit, pars = par)
    bayesplot::mcmc_rhat(rhats) +
      labs(subtitle = stringr::str_to_title(par))
  }
  p_rhat <- purrr::map(par_list, ~one_plot(.x))
  
  patchwork::wrap_plots(p_rhat, ncol = 1) +
    patchwork::plot_layout(guides = "collect") 
}

#' Plot effective sample size for multiple parameters 
#' 
#' @inheritParams plot_rhat_multi
#' @return A ggplot2 and patchwork object
plot_neff_multi <- function(fit, par_list) {
  one_plot <- function(par) {
    rhats <- bayesplot::neff_ratio(fit, pars = par)
    bayesplot::mcmc_neff(rhats) +
      labs(subtitle = stringr::str_to_title(par))
  }
  p_rhat <- purrr::map(par_list, ~one_plot(.x))
  
  patchwork::wrap_plots(p_rhat, ncol = 1) +
    patchwork::plot_layout(guides = "collect") 
}