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
plot_post_parameter <- function(fit, pars, names, ..., top = NULL) {
  
  plot_data <- bayesplot::mcmc_intervals_data(fit, pars = pars, ...)
  plot_data$name <- names
  
  if (nrow(plot_data) != length(names)) {
    rlang::warn("`names` should be the same length as parameters selected.")
  }
  
  if (!is.null(top)) {
    if (top > 0) {
      plot_data <- slice_max(plot_data, order_by = m, n = top)  
    } else if (top < 0) {
      plot_data <- slice_min(plot_data, order_by = m, n = abs(top))
    }
  }
  order <-  dplyr::arrange(plot_data, m)
  
  ggplot2::ggplot(plot_data,
                  ggplot2::aes(y = parameter, yend = parameter)) +
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
    ggplot2::scale_y_discrete(labels = order$name, limits = order$parameter) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = element_blank()) + 
    ggplot2::labs(x = "Estimate", y = NULL)
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