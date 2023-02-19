#' @title Plot central model output from a jitter analysis
#' @inheritParams plot_annual
#' @param jitter_fit A list of \code{\link[gadgetutils]{g3_fit}} objects originating from \code{\link[gadgetutils]{g3_jitter}}.
#' @param vars Character vector giving the variables to plot. Only the default parameters are currently implemented.
#' @param ncol Number of columns to be used in the plot
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_jitter <- function(jitter_fit, vars = c("total.biomass", "hr", "recruitment"), base_size = 8, ncol = 1) {

  if("hr" %in% vars) {
    hr_dat <- lapply(seq_along(jitter_fit), function(i) {
      plot_hr(jitter_fit[[i]], min_catch_length = 0, return_data = TRUE) %>%
        dplyr::select(.data$year, .data$step, .data$value) %>%
        dplyr::mutate(run = i, .before = 1)
    }) %>%
      dplyr::bind_rows()

    hr_plot <- ggplot2::ggplot(
      hr_dat, ggplot2::aes(.data$year, .data$value, color = factor(.data$run))) +
      ggplot2::geom_line() +
      ggplot2::labs(
        y = "Total harvest rate",
        x = 'Year', color = 'Jitter run') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }

  if("total.biomass" %in% vars) {
    biom_dat <- lapply(seq_along(jitter_fit), function(i) {
      plot_biomass(jitter_fit[[i]], min_catch_length = 0, return_data = TRUE) %>%
        dplyr::select(.data$year, .data$step, .data$value) %>%
        dplyr::mutate(run = i, .before = 1)
    }) %>%
      dplyr::bind_rows()

    biom_plot <- ggplot2::ggplot(
      biom_dat, ggplot2::aes(.data$year, .data$value, color = factor(.data$run))) +
      ggplot2::geom_path() +
      ggplot2::labs(
        y = "Total biomass (kt)",
        x='Year',col='Jitter run') +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }

  if("recruitment" %in% vars) {
    rec_dat <- lapply(seq_along(jitter_fit), function(i) {
      plot_rec(jitter_fit[[i]], stocks = "total", return_data = TRUE) %>%
        dplyr::select(.data$year, .data$value) %>%
        dplyr::mutate(run = i, .before = 1)
    }) %>%
      dplyr::bind_rows()

    rec_plot <- ggplot2::ggplot(
      rec_dat, ggplot2::aes(.data$year, .data$value, color = factor(.data$run))) +
      ggplot2::geom_path() +
      ggplot2::labs(
        y = "Recruitment (millions)",
        x='Year',col='Jitter run') +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  }

  plot_list <-
    list(
      if("total.biomass" %in% vars) biom_plot + ggplot2::theme(legend.position = "none"),
      if("hr" %in% vars) hr_plot + ggplot2::theme(legend.position = "none"),
      if("recruitment" %in% vars) rec_plot + ggplot2::theme(legend.position = "none")
    )

  plot_list <- Filter(Negate(is.null), plot_list)

  cowplot::plot_grid(plotlist = plot_list, ncol = ncol)
}
