#' @title Plot central model output from a jitter analysis
#' @inheritParams plot_annual
#' @param jitter_fit A list of \code{\link[gadgetutils]{g3_fit}} objects originating from \code{\link[gadgetutils]{g3_jitter}}.
#' @param vars Character vector giving the variables to plot. Only the default parameters are currently implemented.
#' @param ncol Number of columns to be used in the plot. Set to \code{NULL} for standard layout.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_jitter <- function(jitter_fit, vars = c("nll.summary", "total.biomass", "hr", "recruitment"), base_size = 8, ncol = NULL) {

  if("nll.summary" %in% vars) {
    nll_dat <- bind_fit_components(jitter_fit, 'score') %>%
      dplyr::mutate(id = factor(.data$id, levels = 1:length(unique(.data$id))))

    nll_plot <- ggplot2::ggplot(nll_dat) +
      ggplot2::geom_point(ggplot2::aes(y = .data$nll, x = 1, color = .data$id),
                          pch = 21, size = 2) +
      # ggplot2::geom_text(
      #   ggplot2::aes(
      #     x = -Inf, y = Inf,
      #     label = paste0("sd = ", round(sd(.data$nll), 0), "\n",
      #                    "CV = ", round(100*sd(.data$nll)/mean(.data$nll), 1), " %")
      #     ),
      #     vjust = 2, hjust = -1) +
          ggplot2::labs(
            y = "Negative log-likelihood",
            color = 'Jitter run',
            title = paste0("sd = ", round(stats::sd(nll_dat$nll), 0), "\n",
                           "CV = ", round(100*stats::sd(nll_dat$nll)/mean(nll_dat$nll), 1), " %")
            ) +
          ggplot2::scale_color_viridis_d() +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            title = ggplot2::element_text(size = base_size)
          )
  }

  if("hr" %in% vars) {
    hr_dat <- lapply(seq_along(jitter_fit), function(i) {
      plot_hr(jitter_fit[[i]], min_catch_length = 0, return_data = TRUE) %>%
        dplyr::select(.data$year, .data$step, .data$value) %>%
        dplyr::mutate(id = i, .before = 1)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(id = factor(.data$id, levels = 1:length(unique(.data$id))))

    hr_plot <- ggplot2::ggplot(
      hr_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
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
        dplyr::mutate(id = i, .before = 1)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(id = factor(.data$id, levels = 1:length(unique(.data$id))))

    biom_plot <- ggplot2::ggplot(
      biom_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
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
        dplyr::mutate(id = i, .before = 1)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(id = factor(.data$id, levels = 1:length(unique(.data$id))))

    rec_plot <- ggplot2::ggplot(
      rec_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
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

  if(is.null(ncol) & all(vars == c("nll.summary", "total.biomass", "hr", "recruitment"))) {

    plot_list <-
      list(
        if("total.biomass" %in% vars) biom_plot + ggplot2::theme(legend.position = "none"),
        if("hr" %in% vars) hr_plot + ggplot2::theme(legend.position = "none"),
        if("recruitment" %in% vars) rec_plot + ggplot2::theme(legend.position = "none")
      )

    cowplot::plot_grid(
      nll_plot, cowplot::plot_grid(plotlist = plot_list, ncol = 1), ncol = 2,
      rel_widths = c(2,8)
    )

  } else {
    plot_list <-
      list(
        if("nll.summary" %in% vars) nll_plot,
        if("total.biomass" %in% vars) biom_plot + ggplot2::theme(legend.position = "none"),
        if("hr" %in% vars) hr_plot + ggplot2::theme(legend.position = "none"),
        if("recruitment" %in% vars) rec_plot + ggplot2::theme(legend.position = "none")
      )

    plot_list <- Filter(Negate(is.null), plot_list)

    cowplot::plot_grid(plotlist = plot_list, ncol = ncol)
  }
}
