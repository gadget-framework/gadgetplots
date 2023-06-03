#' @title Plot central model output from a leave-out analysis
#' @inheritParams plot_annual
#' @param lo_fit A list of \code{\link[gadgetutils]{g3_fit}} objects originating from a leave-out analysis. See details.
#' @param vars Character vector giving the variables to plot. Only the default parameters are currently implemented.
#' @param ncol Number of columns to be used in the plot. Set to \code{NULL} for standard layout.
#' @details Leave-out analysis has currently not been implemented to gadgetutils. Ask Mikko for the code.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_leaveout <- function(lo_fit, vars = c("nll.summary", "total.biomass", "hr", "recruitment"), base_size = 8, ncol = NULL) {

  if("nll.summary" %in% vars) {
    nll_dat <- bind_fit_components(lo_fit, 'score')

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
        color = 'Left-out\ncomponent',
        title = paste0(
          "sd = ", round(stats::sd(nll_dat$nll, na.rm = TRUE), 0), "\n",
          "CV = ", round(100*stats::sd(nll_dat$nll, na.rm = TRUE)/mean(nll_dat$nll, na.rm = TRUE), 1), " %", "\n",
          "n = ", sum(!is.na(nll_dat$nll)), "/", length(nll_dat$nll))
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
    hr_dat <- lapply(seq_along(lo_fit), function(i) {
      plot_hr(lo_fit[[i]], min_catch_length = 0, return_data = TRUE) %>%
        dplyr::select(.data$year, .data$step, .data$value) %>%
        dplyr::mutate(id = names(lo_fit)[i], .before = 1)
    }) %>%
      dplyr::bind_rows()

    hr_plot <- ggplot2::ggplot(
      hr_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        y = "Total harvest rate",
        x = 'Year', color = 'Left-out\ncomponent') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }

  if("total.biomass" %in% vars) {
    biom_dat <- lapply(seq_along(lo_fit), function(i) {
      plot_biomass(lo_fit[[i]], min_catch_length = 0, return_data = TRUE) %>%
        dplyr::select(.data$year, .data$step, .data$value) %>%
        dplyr::mutate(id = names(lo_fit)[i], .before = 1)
    }) %>%
      dplyr::bind_rows()

    biom_plot <- ggplot2::ggplot(
      biom_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
      ggplot2::geom_path() +
      ggplot2::labs(
        y = "Total biomass (kt)",
        x='Year',col = 'Left-out\ncomponent') +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }

  if("recruitment" %in% vars) {
    rec_dat <- lapply(seq_along(lo_fit), function(i) {
      plot_rec(lo_fit[[i]], stocks = "total", return_data = TRUE) %>%
        dplyr::select(.data$year, .data$value) %>%
        dplyr::mutate(id = names(lo_fit)[i], .before = 1)
    }) %>%
      dplyr::bind_rows()

    rec_plot <- ggplot2::ggplot(
      rec_dat, ggplot2::aes(.data$year, .data$value, color = .data$id)) +
      ggplot2::geom_path() +
      ggplot2::labs(
        y = "Recruitment (millions)",
        x='Year',col = 'Left-out\ncomponent') +
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
