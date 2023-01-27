#' @title Plot of average growth for each stock
#' @inheritParams plot_annual
#' @param type Character specifying the plot type. Options: \code{"annual"}, \code{"mean"} or \code{"stdev"}. See Details.
#' @param stdev Logical indicating whether standard deviation should be shown on both sides of mean for plots that use mean lengths
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_growth <- function(fit, type = "annual", stdev = FALSE, base_size = 8) {

  dat <- fit$stock.std %>%
    dplyr::filter(.data$number > 0) %>%
    dplyr::arrange(.data$year, .data$step, .data$stock, .data$age)

  if(max(dat$step) > 1) dat$age <- dat$age + dat$step/max(dat$step) - min(dat$step)/max(dat$step)

  if(type != "annual") {
    dat <- dat %>%
      dplyr::group_by(.data$stock, .data$age) %>%
      dplyr::summarise(
        mean = mean(.data$mean_length, na.rm = TRUE),
        mean_sd = mean(.data$stddev_length, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(type = "Mean") %>%
      dplyr::bind_rows(
        dat %>%
          dplyr::filter(.data$year == min(.data$year), .data$step == min(.data$step)) %>%
          dplyr::select(.data$stock, .data$age, .data$mean_length, .data$stddev_length) %>%
          dplyr::rename("mean" = "mean_length", "mean_sd" = "stddev_length") %>%
          dplyr::mutate(type = "First time-step")
      ) %>%
      dplyr::bind_rows(
        dat %>%
          dplyr::filter(.data$year == max(.data$year), .data$step == max(.data$step)) %>%
          dplyr::select(.data$stock, .data$age, .data$mean_length, .data$stddev_length) %>%
          dplyr::rename("mean" = "mean_length", "mean_sd" = "stddev_length") %>%
          dplyr::mutate(type = "Last time-step")
      )
  }


  if(type == "annual") {
    ggplot2::ggplot(dat) + {
      if(stdev) ggplot2::geom_ribbon(
        ggplot2::aes(x = .data$age, ymin = .data$mean_length - .data$stddev_length,
                     ymax = .data$mean_length + .data$stddev_length,
                     fill = .data$stock),
        alpha = 0.5, color = NA
      )
    } +
      ggplot2::geom_line(
        ggplot2::aes(.data$age, .data$mean_length, color=.data$stock),
        linewidth = base_size/16) +
      ggplot2::facet_wrap(~.data$year) +
      ggplot2::expand_limits(x = 0, y = 0) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(n.breaks = 6) +
      ggplot2::labs(y = 'Average length +- st.dev.',x = 'Age',
                    color = 'Stock', fill = 'Stock') +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else if(type == "mean") {

    if(stdev) {
      ggplot2::ggplot(dat) +
        ggplot2::geom_ribbon(
          ggplot2::aes(x = .data$age, ymin = .data$mean - .data$mean_sd,
                       ymax = .data$mean + .data$mean_sd,
                       fill = .data$type),
          alpha = 0.3, color = NA
        ) +
        ggplot2::geom_line(
          ggplot2::aes(.data$age, .data$mean, color=.data$type),
          linewidth = base_size/16) +
        ggplot2::facet_wrap(~.data$stock) +
        ggplot2::expand_limits(x = 0, y = 0) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(n.breaks = 6) +
        ggplot2::labs(y = 'Average length +- st.dev.',x = 'Age',
                      color = 'Stock', fill = 'Stock') +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank())

    } else {

      ggplot2::ggplot(dat) +
      ggplot2::geom_line(
        ggplot2::aes(.data$age, .data$mean, color=.data$stock,
                     linetype = .data$type),
        linewidth = base_size/16) +
        ggplot2::expand_limits(x = 0, y = 0) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(n.breaks = 6) +
        ggplot2::labs(y = 'Average length +- st.dev.',x = 'Age',
                      color = 'Stock', linetype = 'Type') +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank())
    }
  } else {

    ggplot2::ggplot(dat) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data$age, y = .data$mean_sd, color = .data$type)) +
      ggplot2::facet_wrap(~.data$stock) +
      ggplot2::expand_limits(x = 0) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(n.breaks = 6) +
      ggplot2::labs(y = 'Average length standard deviation',x = 'Age',
                    color = 'Type') +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }
}
