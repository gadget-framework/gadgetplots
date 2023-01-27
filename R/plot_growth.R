#' @title Plot of average growth for each stock
#' @inheritParams plot_annual
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_growth <- function(fit, stdev = TRUE, base_size = 8) {

  fit$stock.std %>%
    dplyr::filter(.data$number > 0) %>%
    dplyr::mutate(age = .data$age + .data$step/max(.data$step)) %>%
    ggplot2::ggplot() + {
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
    ggplot2::labs(y='Average length +- st.dev.',x='Age',color='Stock') +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
}
