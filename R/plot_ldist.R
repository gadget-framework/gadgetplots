#' @title Plot model length distributions for stocks by year
#' @inheritParams plot_annual
#' @inheritParams plot_agecomp
#' @param type Character specifying the plot type. Options: \code{"line"}, \code{"bar"}, or \code{"ggridges"}.
#' @param by_age Logical indicating whether the length distributions should be grouped by age. Works only with \code{type = "line"}.
#' @param ncol Number of columns passed to  \code{\link[ggplot2]{facet_wrap}}
#' @details Do not trust the absolute numbers when \code{by_age = TRUE}. They are estimated from normal distributions by using \code{number * dnorm(1:120, mean = mean_length, sd = stddev_length)}.
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{type} argument.
#' @export

plot_ldist <- function(fit, type = "line", by_age = FALSE, scales = "fixed", ncol = NULL, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(by_age & type != "line") {
    message("by_age only works with type = 'line'. Changing to that.")
  }

  if(by_age) {
    nasse <- function(length, number, mean, sd) number * stats::dnorm(length, mean = mean, sd = sd)

    x <- fit$stock.std %>%
      dplyr::filter(!is.na(.data$mean_length)) %>%
      split(list(.$year, .$step, .$area, .$stock, .$age), drop = TRUE)

    x <- lapply(x, function(k) {
      tidyr::expand(k, !!!k, length = 1:120) %>%
        dplyr::mutate(y = nasse(.data$length, .data$number, .data$mean_length, .data$stddev_length)) %>%
        dplyr::filter(.data$y > 1)
    }) %>% dplyr::bind_rows()

    ggplot2::ggplot(
      x,
      ggplot2::aes(x = .data$length, y = .data$y/1e6, group = .data$age, color = .data$stock)
    ) +
      ggplot2::geom_path(alpha = 0.7) +
      ggplot2::facet_wrap(
        ~.data$year+.data$step, drop = FALSE,
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
        scales = scales, ncol = ncol) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(x = "Length", y = 'Abundance (in millions)', color = "Stock") +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  } else if(type == "line") {
    ggplot2::ggplot(
      data = fit$stock.full,
      ggplot2::aes(.data$length,.data$number/1e6, color = .data$stock)
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(
        ~.data$year+.data$step, drop = FALSE,
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
        scales = scales, ncol = ncol)  +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(x = "Length", y = 'Abundance (in millions)', color = "Stock") +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())

  } else if(type == "bar") {
    ggplot2::ggplot(
      data = fit$stock.full,
      ggplot2::aes(.data$length,.data$number/1e6, fill = .data$stock,
                   color = .data$stock)
    ) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(
        ~.data$year+.data$step, drop = FALSE,
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
        scales = scales, ncol = ncol)  +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(x = "Length", y = 'Abundance (in millions)', color = "Stock",
                    fill = "Stock") +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  } else {
    ggplot2::ggplot(
      fit$stock.full %>%
        dplyr::group_by(.data$year, .data$step) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)),
      ggplot2::aes(
        x = .data$length, y = .data$year, height = 100*.data$p,
        fill = .data$stock, group = interaction(.data$year, .data$stock))
    ) +
      ggridges::geom_ridgeline(alpha = 0.5, size = 0.1/2.13) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
      ggplot2::expand_limits(x = 0) +
      ggplot2::labs(x = "Length", y = "Year", fill = "Stock") +
      ggplot2::theme_bw(base_size = base_size)
  }
}
