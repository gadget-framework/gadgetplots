#' @title Plot model length distributions for stocks by year
#' @inheritParams plot_annual
#' @param ggridges Logical indicating whether to return a ggridges plot instead of a facetted plot.
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{type} argument.
#' @export

plot_ldist <- function(fit, ggridges = FALSE, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(!ggridges) {
    ggplot2::ggplot(
      data = fit$stock.full,
      ggplot2::aes(.data$length,.data$number, color = .data$stock)
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(
        ~.data$year+.data$step,drop = FALSE,
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE))  +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(x = "Length", y = "Number", color = "Stock") +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  } else {
    ggplot2::ggplot(
      fit$stock.full %>%
        dplyr::group_by(.data$year) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)),
      ggplot2::aes(x = .data$length, y = .data$year, height = 100*.data$p,
                   fill = .data$stock, group = interaction(.data$year, .data$stock))) +
      ggridges::geom_ridgeline(alpha = 0.5, size = 0.5/2.13) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
      ggplot2::labs(x = "Length", y = "Year", fill = "Stock") +
      ggplot2::theme_bw()
  }
}
