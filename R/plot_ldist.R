#' @title Plot model length distributions for stocks by year
#' @inheritParams plot_annual
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{type} argument.
#' @export

plot_ldist <- function(fit, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

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

}
