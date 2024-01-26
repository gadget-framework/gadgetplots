#' @title Plot model stock composition
#' @description Plots proportions of stocks in the model by length or age
#' @inheritParams plot_annual
#' @inheritParams plot_biomass
#' @param type Character specifying the plot type. Options: "line" or "area". See examples.
#' @param by_age Logical indicating whether age should be used on the x-axis instead of length
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{type} argument.
#' @examples
#' data(fit)
#' plot_stockcomp(fit)
#' plot_stockcomp(fit, type = "area")
#' plot_stockcomp(fit, by_age = TRUE)
#' @export

plot_stockcomp <- function(fit, type = "line", by_age = FALSE, base_size = 8) {

  if(!by_age) {
    x <- fit$stock.full %>%
      dplyr::group_by(.data$year, .data$step, .data$stock, .data$length) %>%
      dplyr::summarise(n = sum(.data$number, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$year, .data$step, .data$length) %>%
      dplyr::mutate(p = .data$n/sum(.data$n),
                    xval = .data$length)
  } else {
    x <- fit$stock.std %>%
      dplyr::group_by(.data$year, .data$step, .data$stock, .data$age) %>%
      dplyr::summarise(n = sum(.data$number, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(.data$year, .data$step, .data$age) %>%
      dplyr::mutate(p = .data$n/sum(.data$n),
                    xval = .data$age)
  }

  {
    if(type == "area") {
      ggplot2::ggplot(data = x, ggplot2::aes(x = .data$xval, y = .data$p)) +
        ggplot2::geom_area(ggplot2::aes(fill = .data$stock))
    } else {
      ggplot2::ggplot(data = x, ggplot2::aes(x = .data$xval, y = .data$p)) +
        ggplot2::geom_line(ggplot2::aes(color = .data$stock),
                           linewidth = base_size/16)
    }
  } +
    ggplot2::labs(
      y = 'Stock proportion', color = "Stock", fill = "Stock",
      x = ifelse(by_age, "Age", "Length")
    ) +
    ggplot2::facet_wrap(~.data$year+.data$step,
                        labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom",
                   strip.background = ggplot2::element_blank())
}
