#' @title Plot model age distributions for stocks by year
#' @inheritParams plot_annual
#' @inheritParams plot_agecomp
#' @inheritParams plot_ldist
#' @param type Character specifying the plot type. Options: \code{"line"}, \code{"bar"} or \code{"ggridges"}. See Details.
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{type} argument.
#' @export

plot_adist <- function(fit, type = "bar", scales = "fixed", ncol = NULL, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  dat <- fit$stock.std %>%
    dplyr::group_by(.data$year, .data$step, .data$area, .data$stock, .data$age) %>%
    dplyr::summarise(
      number = sum(.data$number, na.rm = TRUE)/1e6, # millions
      weight = sum(.data$number*.data$mean_weight, na.rm = TRUE)/1e6, #kt
      .groups = "drop") %>%
    dplyr::mutate(yc = as.factor(.data$year - .data$age))


  if(type == "line") {
    ggplot2::ggplot(
      dat,
      ggplot2::aes(.data$age, .data$number, color = .data$stock)
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
  } else if (type == "bar") {
    ggplot2::ggplot(
      dat,
      ggplot2::aes(.data$age, .data$number, fill = .data$stock)
    ) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(
        ~.data$year+.data$step, drop = FALSE,
        labeller = ggplot2::label_wrap_gen(multi_line=FALSE),
        scales = scales, ncol = ncol)  +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::labs(x = "Age", y = 'Abundance (in millions)', fill = "Stock") +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  } else {
    ggplot2::ggplot(
      dat %>%
        dplyr::group_by(.data$year, .data$step) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)),
      ggplot2::aes(
        x = .data$age, y = .data$year, height = 10*.data$p,
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
