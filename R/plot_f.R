#' @title Plot harvest rate
#' @inheritParams plot_annual
#' @param stock Character specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted. Not applicable if \code{min_catch_length} is defined.
#' @param min_catch_length Numeric value defining the minimum catch length (size), which will be used to filter (\code{>=}) the model population before calculating harvest rates using catches. Combines all stocks. Turn of by setting to \code{NULL} (default).
#' @param biomass Logical indicating whether biomass should be used to calculate harvest rates instead of abundance.
#' @return A \link[ggplot2]{ggplot} object.
#' @seealso plot_f
#' @export

plot_hr <- function(fit, stock = NULL, min_catch_length = NULL, biomass = TRUE, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(!is.null(min_catch_length)) {
    dt <- fit$stock.full %>%
      dplyr::filter(.data$length >= min_catch_length) %>%
      dplyr::mutate(biomass = .data$number * .data$mean_weight) %>%
      dplyr::group_by(.data$year, .data$step, .data$area) %>%
      dplyr::summarise(
        abundance = sum(.data$number, na.rm = TRUE),
        biomass = sum(.data$biomass, na.rm = TRUE),
        .groups = "drop"
      ) %>% dplyr::ungroup() %>%
      dplyr::left_join(
        fit$stock.prey %>%
          dplyr::group_by(.data$year, .data$step, .data$area) %>%
          dplyr::summarise(
            catch_biom = sum(.data$biomass_consumed),
            catch_num = sum(.data$number_consumed),
            .groups = "drop"),
        by = c("year", "step", "area")
      ) %>%
      dplyr::mutate(value = ifelse(biomass, .data$catch_biom/.data$biomass,
                            .data$catch_num/.data$abundance))

  } else {
    dt <- fit$res.by.year %>%
      dplyr::mutate(value = .data$catch/.data$harv.biomass)
  }

  ggplot2::ggplot(dt) + {
    if(!is.null(min_catch_length)) ggplot2::aes(.data$year, .data$value)
  } + {
    if(is.null(min_catch_length)) ggplot2::aes(.data$year, .data$value,
                                               color=.data$stock)
  } +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = ifelse(is.null(min_catch_length), "Harvest rate",
                 paste("Harvest rate for >= ", min_catch_length)),
      x = 'Year', color = 'Stock') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::theme_classic(base_size = base_size)
}

#' @title Plot fishing mortality
#' @inheritParams plot_annual
#' @param stock Character specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_f <- function(fit, stock = NULL, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  ggplot2::ggplot(
    fit$res.by.year,
    ggplot2::aes(.data$year,
                 .data$`F`,
                 color=.data$stock)) +
    ggplot2::geom_line() +
    ggplot2::labs(y = "F", x = 'Year', color = 'Stock') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::theme_classic(base_size = base_size)

}
