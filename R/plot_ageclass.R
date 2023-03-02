#' @title Plot of age class from the model
#' @title Plots age class from the model at the beginning of the year
#' @inheritParams plot_annual
#' @inheritParams plot_biomass
#' @param age Integer giving the age class to plot.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_ageclass <- function(fit, age = 2, biomass = FALSE, base_size = 8) {

  dat <- fit$stock.std %>%
    dplyr::filter(.data$age == !!age,
                  .data$step == 1) %>%
    dplyr::group_by(.data$year, .data$stock) %>%
    dplyr::summarise(
      total.number = sum(.data$number, na.rm = TRUE),
      total.biomass = sum(.data$number*.data$mean_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>% dplyr::ungroup()

if(biomass) {
  ylab <- paste("Biomass of age class", age, "(kt)")
  dat$value <- dat$total.biomass/1e6
} else {
  ylab <- paste("Abundance of age class", age, "(millions)")
  dat$value <- dat$total.number/1e6
}

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$year,
                 y = .data$value,
                 fill = .data$stock)) +
    ggplot2::geom_col() +
    ggplot2::labs(y = ylab, x = 'Year', fill = 'Stock') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::theme_classic(base_size = base_size)
}
