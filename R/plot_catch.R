#' @title Plot catches
#' @inheritParams plot_annual
#' @param type Character specifying the data type: \code{"stock"} plots the catches by stock, \code{"fleet"} by fleet, \code{"total"} catches without separating to stock or fleet, and \code{"hr"} harvest rates by fleet.
#' @param biomass Logical indicating whether biomass should be plotted instead of estimated abundance.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_catch <- function(fit, type = "stock", biomass = TRUE, base_size = 8) {

  if(type == "fleet") {

    ggplot2::ggplot(
      fit$fleet.info %>%
        dplyr::filter(.data$amount > 1) %>%
        droplevels() %>%
        dplyr::mutate(
          date = zoo::as.yearqtr(paste(.data$year, .data$step, sep = "Q"))),
      ggplot2::aes(.data$date, .data$amount/1e6, fill = .data$fleet)) +
      ggplot2::geom_col() +
      ggplot2::labs(y="Catch (kt)",x='Year',fill='Fleet') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  } else if (type == "hr") {

    ggplot2::ggplot(
      fit$fleet.info %>%
        dplyr::filter(.data$amount > 1, !is.infinite(.data$harv.rate)) %>%
        droplevels() %>%
        dplyr::mutate(
          date = zoo::as.yearqtr(paste(.data$year, .data$step, sep = "Q"))),
      ggplot2::aes(.data$date, .data$harv.rate, fill = .data$fleet)) +
      ggplot2::geom_col()  +
      ggplot2::labs(y="Harvest rate (catch/estimated biomass)",x='Year',fill='Fleet') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  } else {

    if(biomass) {
      dat <- fit$res.by.year %>% dplyr::mutate(value = .data$catch/1e6)
      } else {
      dat <- fit$res.by.year %>% dplyr::mutate(value = .data$num.catch/1e6)
    }

    if(type == "total") {
      ggplot2::ggplot(
        dat %>%
          dplyr::group_by(.data$year) %>%
          dplyr::summarise(value = sum(.data$value, na.rm = TRUE)),
        ggplot2::aes(.data$year, .data$value)) +
        ggplot2::geom_col(fill = "grey", color = "black", linewidth = LS(0.5)) +
        ggplot2::labs(
          y = ifelse(biomass,
                     "Catch (weight in '000 tons)",
                     "Catch (abundance in millions)"),
          x='Year',fill='Stock') +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::theme_classic(base_size = base_size)
    } else {
      ggplot2::ggplot(
        dat,
        ggplot2::aes(.data$year, .data$value, fill = .data$stock)) +
        ggplot2::geom_col() +
        ggplot2::labs(
          y = ifelse(biomass,
                     "Catch (weight in '000 tons)",
                     "Catch (abundance in millions)"),
          x='Year',fill='Stock') +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::theme_classic(base_size = base_size)
    }
  }
}




