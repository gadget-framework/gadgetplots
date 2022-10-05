#' @title Plot stock biomasses
#' @inheritParams plot_annual
#' @param total Logical indicating whether total biomass should be plotted. Has no effect if \code{geom_area = TRUE}.
#' @param geom_area Logical indicating whether stacked area should be plotted instead of lines.
#' @param biomass Logical indicating whether biomass should be plotted instead of estimated abundance.
#' @param panelrow something here
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_biomass <- function(fit, total = FALSE, geom_area = FALSE, biomass = TRUE, panelrow = FALSE, base_size = 8){

  if (length(fit) == 1) fit <- fit[[1]]
  if (inherits(fit, 'gadget.fit')) {

    if(biomass) {
      fit$res.by.year$value <- fit$res.by.year$total.biomass/1e6
    } else {
      fit$res.by.year$value <- fit$res.by.year$total.number/1e6
    }

    if(geom_area) {
      pl <-
        ggplot2::ggplot(fit$res.by.year,
                        ggplot2::aes(.data$year,
                                     .data$value,
                                     fill = .data$stock)) +
        ggplot2::geom_area() +
        ggplot2::labs(
          y = ifelse(biomass, "Biomass ('000 tons)", "Abundance (millions)"),
          x='Year',col='Stock') +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::theme_classic(base_size = base_size)


    } else {

      pl <-
        ggplot2::ggplot(fit$res.by.year,
                        ggplot2::aes(.data$year,
                                     .data$value,
                                     col=.data$stock)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          y = ifelse(biomass, "Biomass ('000 tons)", "Abundance (millions)"),
          x='Year',col='Stock') +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::theme_classic(base_size = base_size)

      if (total){
        pl <-
          pl + ggplot2::geom_line(
            ggplot2::aes(.data$year,
                         .data$total.value),
            data =
              fit$res.by.year %>%
              dplyr::group_by(.data$year) %>%
              dplyr::summarise(total.value = sum(.data$value)) %>%
              dplyr::mutate(stock = 'Total')

          )
      }
    }
  } else{

    ## Extract components from each fit and bind together
    dat <- bind_fit_components(fit, component = 'res.by.year')

    ## Plot
    pl <-
      ggplot2::ggplot(dat,
                      ggplot2::aes(.data$year,
                                   .data$total.biomass/1e6,
                                   col=.data$id)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~stock, ncol = if (panelrow) 1 else NULL) +
      ggplot2::labs(y="Biomass (in '000 tonnes)", x='Year',col='Model')

    if (total){

      pl <-
        pl + ggplot2::geom_line(
          ggplot2::aes(.data$year, .data$total.biomass/1e6),
          data =
            dat %>%
            dplyr::group_by(.data$id, .data$year) %>%
            dplyr::summarise(total.biomass = sum(.data$total.biomass)) %>%
            dplyr::mutate(stock = 'Total'))

    }
  }
  return(pl)
}

