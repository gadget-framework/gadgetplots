#' @title Plot recruitment
#' @inheritParams plot_annual
#' @param panelrow something here
#' @param stock Character specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted.
#' @param total Logical indicating whether total F should be plotted.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_f <- function(fit, stock = NULL, total = TRUE, panelrow = FALSE, base_size = 8){

  if (length(fit) == 1) fit <- fit[[1]]
  if (inherits(fit, 'gadget.fit')){

    pl <-
      ggplot2::ggplot(fit$res.by.year,
                      ggplot2::aes(.data$year,
                                   .data$`F`,
                                   color=.data$stock)) +
      ggplot2::geom_line() +
      ggplot2::labs(y='F', x='Year', color='Stock') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

    if (total){
      pl <-
        pl + ggplot2::geom_line(
          ggplot2::aes(.data$year,
                       .data$`F`),
          data =
            fit$res.by.year %>%
            dplyr::group_by(.data$year, .data$step) %>%
            dplyr::summarise(
              total.biomass = sum(.data$total.biomass),
              total.catch = sum(.data$catch)) %>%
            dplyr::mutate(harv.rate = .data$total.catch/.data$total.biomass,
                          `F` = -log(1 - .data$harv.rate)) %>%
            dplyr::mutate(stock = 'Total')
        )
    }

  }
  else{

    ## Extract components from each fit and bind together
    dat <- bind_fit_components(fit, component = 'res.by.year')

    ## Plot
    pl <-
      ggplot2::ggplot(dat,
                      ggplot2::aes(.data$year,
                                   .data$F,
                                   col=.data$id)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~stock, ncol = if (panelrow) 1 else NULL) +
      ggplot2::labs(y='F', x='Year', col='Model')

  }

  return(pl)
}
