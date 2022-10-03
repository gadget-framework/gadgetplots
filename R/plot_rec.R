#' @title Plot recruitment
#' @inheritParams plot_annual
#' @param panelrow something here
#' @param stocks Character specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

# Debugging arguments:
# panelrow = FALSE; stock = NULL; base_size = 8
plot_rec <- function(fit, panelrow = FALSE, stocks = NULL, base_size = 8){

  if (length(fit) == 1) fit <- fit[[1]]
  if (inherits(fit, 'gadget.fit')){

    if (is.null(stocks)) stocks <- unique(fit$res.by.year$stock)

    pl <-
      ggplot2::ggplot(
        fit$res.by.year %>%
          dplyr::filter(.data$stock %in% stocks) %>%
          dplyr::select(.data$year, .data$stock, .data$recruitment) %>%
          tidyr::drop_na(),
        ggplot2::aes(x = .data$year,
                     y = .data$recruitment/1e6,
                     fill = .data$stock)) +
      ggplot2::geom_col() +
      ggplot2::labs(y = 'Recruitment (in millions)',
                    x = 'Year',
                    fill = 'Stock') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  }

  else{

    ## Extract components from each fit and bind together
    dat <- bind_fit_components(fit, component = 'res.by.year')

    if (is.null(stocks)) stocks <- unique(dat$stock)
    pdat <-
      dat %>%
      dplyr::filter(.data$stock %in% stocks) %>%
      tidyr::drop_na()

    ## Plot
    pl <-
      ggplot2::ggplot(pdat %>% dplyr::filter(.data$stock %in% stocks),
                      ggplot2::aes(.data$year,
                                   .data$recruitment/1e6,
                                   col=.data$id)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~.data$stock, ncol = if (panelrow) 1 else NULL) +
      ggplot2::labs(y='Recruitment (in millions)', x='Year', col='Model')

  }

  return(pl)
}
