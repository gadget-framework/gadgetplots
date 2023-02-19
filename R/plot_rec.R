#' @title Plot recruitment
#' @inheritParams plot_annual
#' @inheritParams plot_hr
#' @param panelrow something here
#' @param stocks Character specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted. If \code{"total"}, total recruitment will be plotted instead.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

# Debugging arguments:
# panelrow = FALSE; stock = NULL; base_size = 8
plot_rec <- function(fit, panelrow = FALSE, stocks = NULL, return_data = FALSE, base_size = 8){

  if (length(fit) == 1) fit <- fit[[1]]
  if (inherits(fit, 'gadget.fit')){

    if (is.null(stocks)) stocks <- unique(fit$res.by.year$stock)

    if(length(stocks) == 1 && "total" %in% stocks) {
        dat <- fit$res.by.year %>%
          dplyr::group_by(.data$year) %>%
          dplyr::summarise(value = sum(.data$recruitment, na.rm = TRUE)/1e6) %>%
          dplyr::mutate(stock = "total", .before = "value")
    } else {
      dat <- fit$res.by.year %>%
        dplyr::filter(.data$stock %in% stocks) %>%
        dplyr::group_by(.data$year, .data$stock) %>%
        dplyr::summarise(value = sum(.data$recruitment, na.rm = TRUE)/1e6)
    }

    if(return_data) return(dat)

    pl <-
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = .data$year,
                     y = .data$value,
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
