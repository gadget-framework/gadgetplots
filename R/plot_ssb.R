#' @title Plot spawning stock biomass
#' @inheritParams plot_annual
#' @param spawning_stock Character specifying the name of the spawning stock in the \code{fit} object (i.e. the female mature stock). If \code{NULL}, the correct stock is guessed.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

# Debugging arguments:
# panelrow = FALSE; stock = NULL; base_size = 8
plot_ssb <- function(fit, spawning_stock = NULL, base_size = 8){

  # fit$stock.full for reference length
  # fit$stock.std for reference age

  if (length(fit) == 1) fit <- fit[[1]]
  if (!inherits(fit, 'gadget.fit')) stop("Requires a gadget.fit object")

  if (is.null(spawning_stock)) {
    spawning_stock <- grep("fem.*mat", unique(fit$res.by.year$stock), value = TRUE)
  }

  ggplot2::ggplot(fit$res.by.year %>%
                    dplyr::filter(.data$stock %in% spawning_stock),
                  ggplot2::aes(x = .data$year,
                               y = .data$total.biomass/1e6)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(y = "Biomass (in '000 tonnes)",
                  x = 'Year') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_classic(base_size = base_size)

}
