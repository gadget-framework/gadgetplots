#' @title Plot stock-recruitment relationship
#' @description Plots spawning stock biomass (SSB) against recruitment with a lag
#' @inheritParams plot_annual
#' @param spawning_stock Either character defining the name of the stock that should be understood as spawning stock, or numeric giving the minimum length of spawning fish. If \code{NULL}, all stocks are used.
#' @param lag Integer giving the number of time steps that should be used as a lag between SSB and recruitment.
#' @param years A vector of recruitment years to include. If \code{NULL}, all years will be used.
#' @param add_line Add a line connecting years
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{stocks} argument.
#' @export

plot_sr <- function(fit, spawning_stock = NULL, lag = 1, years = NULL, add_line = TRUE, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  ## SSB

  if(is.null(spawning_stock)) {
    dat <- fit$res.by.year %>%
      dplyr::group_by(.data$year, .data$step) %>%
      dplyr::summarise(biomass = sum(.data$total.biomass, na.rm = TRUE),
                       .groups = "drop")
  } else if (inherits(spawning_stock, "character")) {
    dat <- fit$res.by.year %>%
      dplyr::filter(.data$stock == spawning_stock) %>%
      dplyr::group_by(.data$year, .data$step) %>%
      dplyr::summarise(biomass = sum(.data$total.biomass, na.rm = TRUE),
                       .groups = "drop")
  } else if (inherits(spawning_stock, "numeric")) {
    dat <- fit$stock.full %>%
      dplyr::filter(.data$length >= spawning_stock) %>%
      dplyr::mutate(biomass = .data$number * .data$mean_weight) %>%
      dplyr::group_by(.data$year, .data$step) %>%
      dplyr::summarise(
        biomass = sum(.data$biomass, na.rm = TRUE),
        .groups = "drop"
      )
  } else stop("Don't know what to do.")

  if (unique(dat$step) == 1) {
    dat$year <- dat$year - 1
  } else stop("Other time steps than 1 have not been implemented yet.")

  ## Recruitment

  tmp <- fit$res.by.year %>%
    dplyr::group_by(.data$year, .data$step) %>%
    dplyr::summarise(recruits = sum(.data$recruitment, na.rm = TRUE),
                     .groups = "drop")

  tmp <- tmp[-1,]

  if(!is.null(years)) {
    tmp <- tmp %>% dplyr::group_by(.data$year %in% years)
  }

  ## Join

  dat <- dplyr::inner_join(tmp, dat)

  ## Plot

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$biomass/1e6,
                 y = .data$recruits/1e6,
                 label = .data$year)) + {
                   if(add_line) ggplot2::geom_path(color = "grey")
                 } +
    ggplot2::geom_text(size = base_size/2.845276) +
    # ggplot2::geom_point(shape = ".") +
    ggplot2::labs(
      y = 'Recruits (millions)',
      x= 'Spawning stock biomass (kt)') +
    ggplot2::theme_classic(base_size = base_size)
}
