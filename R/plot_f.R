#' @title Plot harvest rate
#' @inheritParams plot_annual
#' @param stocks Character specifying the substock to plot in \code{fit}. If \code{NULL}, all stocks are plotted. Not applicable if \code{min_catch_length} is defined.
#' @param min_catch_length Numeric value defining the minimum catch length (size), which will be used to filter (\code{>=}) the model population before calculating harvest rates using catches. Combines all stocks. Turn of by setting to \code{NULL} (default). Set to 0 to get HR for the entire model population. See Details.
#' @param biomass Logical indicating whether biomass should be used to calculate harvest rates instead of abundance.
#' @param model_selectivity Logical indicating whether to use model selectivity when calculating harvest rates. \code{FALSE} will calculate harvest rate using the entire population, \code{TRUE} using the harvestable biomass following fleet suitabilities. Only applicable when using the \code{stocks} parameter or when \code{min_catch_length} = 0.
#' @param return_data Logical indicating whether to return data for the plot instead of the plot itself.
#' @details The function uses all catches without filtering by size (but allocates them to substocks) and varies denominator depending on the \code{min_catch_length} argument. If \code{min_catch_length} = NULL, the output represents harvest rate for each substock assuming model fleet selectivities (fetched from the \code{harv.biomass} column in \code{fit$res.by.year}). If \code{min_catch_length} = 0, the output represents harvest rate for the entire model population assuming model fleet selectivities (fetched from the \code{harv.biomass} column in \code{fit$res.by.year}, stocks are summed before calculating the harvest rate). Finally, if \code{min_catch_length} is any number > 0, the output represents harvest rate \emph{assuming} flat selectivity for lengths >= \code{min_catch_length}. Note that the last \strong{IS NOT real model harvest rate}. It should only be used for reporting results, never for finding target harvest rates in projections, for instance. The \code{min_catch_length} = 0 is for the latter case.
#' @return A \link[ggplot2]{ggplot} object.
#' @seealso \link{plot_f}
#' @examples
#' data(fit)
#' plot_hr(fit)
#' plot_hr(fit, model_selectivity = FALSE)
#' plot_hr(fit, min_catch_length = 45)
#' @export

plot_hr <- function(fit, stocks = NULL, min_catch_length = NULL, biomass = TRUE, model_selectivity = TRUE, base_size = 8, return_data = FALSE) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(!is.null(min_catch_length)) {

    if(min_catch_length == 0) {

        dt <- fit$res.by.year %>%
          dplyr::group_by(.data$year, .data$step, .data$area) %>%
          dplyr::summarise(catch = sum(.data$catch, na.rm = TRUE),
                           harv.biomass = sum(.data$harv.biomass, na.rm = TRUE),
                           total.biomass = sum(.data$total.biomass, na.rm = TRUE)) %>%
          dplyr::mutate(value = if(model_selectivity) {
            .data$catch/.data$harv.biomass} else {.data$catch/.data$total.biomass}) %>%
          dplyr::rename(catch_biom = .data$catch) %>%
          dplyr::ungroup() %>%
          tidyr::replace_na(list(value = 0))

    } else {

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
                                     .data$catch_num/.data$abundance)) %>%
        dplyr::ungroup() %>%
        tidyr::replace_na(list(value = 0))
    }
  } else {
    if(is.null(stocks)) stocks <- unique(fit$res.by.year$stock)

    dt <- fit$res.by.year %>%
      dplyr::filter(.data$stock %in% stocks) %>%
      dplyr::mutate(value = if(model_selectivity) {
        .data$catch/.data$harv.biomass} else {.data$catch/.data$total.biomass}) %>%
      dplyr::rename(catch_biom = .data$catch, catch_num = .data$num.catch) %>%
      tidyr::replace_na(list(value = 0))
  }

  if(return_data) return(dt)

  ggplot2::ggplot(dt) + {
    if(!is.null(min_catch_length)) ggplot2::aes(.data$year, .data$value)
  } + {
    if(is.null(min_catch_length)) ggplot2::aes(.data$year, .data$value,
                                               color=.data$stock)
  } +
    ggplot2::geom_line() +
    ggplot2::labs(
      y = ifelse(is.null(min_catch_length), "Harvest rate",
                 paste("Harvest rate for >= ", min_catch_length, " length units")),
      x = 'Year', color = 'Stock') +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::theme_classic(base_size = base_size)
}

#' @title Plot fishing mortality
#' @inheritParams plot_annual
#' @inheritParams plot_hr
#' @param stocks Character specifying the substock to plot in \code{fit}. If \code{NULL}, all stocks are plotted. Not applicable if \code{fbar_ages} is defined.
#' @param fbar_ages Either \code{NULL} or a numeric vector of ages to include to calculate Fbar (averaged F over age ranges) for selected ages instead of F for each stock.
#' @details The function calculates either average fishing mortality per substock or average fishing mortality over an age range depending on the \code{fbar_ages} argument.
#' @return A \link[ggplot2]{ggplot} object.
#' @seealso \link{plot_hr}
#' @examples
#' data(fit)
#' plot_f(fit)
#' plot_f(fit, fbar_ages = 5:10)
#' @export

plot_f <- function(fit, stocks = NULL, fbar_ages = NULL, return_data = FALSE, base_size = 8) {

  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(is.null(fbar_ages)) {
    if(is.null(stocks)) stocks <- unique(fit$res.by.year$stock)
    dt <- fit$res.by.year %>% dplyr::filter(.data$stock %in% stocks)

    if(return_data) return(dt)

    ggplot2::ggplot(
      dt,
      ggplot2::aes(.data$year,
                   .data$`F`,
                   color=.data$stock)) +
      ggplot2::geom_line() +
      ggplot2::labs(y = "F", x = 'Year', color = 'Stock') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  } else {
    dt <- fit$stock.prey %>%
      dplyr::filter(.data$age %in% fbar_ages) %>%
      dplyr::group_by(.data$year, .data$age) %>%
      dplyr::summarise(
        c = sum(.data$number_consumed),
        n = sum(.data$number[.data$step==1]),
        catch_mass = sum(.data$biomass_consumed)) %>%
      dplyr::mutate(f = -log(1 - .data$c/.data$n)) %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        Fbar = mean(.data$f, na.rm = TRUE),
        catch_n = sum(.data$c, na.rm = TRUE),
        catch_mass = sum(.data$catch_mass, na.rm = TRUE))

    if(return_data) return(dt)

    ggplot2::ggplot(dt, ggplot2::aes(.data$year, .data$Fbar)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        y = paste0("Fbar (", min(fbar_ages), "-", max(fbar_ages), ")"),
        x = 'Year') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }


}
