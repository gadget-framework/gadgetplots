#' @title Plot of survey indices
#' @inheritParams plot_annual
#' @param type Character specifying the plot type: \code{"model_fit"} plots the model fit to survey index data by year, \code{"regression"} plots regression lines together with logarithms of model biomass/abundance and observed index values, and \code{"scatter"} produces a scatter plot for the predicted and observed index using the same values than in \code{"model_fit"}. See details.
#' @details Relationship between a survey index (I) and model biomass or abundance (B) over a defined length range is expressed as \eqn{I = e^a B^b}, where \eqn{e^a} is the catchability (also called q) and b the stock size dependent catchability (see \code{type = "model_fit"}). Whether I is compared to model biomass or abundance, depends on the units of I (i.e. column name of the data object; "number" -> abundance, "weight" -> biomass). The survey index I is called "observed" in the figure, and "predicted" is derived as \eqn{exp(a + b \; log(B))}. The a and b can be visualized using a log-linear formula \eqn{log(I) = a + b \;  log(B)} (see \code{type = "regression"}; log-linear regressions shown using blue lines). The coefficient of determination \eqn{R^2} is calculated using this regression. To compare the observed and predicted values in the model fit plot, use \code{type = "scatter"}. The grey line represents a diagonal line with a slope of 1 and intercept of 0 (i.e. \eqn{I = B}, \eqn{a = 0}, \eqn{b = 1}).
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_si <- function(fit, type = "model_fit", base_size = 8) {
  
  ## Gadget2 compatability
  if (!('type' %in% names(fit$sidat)) & 'fittype' %in% names(fit$sidat)){
    fit$sidat <- fit$sidat %>% dplyr::rename(type = .data$fittype)
  }
  if(length(unique(fit$sidat$type)) > 1 | !grepl('log', unique(fit$sidat$type))) {
    warning("Regression parameters have been calculated assuming log space. That does not seem to be the case here. Note that the estimates will be wrong.")
  }

  x <- fit$sidat %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(
      ssr = sum((log(.data$number) - mean(log(.data$observed)))^2),
      sse = sum((log(.data$observed) - log(.data$number))^2),
      sst = sum((log(.data$observed) - mean(log(.data$observed)))^2),
      r2 = stats::cor(log(.data$observed), log(.data$number))^2 # .data$ssr/(.data$ssr+.data$sse)
    ) %>%
    dplyr::mutate(name = gsub("surveyindices\\.", "", .data$name))

  tmp <- fit$params %>%
    dplyr::filter(grepl("weight$", .data$switch),
                  grepl(paste(x$name %>% unique(), collapse = "|"), .data$switch))

  if(length(tmp) > 0) {
    if(any(!is.na(tmp$value))) {
      tmp <- tmp %>%
        dplyr::mutate(switch = gsub("_weight$|adist_surveyindices|cdist_surveyindices|_log_", "", .data$switch)) %>%
        dplyr::select(.data$switch, .data$value) %>%
        dplyr::rename("name" = "switch") %>%
        dplyr::mutate(value = unname(unlist(.data$value)),
                      weight = round(100*.data$value/max(.data$value), 1))

      x <- dplyr::left_join(x, tmp, by = "name")
    } else {
      x$weight <- rep(NA, nrow(x))
    }
  } else {
    x$weight <- rep(NA, nrow(x))
  }

  x <- x %>%
    dplyr::mutate(
      name = paste0(
        gsub("surveyindices\\.", "", .data$name), " (len:", .data$length,
        ", a=", round(.data$intercept, 1), ", b=", round(.data$slope, 3),
        ", R2=", round(.data$r2, 2), ", w=", .data$weight, "% of SI max)")
    )

  ## Convert year to year+step if multiple steps exist
  steps <- unique(x$step)
  if (length(steps) > 1) {
    x <- x %>%
      dplyr::mutate(year = .data$year + (.data$step-1)/length(steps))
  }

  if(type == "scatter") {

    ggplot2::ggplot(x, ggplot2::aes(.data$predicted,.data$observed,label=.data$year)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey") +
      ggplot2::geom_text(size = 0.8*base_size/2.845276) +
      ggplot2::facet_wrap(~.data$name,scales = 'free') +
      ggplot2::labs(x='Predicted (exp(a + b log(B)))', y='Observed index (I)') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else if(type == "regression") {

    ggplot2::ggplot(x, ggplot2::aes(log(.data$number),log(.data$observed),label=.data$year)) +
      ggplot2::geom_text(size = 0.8*base_size/2.845276) +
      ggplot2::facet_wrap(~.data$name,scales = 'free') +
      ggplot2::geom_abline(ggplot2::aes(slope = .data$slope, intercept = .data$intercept),
                           color = "blue") +
      ggplot2::labs(x='Model biomass/abundance (log(B))', y='Observed index (log(I))') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else {

    ggplot2::ggplot(x, ggplot2::aes(.data$year,.data$observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(.data$year,.data$predicted)) +
      ggplot2::geom_linerange(
        data = x %>% dplyr::filter(.data$year==max(.data$year)),
        ggplot2::aes(x= .data$year, ymax=.data$observed, ymin=.data$predicted),
        col='green') +
      ggplot2::facet_wrap(~.data$name,scales = 'free_y',ncol=2) +
      ggplot2::labs(y='Index', x='Year') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }
}
