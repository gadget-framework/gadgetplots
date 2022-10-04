#' @title Plot \code{gadget.fit} object
#' @description A wrapper function to plot the results from a \link[gadget3:g3_fit]{gadget.fit} object.
#' @param x A gadget fit object. See \code{\link[gadgetutilss]{g3_fit}}.
#' @param param Character defining the parameter to plot. See Details.
#' @param ... Additional parameters passed to the separate plotting functions.
#' @details A wrapper to plot the results from a \link[gadget3:g3_fit]{gadget.fit} object. The function produces a different plots defined by \code{param} argument and additional arguments passed to the respective functions.
#'
#' Valid \code{param} options are:
#' \describe{
#'   \item{annual}{Annual summary of central model data. Uses \code{\link{plot_annual}}}
#'   \item{sidat}{Surveyindices}
#'   \item{likelihood}{\link[=plot_likelihood]{Likelihood summary data}}
#'   \item{catchdist}{Catchdistribution data}
#'   \item{stockdist}{Stockdistribution data}
#'   \item{res.by.year}{Results by year}
#'   \item{stock.std}{Age composition from the model}
#'   \item{suitability}{Suitability estimated from the model by year and step}
#'   \item{params}{Parameter values relative to their boundaries}
#' }
#' and valid plottypes are:
#' \describe{
#'   \item{direct}{Default value, plots direct comparisons of data with model output. Valid for all datatype except 'res.by.year'}
#'   \item{weighted}{Only for 'summary'. Plots the weighted likelihood value for each component.}
#'   \item{pie}{Only for 'summary'. Plots the likelihood composition as a pie chart}
#'   \item{lengths}{Only for 'sidat'. Plot the surveyindex based on the SI length group instead of component name.}
#'   \item{bio}{Only for 'sidat'. Plot the biomass weighted survey index, assumes length based abundance index.}
#'   \item{scatter}{Only for 'sidat'. Produces a x-y scatter-plot for the fitted and observed index.}
#'   \item{resid}{Only for 'catchdist.fleets'. Produces a residual plot for each component.}
#'   \item{bubble}{Only for 'catchdist.fleets'. Produces a bubble plot for each component.}
#'   \item{growth}{Only for 'catchdist.fleets'. Produces a plot of fitted growth for each age-length component.}
#'   \item{resid}{Only for 'catchdist.fleets'. Produces a residual plot for each component.}
#'   \item{F}{Only for 'res.by.year'. Produces a F plot by stock.}
#'   \item{total}{Only for 'res.by.year'. Produces a total biomass plot by stock.}
#'   \item{catch}{Only for 'res.by.year'. Produces a total catch plot by stock.}
#'   \item{rec}{Only for 'res.by.year'. Produces a recruitment biomass plot by stock.}
#' }
#' @return Single or a list of \link[ggplot2]{ggplot} objects depending on the arguments.
#' @export

# data = NULL; type = NULL
plot.gadget.fit <- function(x, param = "annual", ...){
  args <- list(...)
  fit <- x
  # data <- if (is.null(args$data)) NULL else args$data
  # type <- if (is.null(args$type)) NULL else args$type

  if(param == "annual") {
    return(plot_annual(fit, ...))
  }

  if(param == "growth") {
    return(plot_growth(fit, ...))
  }

  if(param %in% c("agelength", "age")) {
    return(plot_agelength(fit, ...))
  }

  if(param == "resid") {
    return(plot_resid(fit, ...))
  }

  if(param %in% c("f", "F")) {
    return(plot_f(fit, ...))
  }

  if(param %in% c("rec", "recruitment")) {
    return(plot_rec(fit, ...))
  }

  if(param %in% c("ssb", "spawning", "spawning_stock")) {
    return(plot_ssb(fit, ...))
  }

  if(param %in% c("biomass", "total", "abundance")) {
    if(param == "total") {
      return(plot_biomass(fit, total = TRUE, ...))
    } else if(param == "abundance") {
      return(plot_biomass(fit, biomass = FALSE, ...))
    } else {
      return(plot_biomass(fit, ...))
    }
  }

  if(param %in% c("catch", "catches", "hr", "fleet")) {
    if(param == "hr") {
      type <- "hr"
    } else if (param == "fleet") {
      type <- "fleet"
    } else if(!exists("type")) {
      type <- "stock"
    } else if (!type %in% c("stock", "fleet", "hr")) {
      type <- "stock"
    }
    return(plot_catch(fit, type = type, ...))
  }

  if(param %in% c('likelihood', 'weighted', 'pie')) {
    if(param == "pie") {
      type <- "pie"
    } else if (param == "weighted") {
      type <- "weighted"
    } else if(!exists("type")) {
      type <- "direct"
    } else if (!type %in% c("direct", "weighted", "pie")) {
      type <- "direct"
    }
    return(plot_likelihood(fit, type = type, ...))
  }

  if(param %in% c('si', 'survey', 'surveyindex', 'surveyindices')) {
    return(plot_si(fit, ...))
  }

  if(param == "catchdist") {
    return(plot_catchdist(fit, ...))
  }

  if(param %in% c("suitability", "suit")) {
    return(plot_suitability(fit, ...))
  }

  if(param %in% c("agecomp", "composition", "stock.std")) {
    return(plot_agecomp(fit, ...))
  }

  if(param %in% c("stockdist", "maturity", "matp", "mat")) {
    return(plot_stockdist(fit, ...))
  }

  if(param %in% c("params", "parameter")) {
    warning("Parameter bounds not exported in g3_fit yet. Contact the developers.")
    return(NULL)
  }

  warning("param not found from the predefined list. Returning NULL")
  return(NULL)

}
