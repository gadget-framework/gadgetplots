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
#'   \item{biomass}{Stock biomass. Uses \code{\link{plot_biomass}}}
#'   \item{total}{Stock biomass with total. Uses \code{\link{plot_biomass}}(..., total = TRUE)}
#'   \item{abundance}{Stock abundance. Uses \code{\link{plot_biomass}}(..., biomass = FALSE)}
#'   \item{ssb}{Spawning stock biomass. Uses \code{\link{plot_ssb}}}
#'   \item{rec}{Recruitment. Uses \code{\link{plot_rec}}}
#'   \item{f}{Fishing mortality. Uses \code{\link{plot_f}}}
#'   \item{catch}{Catches by stock. Uses \code{\link{plot_catch}}(..., type = "stock")}
#'   \item{catch}{Catches by fleet. Uses \code{\link{plot_catch}}(..., type = "fleet")}
#'   \item{hr}{Harvest rate. Uses \code{\link{plot_catch}}(..., type = "hr")}
#'   \item{si}{Survey indices. Uses \code{\link{plot_si}}}
#'   \item{catchdist}{Catch distribution. Uses \code{\link{plot_catchdist}}}
#'   \item{stockdist}{Stock distribution. Uses \code{\link{plot_stockdist}}}
#'   \item{suitablity}{Suitability (fleet selection). Uses \code{\link{plot_suitability}}}
#'   \item{growth}{Average length by age. Uses \code{\link{plot_growth}}}
#'   \item{agelength}{Age-length (growth parameter) fit. Uses \code{\link{plot_agelength}}}
#'   \item{agecomp}{Age composition. Uses \code{\link{plot_agecomp}}}
#'   \item{resid}{Residuals. Uses \code{\link{plot_resid}}}
#'   \item{likelihood}{Likelihood scores. Uses \code{\link{plot_likelihood}}}
#'   \item{weighted}{Weighted likelihood scores. Uses \code{\link{plot_likelihood}}(..., type = "weighted")}
#'   \item{pie}{Proportion of summed weighted likelihood scores. Uses \code{\link{plot_likelihood}}(..., type = "pie")}
#'   \item{params}{Parameter values relative to their boundaries (not implemented yet)}
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
