% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_suitability.R
\name{plot_suitability}
\alias{plot_suitability}
\title{Plot suitability}
\usage{
plot_suitability(
  fit,
  fleets = "all",
  add_models = TRUE,
  include_missing = TRUE,
  base_size = 8
)
}
\arguments{
\item{fit}{A gadget fit object. See \code{\link[gadgetutils]{g3_fit}}.}

\item{fleets}{Character vector specifying the fleets to plot in \code{fit}. If \code{NULL}, all fleets will be plotted in separate plots. Use \code{"all"} to plot all fleets to the same plot.}

\item{add_models}{Logical indicating whether models using suitability parameters should be plotted together with the suitabilities estimated from data. The name of the parameter has to be similar to the fleet name. Uses grep and does not always work.}

\item{include_missing}{Logical indicating whether years with missing catch data should be plotted.}

\item{base_size}{Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.}
}
\value{
A \link[ggplot2]{ggplot} object. If \code{fleet = NULL}, a list of ggplot objects.
}
\description{
Plot suitability
}
\examples{
data(fit)
plot_suitability(fit)
plot_suitability(fit, fleets = "EggaN_survey",
                 include_missing = FALSE)
}
