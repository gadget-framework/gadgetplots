#' @title Save diagnostic graphs to a single html document
#' @inheritParams plot_annual
#' @inheritParams gadget_plots
#' @param file_name Character specifying the name of the html file without path. Must include the file extension.
#' @param template Character specifying the html template to use. See details.
#' @return Returns nothing, but makes the requested file.
#' @details The package contains html templates tailored for different stocks needed by the authors. Specify the name in the \code{template} argument. Current alternatives are:
#' \describe{
#'   \item{standard}{Standard model output figures. Use the \code{harvest_rate} argument to switch between F and HR figures.}
#'   \item{nea_ghl}{Output tailored for Northeast Arctic Greenland halibut assessment.}
#' }
#' @importFrom DT datatable formatRound
#' @importFrom plotly ggplotly layout subplot
#' @import flexdashboard
#' @export

make_html <- function(fit, path, harvest_rate = TRUE, file_name = 'model_output_figures.html', template = "standard") {
  filename <- paste0(template, ".Rmd")
  rmarkdown::render(
    input = system.file(filename, package="gadgetplots"),
    output_dir = path,
    output_file = file_name,
    params = list(fit = fit, harvest_rate = harvest_rate)
    )
}
