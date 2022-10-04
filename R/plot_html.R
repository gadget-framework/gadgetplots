#' @title Wrapper to save diagnostic graphs to a single html document
#' @inheritParams plot_annual
#' @inheritParams gadget_plots
#' @param file_name Character specifying the name of the html file without path. Must include the file extension.
#' @return Returns nothing, but makes the requested file.
#' @importFrom DT datatable formatRound
#' @importFrom plotly ggplotly layout subplot
#' @import flexdashboard
#' @export

plot_html <- function(fit, path, file_name = 'model_output_figures.html') {
  rmarkdown::render(
    input = "inst/html-figures.Rmd",
    output_file = file.path(path, file_name),
    params = list(fit = fit)
    )
}
