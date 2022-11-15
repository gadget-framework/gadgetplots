#' @title Save diagnostic graphs to a single html document
#' @inheritParams plot_annual
#' @inheritParams gadget_plots
#' @param file_name Character specifying the name of the html file without path. Must include the file extension.
#' @return Returns nothing, but makes the requested file.
#' @importFrom DT datatable formatRound
#' @importFrom plotly ggplotly layout subplot
#' @import flexdashboard
#' @export

make_html <- function(fit, path, file_name = 'model_output_figures.html') {
  rmarkdown::render(
    input = system.file("html-figures.Rmd", package="gadgetplots"),
    output_dir = path,
    output_file = file_name,
    params = list(fit = fit)
    )
}
