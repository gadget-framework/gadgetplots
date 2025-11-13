#' @title Save diagnostic graphs to a single html document
#' @inheritParams plot_annual
#' @inheritParams gadget_plots
#' @param path Directory path for saving the html output. Defaults to working directory. 
#' @param file_name Character specifying the name of the html file without path. Must include the file extension.
#' @param template Character specifying the html template to use. See details.
#' @param model_name Character specifying the name of the model when "standard \code{template} is used. 
#' @return Returns nothing, but makes the requested file. See the \href{https://gadget-framework.github.io/gadgetplots/articles/make_html_output.html}{webpage for example output}.
#' @details The package contains html templates tailored for different stocks needed by the authors. Specify the name in the \code{template} argument. Current alternatives are:
#' \describe{
#'   \item{standard}{Standard model output figures. Use the \code{harvest_rate} argument to switch between F and HR figures.}
#'   \item{iceland}{Output tailored for Icelandic stock assessment.}
#'   \item{nea_ghl}{Output tailored for Northeast Arctic Greenland halibut assessment.}
#' }
#' @importFrom DT datatable formatRound
#' @importFrom plotly ggplotly layout subplot
#' @import flexdashboard
#' @export

# Debug params:
# path = getwd(); harvest_rate = TRUE; file_name = 'test.html'; template = "standard"; model_name = "GADGET model"
make_html <- function(fit, path = getwd(), harvest_rate = TRUE, 
                      file_name = 'model_output_figures.html', 
                      template = "standard", model_name = "Gadget model output") {
  path <- fs::path_abs(path)
  fs::dir_create(path)

  # Copy the template to use into the output directory, so temporary files also go there
  # NB: Without, rmarkdown will store temporary kint files in the package directory.
  #     We could set knit_root_dir / intermediates_dir, but intermediates_dir causes spurious duplicated chunks
  template_path <- file.path(path, "template.Rmd")
  fs::file_copy(
      system.file(paste0(template, ".Rmd"), package="gadgetplots"),
      template_path,
      overwrite = TRUE)
  on.exit(fs::file_delete(template_path), add = TRUE)

  rmarkdown::render(
    input = template_path,
    output_dir = path,
    output_file = file_name,
    params = list(fit = fit, harvest_rate = harvest_rate, model_name = model_name)
    )
}
