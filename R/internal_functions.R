# Internal functions that are not exported

#' @title Bind fit components over multiple gadget.fit objects
#' @param fit_list A list of multiple gadget.fit objects
#' @param component Character specifying the list component in a gadget.fit object
#' @keywords internal
#' @export
bind_fit_components <- function(fit_list, component){

  tmp <- lapply(fit_list, function(x, component){
    return(x[[component]])
  }, component = component)
  out <- dplyr::bind_rows(tmp, .id = 'id')
  return(out)

}

#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @keywords internal
#'
FS <- function(x) x/2.845276 # x is the desired font / line size in pt

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
#'
LS <- function(x) x/2.13

#' @title Pick a suitable number of cores
#' @description Picks maximum four cores for parallel processing
#' @return Integer of suitable number of cores
#' @keywords internal
#' @importFrom parallel detectCores
#' @author The \href{https://github.com/StoXProject/RstoxData/blob/master/R/Utilities.R}{StoXProject}
#' @export

getCores <- function() {
  cores <- as.integer(getOption("mc.cores"))
  if (length(cores) == 0 || is.na(cores)) {
    cores <- parallel::detectCores()
    if (is.na(cores)) {
      return(1)
    } else {
      # Don't use too many cores in autodetect
      if (cores > 4)
        return(4)
      else
        return(cores)
    }
  } else {
    return(cores)
  }
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Define global variables
utils::globalVariables(c(".data"))
utils::globalVariables(".")
