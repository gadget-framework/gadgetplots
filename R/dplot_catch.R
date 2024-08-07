#' @title Plot catch data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @inheritParams dplot_aldist
#' @param x A gadget3 ready data frame created using mfdb, \link[gadgetutils]{g3_data} or \link[gadgetutils]{add_g3_attributes}. Can be a single data frame or a named list of data frames in
#' which case multiple catches are plotted in the same figure.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(example_catches)
#' dplot_catch(TrawlNor_catches)
#' dplot_catch(
#'   list("TrawlNor" = TrawlNor_catches,
#'        "OtherNor" = OtherNor_catches,
#'        "TrawlRus" = TrawlRus_catches,
#'        "OtherRus" = OtherRus_catches,
#'        "Internat" = Internat_catches)
#'        )
#' @export

dplot_catch <- function(x, base_size = 8) {

  ## Compile multiple catches
  if(inherits(x, "list")) {
    x <- lapply(seq_along(x), function(i) {
      out <- x[[i]]
      out$name <- names(x)[i]
      out
    }) %>%
      dplyr::bind_rows()
  }

  if(nrow(x) == 0) {
    return({
      ggplot2::ggplot() +
        ggplot2::geom_blank() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data",
                          size = FS(12), fontface = 2) +
        ggplot2::labs(x = "Year", y = "Total weight (kg)") +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    })
  }

  ## Time

  step <- attributes(x)$step

  if(!length(step) == 1 & all(1:12 %in% step[[1]])) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% dplyr::arrange(.data$Date)

  ## Plot

  if(is.null(x$name)) {
    ggplot2::ggplot(x, ggplot2::aes(x = .data$Date, y = .data$total_weight)) +
      ggplot2::geom_col(fill = "#449BCF") +
      ggplot2::scale_x_continuous(n.breaks = 10) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(x = "Year", y = "Total weight (kg)") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else {

    ggplot2::ggplot(
      x,
      ggplot2::aes(x = .data$Date, y = .data$total_weight, fill = .data$name)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(n.breaks = 10) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(x = "Year", y = "Total weight (kg)", fill = "Fleet") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }

}


