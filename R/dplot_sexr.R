#' @title Plot sex ratio data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @inheritParams plot_annual
#' @param x A gadget3 ready data frame created using mfdb, \link[gadgetutils]{g3_data} or \link[gadgetutils]{g3_data}.
#' @param ncol Number of columns passed to \code{\link[ggplot2]{facet_wrap}}
#' @return A \link[ggplot2]{ggplot} object.
#' @export

dplot_sexr <- function(x, ncol = NULL, base_size = 8) {

  length_groups <- names(attributes(x)$length) %>%
    gsub("[^0-9.-]", "", .) %>%
    as.numeric()

  first_length_group <- attributes(x)$length[1]
  last_length_group <- attributes(x)$length[length(length_groups)]

  if(!is.null(attr(first_length_group[[1]], "min_open_ended"))) {
    length_groups <- length_groups[-1]
  }

  if(!is.null(attributes(x)$step)) {
    step <- attributes(x)$step
  } else {
    step <- 1
  }

  x$min_length <- unname(sapply(x$length, function(k) {
    tmp <- attributes(x)$length
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "min")
  }))

  x$max_length <- unname(sapply(x$length, function(k) {
    tmp <- attributes(x)$length
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "max")
  }))

  if(!length(step) == 1 & all(1:12 %in% step[[1]])) {
    x$Date <- lubridate::yq(paste(x$year, x$step, sep = "-")) # zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  ggplot2::ggplot(
    data =
      x %>%
      dplyr::group_by(.data$Date, .data$step, .data$area, .data$min_length, .data$max_length) %>%
      dplyr::summarise(n = sum(.data$number), ratio = .data$number[.data$sex == "female"]/.data$n) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(length = mean(c(.data$min_length, .data$max_length))),
  ) +
    ggplot2::geom_vline(xintercept = length_groups, color = "grey") +
    ggplot2::geom_vline(
      xintercept = attr(first_length_group[[1]], "min"),
      color = "grey",
      linetype = ifelse(!is.null(attr(first_length_group[[1]], "min_open_ended")),
                        "dotted", "solid")) +
    ggplot2::geom_vline(
      xintercept = attr(last_length_group[[1]], "max"),
      color = "grey",
      linetype = ifelse(!is.null(attr(last_length_group[[1]], "max_open_ended")),
                        "dotted", "solid")) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$min_length, xmax = .data$max_length,
                   ymin = 0, ymax = .data$ratio),
      fill = "#FF5F68", color = "black") +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$min_length, xmax = .data$max_length,
                   ymin = .data$ratio, ymax = 1),
      fill = "#449BCF", color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$length, y = 1.1, label = .data$n),
      size = FS(6), angle = 270) +
    ggplot2::labs(x = "Length", y = "Sex ratio") +
    ggplot2::facet_wrap(
      ~.data$Date, dir = "v", ncol = ncol,
      labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
    ggplot2::scale_y_continuous(breaks = seq(0,1,0.25), expand = c(0,0)) +
    ggplot2::expand_limits(y = 1.2) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom",
                   strip.background = ggplot2::element_blank())
}
