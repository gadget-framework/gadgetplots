#' @title Plot length distribution data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @inheritParams dplot_aldist
#' @param type Character specifying the plot type: "bar", "ggridges" or path
#' @return A \link[ggplot2]{ggplot} object.
#' @export

dplot_ldist <- function(x, type = "bar", scales = "free_y", base_size = 8) {

  length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

  first_length_group <- attributes(x)$length[1]
  last_length_group <- attributes(x)$length[length(length_groups)]

  if(!is.null(attr(first_length_group[[1]], "min_open_ended"))) {
    length_groups <- length_groups[-1]
    min_open_ended <- TRUE
  } else {
    min_open_ended <- FALSE
  }

  if(!is.null(attr(last_length_group[[1]], "max_open_ended"))) {
    max_open_ended <- TRUE
  } else {
    max_open_ended <- FALSE
  }

  step <- attributes(x)$step

  x$min_length <- unname(sapply(x$length, function(k) {
    tmp <- attributes(x)$length
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "min")
  }))

  x$max_length <- unname(sapply(x$length, function(k) {
    tmp <- attributes(x)$length
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "max")
  }))

  x$Length <- rowMeans(x[c("min_length", "max_length")])

  if(!length(step) == 1 & all(1:12 %in% step[[1]])) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x <- x %>% dplyr::arrange(.data$Date, .data$Length)

  if(type == "bar") {

    ggplot2::ggplot(data = x,
                    ggplot2::aes(xmin = .data$min_length, xmax = .data$max_length,
                                 ymin = 0, ymax = .data$number)) +
      ggplot2::geom_vline(xintercept = length_groups, color = "grey") +
      ggplot2::geom_vline(xintercept = attr(first_length_group[[1]], "min"),
                          color = "grey",
                          linetype = ifelse(min_open_ended, "dotted", "solid")
      ) +
      ggplot2::geom_vline(xintercept = attr(last_length_group[[1]], "max"),
                          color = "grey",
                          linetype = ifelse(max_open_ended, "dotted", "solid")
      ) +
      ggplot2::geom_rect(fill = "grey", color = "black") +
      ggplot2::labs(x = "Length (cm)", y = "Number") +
      ggplot2::facet_wrap(~.data$year+.data$step, scales = scales,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else if(type == "ggridges") {

    ggplot2::ggplot(
      x %>%
        dplyr::group_by(.data$Date) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)),
      ggplot2::aes(x = .data$Length, y = .data$Date, height = 10*.data$p,
                   group = .data$Date)
    ) +
      ggplot2::geom_vline(xintercept = length_groups, color = "grey", linewidth = 0.5/2.13) +
      ggplot2::geom_vline(xintercept = attr(first_length_group[[1]], "min"),
                          color = "grey",
                          linetype = ifelse(min_open_ended, "dotted", "solid"),
                          linewidth = 1/2.13) +
      ggplot2::geom_vline(xintercept = attr(last_length_group[[1]], "max"),
                          color = "grey",
                          linetype = ifelse(max_open_ended, "dotted", "solid"),
                          linewidth = 1/2.13) +
      ggridges::geom_ridgeline(fill = 'darkblue', alpha = 0.5, linewidth = 0.5/2.13) +
      ggplot2::scale_y_reverse(breaks = seq(1900,2050,2), expand = c(0,0.5)) +
      ggplot2::scale_x_continuous(
        limits = c(if(min_open_ended) 0 else attr(first_length_group[[1]], "min")-1,
                   attr(last_length_group[[1]], "max")+1),
        expand = c(0,0.5), n.breaks = 8) +
      ggplot2::labs(x = "Length", y = "Year", fill = "Stock") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank())

  } else {

    ggplot2::ggplot(
      x,
      ggplot2::aes(x = .data$Length, y = .data$number, color = .data$step)
    ) +
      ggplot2::geom_path() +
      ggplot2::facet_wrap(~.data$year, scales = scales, dir = "v") +
      ggplot2::labs(x = "Length (cm)", y = "Count", color = "Timestep") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  }
}
