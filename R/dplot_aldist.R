#' @title Plot age-length distribution data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @param x A gadget3 ready data frame created using mfdb, \link[gadgetutils]{g3_data} or \link[gadgetutils]{add_g3_attributes}.
#' @param type Character specifying the plot type: "bar" or "area".
#' @param facet_age Logical indicating whether ages should be plotted in separate facets
#' @param scales Character defining the \code{\link[ggplot2]{facet_wrap}} \code{scales} argument to use.
#' @param ncol Number of columns passed to \code{\link[ggplot2]{facet_wrap}}
#' @param color_palette A function defining the color palette to be used for fill of bars when \code{facet_age = TRUE}. See \link[ggplot2]{scale_color_manual}. To adjust color when \code{facet_age = FALSE}, use the standard \code{ggplot2::scale_fill_*} functions.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

dplot_aldist <- function(
    x, type = "bar", facet_age = FALSE, scales = "fixed", ncol = NULL,
    color_palette = scales::brewer_pal(palette = "Set1"), base_size = 8
) {

  length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

  first_length_group <- attributes(x)$length[1]
  last_length_group <- attributes(x)$length[length(length_groups)]

  length_groups <- length_groups[-1]


  age_groups <- sapply(attributes(x)$age, function(k) attr(k, "min"))

  first_age_group <- attributes(x)$age[1]
  last_age_group <- attributes(x)$age[length(age_groups)]

  if(!is.null(attr(first_age_group[[1]], "min_open_ended"))) {
    age_groups <- age_groups[-1]
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

  width <- unique(x$max_length - x$min_length)

  x$Age <- as.integer(gsub("age", "", x$age))
  x$year_class <- factor(x$year - x$Age)

  x <- x %>% dplyr::arrange(.data$Date, .data$Age, .data$Length)


  if(type == "bar") {

    if(facet_age) {

      ggplot2::ggplot(
        data = x,
        ggplot2::aes(xmin = .data$min_length, xmax = .data$max_length,
                     ymin = 0, ymax = .data$number,
                     fill = factor(.data$year_class))) +
        ggplot2::geom_vline(xintercept = length_groups, color = "grey",
                            linewidth = 0.5/2.13) +
        ggplot2::geom_vline(
          xintercept =
            attr(first_length_group[[1]], "min"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(first_length_group[[1]], "min_open_ended")),
            "dotted", "solid"), linewidth = 1/2.13) +
        ggplot2::geom_vline(
          xintercept = attr(last_length_group[[1]], "max"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(last_length_group[[1]], "max_open_ended")),
            "dotted", "solid"), linewidth = 1/2.13) +
        ggplot2::geom_rect(color = "black") +
        ggplot2::labs(x = "Length (cm)", y = "Number") +
        ggplot2::facet_grid(.data$Age~.data$Date, scales = scales,
                            labeller = ggplot2::label_wrap_gen(multi_line=FALSE))+
        ggplot2::scale_fill_manual(
          values = repeat_palette(nlevels(x$year_class), pal = color_palette)) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "none",
                       strip.background = ggplot2::element_blank())

    } else {

      ggplot2::ggplot(
        data = x,
        ggplot2::aes(x = .data$Length, y = .data$number, fill = .data$Age)) +
        ggplot2::facet_wrap(~.data$Date, scales = scales, dir = "v", ncol = ncol,
                            labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
        ggplot2::geom_vline(xintercept = length_groups, color = "grey",
                            linewidth = 0.5/2.13) +
        ggplot2::geom_vline(
          xintercept = attr(first_length_group[[1]], "min"),
          color = "grey",
          linetype =
            ifelse(!is.null(attr(first_length_group[[1]], "min_open_ended")),
                   "dotted", "solid"), linewidth = 1/2.13) +
        ggplot2::geom_vline(
          xintercept = attr(last_length_group[[1]], "max"),
          color = "grey",
          linetype =
            ifelse(!is.null(attr(last_length_group[[1]], "max_open_ended")),
                   "dotted", "solid"), linewidth = 1/2.13) +
        ggplot2::scale_x_continuous(expand = c(0,0.5), n.breaks = 8) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::geom_col(color = "black") +
        ggplot2::labs(x = "Length (cm)", y = "Number") +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
    }
  } else {

    ggplot2::ggplot(
      data = x,
      ggplot2::aes(x = .data$Length, y = .data$number, fill = .data$Age,
          group = .data$Age)) +
      ggplot2::geom_vline(xintercept = length_groups, color = "grey",
                          linewidth = 0.5/2.13) +
      ggplot2::geom_vline(
        xintercept = attr(first_length_group[[1]], "min"),
        color = "grey",
        linetype =
          ifelse(!is.null(attr(first_length_group[[1]], "min_open_ended")),
                 "dotted", "solid"), linewidth = 1/2.13) +
      ggplot2::geom_vline(
        xintercept = attr(last_length_group[[1]], "max"),
        color = "grey",
        linetype =
          ifelse(!is.null(attr(last_length_group[[1]], "max_open_ended")),
                 "dotted", "solid"), linewidth = 1/2.13) +
      ggplot2::geom_area(color = "black") +
      ggplot2::labs(x = "Length (cm)", y = "Number") +
      ggplot2::facet_wrap(~.data$Date, scales = scales, dir = "v", ncol = ncol,
                 labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  }
}
