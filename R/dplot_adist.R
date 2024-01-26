#' @title Plot age distribution data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @inheritParams dplot_aldist
#' @param type  Character specifying the plot type: "bar" or "ggridges".
#' @param color_palette A function defining the color palette to be used for fill of bars.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(aldist_example)
#' dplot_adist(aldist_example)
#' dplot_adist(aldist_example, type = "ggridges")
#' @export

dplot_adist <- function(
    x, type = "bar", color_palette = scales::brewer_pal(palette = "Set1"),
    scales = "fixed", ncol = NULL, base_size = 8
    ) {

  age_groups <- sapply(attributes(x)$age, function(k) attr(k, "min"))

  first_age_group <- attributes(x)$age[1]
  last_age_group <- attributes(x)$age[length(age_groups)]

  if(!is.null(attr(first_age_group[[1]], "min_open_ended"))) {
    age_groups <- age_groups[-1]
  }

  step <- attributes(x)$step

  if(!length(step) == 1 & all(1:12 %in% step[[1]])) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  x$min_age <- unname(sapply(x$age, function(k) {
    tmp <- attributes(x)$age
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "min")
  }))

  x$max_age <- unname(sapply(x$age, function(k) {
    tmp <- attributes(x)$age
    attr(tmp[names(tmp) == gsub("\\+", "", k)][[1]], "max")
  }))

  x$Age <- rowMeans(x[c("min_age", "max_age")])

  x <- x %>%
    dplyr::group_by(.data$year, .data$step, .data$area, .data$age, .data$Age) %>%
    dplyr::summarise(value = sum(.data$number)) %>%
    dplyr::arrange(.data$year, .data$step, .data$area, .data$Age) %>%
    dplyr::mutate(year_class = factor(.data$year - .data$Age))

  if(type == "bar") {
    ggplot2::ggplot(
      x, ggplot2::aes(x = .data$Age, y = .data$value, fill = .data$year_class)) +
      ggplot2::geom_vline(xintercept = age_groups, color = "grey", linewidth = 0.5/2.13) +
      ggplot2::geom_vline(xintercept = attr(first_age_group[[1]], "min"),
                 color = "grey",
                 linetype = ifelse(!is.null(attr(first_age_group[[1]], "min_open_ended")),
                                   "dotted", "solid"), linewidth = 1/2.13) +
      ggplot2::geom_vline(xintercept = attr(last_age_group[[1]], "max"),
                 color = "grey",
                 linetype = ifelse(attr(last_age_group[[1]], "max_open_ended"),
                                   "dotted", "solid"), linewidth = 1/2.13) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~year, dir = "v", scales = scales, ncol = ncol) +
      ggplot2::scale_fill_manual(values = repeat_palette(nlevels(x$year_class),
                                                pal = color_palette)) +
      ggplot2::scale_x_continuous(expand = c(0,0.1), n.breaks = 8) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::expand_limits(x = 0) +
      ggplot2::labs(x = "Age", y = "Number") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = "none",
            strip.background = ggplot2::element_blank())
  } else {
    ggplot2::ggplot(
      x %>%
        dplyr::group_by(.data$year) %>%
        dplyr::mutate(p = .data$value/sum(.data$value)),
           ggplot2::aes(x = .data$Age, y = .data$year, height = 10*.data$p,
                        group = .data$year)) +
      ggplot2::geom_vline(xintercept = age_groups, color = "grey", linewidth = 0.5/2.13) +
      ggplot2::geom_vline(xintercept = attr(first_age_group[[1]], "min"),
                 color = "grey",
                 linetype = ifelse(!is.null(attr(first_age_group[[1]], "min_open_ended")),
                                   "dotted", "solid"), linewidth = 1/2.13) +
      ggplot2::geom_vline(xintercept = attr(last_age_group[[1]], "max"),
                 color = "grey",
                 linetype = ifelse(attr(last_age_group[[1]], "max_open_ended"),
                                   "dotted", "solid"), linewidth = 1/2.13) +
      ggridges::geom_ridgeline(alpha = 0.5, fill = 'darkblue') +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::scale_y_reverse(breaks = seq(1980,2030,2), expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = c(0,0.1), n.breaks = 8) +
      ggplot2::expand_limits(x = 0) +
      ggplot2::labs(x = "Age", y = "Year") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }
}

