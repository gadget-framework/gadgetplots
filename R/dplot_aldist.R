#' @title Plot age-length distribution data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @param x A gadget3 ready data frame created using mfdb, \link[gadgetutils]{g3_data} or \link[gadgetutils]{add_g3_attributes}.
#' @param type Character specifying the plot type: "bar", "step", or "area". "step" produces a similar plot where age and length distributions are plotted separately as in \link{plot_aldist}. "area" produces an area plot instead of a bar plot, but works poorly when there are many age groups.
#' @param facet_age Logical indicating whether ages should be plotted in separate facets. Does not apply for \code{type = "step"}.
#' @param scales Character defining the \code{\link[ggplot2]{facet_wrap}} \code{scales} argument to use.
#' @param ncol Number of columns passed to \code{\link[ggplot2]{facet_wrap}}
#' @param color_palette A function defining the color palette to be used for fill of bars when \code{facet_age = TRUE}. See \link[ggplot2]{scale_color_manual}. To adjust color when \code{facet_age = FALSE}, use the standard \code{ggplot2::scale_fill_*} functions.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(aldist_example)
#' dplot_aldist(aldist_example)
#' dplot_aldist(aldist_example, facet_age = TRUE)
#' dplot_aldist(aldist_example, type = "area") # works poorly
#' @export

# type = "bar"; facet_age = FALSE; scales = "fixed"; ncol = NULL; color_palette = scales::brewer_pal(palette = "Set1"); base_size = 8
dplot_aldist <- function(
  x,
  type = "bar",
  facet_age = FALSE,
  scales = "fixed",
  ncol = NULL,
  color_palette = scales::brewer_pal(palette = "Set1"),
  base_size = 8
) {
  length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

  first_length_group <- attributes(x)$length[1]
  last_length_group <- attributes(x)$length[length(length_groups)]

  length_groups <- length_groups[-1]

  age_groups <- sapply(attributes(x)$age, function(k) attr(k, "min"))

  first_age_group <- attributes(x)$age[1]
  last_age_group <- attributes(x)$age[length(age_groups)]

  if (!is.null(attr(first_age_group[[1]], "min_open_ended"))) {
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

  if (!length(step) == 1 & all(1:12 %in% step[[1]])) {
    x$Date <- zoo::as.yearqtr(paste(x$year, x$step, sep = "Q"))
  } else {
    x$Date <- x$year
  }

  width <- unique(x$max_length - x$min_length)

  x$Age <- as.integer(gsub("age", "", x$age))
  x$year_class <- factor(x$year - x$Age)

  x <- x %>% dplyr::arrange(.data$Date, .data$Age, .data$Length)

  if (type == "step") {
    p1 <- x %>%
      dplyr::group_by(.data$year, .data$step, .data$Age) %>%
      dplyr::reframe(observed = sum(.data$number, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$Age, y = .data$observed),
        color = "grey"
      ) +
      ggplot2::facet_wrap(
        ~ .data$year + .data$step,
        labeller = ggplot2::label_wrap_gen(multi_line = FALSE),
        scales = scales
      ) +
      ggplot2::labs(y = 'Proportion', x = 'Age') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )

    p2 <- x %>%
      dplyr::group_by(.data$year, .data$step, .data$min_length) %>%
      dplyr::reframe(observed = sum(.data$number, na.rm = TRUE)) %>%
      dplyr::bind_rows(
        x %>%
          dplyr::group_by(.data$year, .data$step, .data$max_length) %>%
          dplyr::reframe(observed = sum(.data$number, na.rm = TRUE)) %>%
          dplyr::group_by(.data$year, .data$step) %>%
          dplyr::filter(.data$max_length == max(.data$max_length)) %>%
          dplyr::rename(min_length = .data$max_length)
      ) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$min_length, y = .data$observed),
        color = "grey"
      ) +
      ggplot2::facet_wrap(
        ~ .data$year + .data$step,
        labeller = ggplot2::label_wrap_gen(multi_line = FALSE),
        scales = scales
      ) +
      ggplot2::labs(y = 'Proportion', x = 'Length') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )

    cowplot::plot_grid(p1, p2, ncol = 2)
  } else if (type == "bar") {
    if (facet_age) {
      ggplot2::ggplot(
        data = x,
        ggplot2::aes(
          xmin = .data$min_length,
          xmax = .data$max_length,
          ymin = 0,
          ymax = .data$number,
          fill = factor(.data$year_class)
        )
      ) +
        ggplot2::geom_vline(
          xintercept = length_groups,
          color = "grey",
          linewidth = 0.5 / 2.13
        ) +
        ggplot2::geom_vline(
          xintercept = attr(first_length_group[[1]], "min"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(first_length_group[[1]], "min_open_ended")),
            "dotted",
            "solid"
          ),
          linewidth = 1 / 2.13
        ) +
        ggplot2::geom_vline(
          xintercept = attr(last_length_group[[1]], "max"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(last_length_group[[1]], "max_open_ended")),
            "dotted",
            "solid"
          ),
          linewidth = 1 / 2.13
        ) +
        ggplot2::geom_rect(color = "black", linewidth = 0.5 / 2.13) +
        ggplot2::labs(x = "Length (cm)", y = "Number") +
        ggplot2::facet_grid(
          .data$Age ~ .data$Date,
          scales = scales,
          labeller = ggplot2::label_wrap_gen(multi_line = FALSE)
        ) +
        ggplot2::scale_fill_manual(
          values = repeat_palette(nlevels(x$year_class), pal = color_palette)
        ) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(
          legend.position = "none",
          strip.background = ggplot2::element_blank()
        )
    } else {
      ggplot2::ggplot(
        data = x,
        ggplot2::aes(x = .data$Length, y = .data$number, fill = .data$Age)
      ) +
        ggplot2::facet_wrap(
          ~ .data$Date,
          scales = scales,
          dir = "v",
          ncol = ncol,
          labeller = ggplot2::label_wrap_gen(multi_line = FALSE)
        ) +
        ggplot2::geom_vline(
          xintercept = length_groups,
          color = "grey",
          linewidth = 0.5 / 2.13
        ) +
        ggplot2::geom_vline(
          xintercept = attr(first_length_group[[1]], "min"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(first_length_group[[1]], "min_open_ended")),
            "dotted",
            "solid"
          ),
          linewidth = 1 / 2.13
        ) +
        ggplot2::geom_vline(
          xintercept = attr(last_length_group[[1]], "max"),
          color = "grey",
          linetype = ifelse(
            !is.null(attr(last_length_group[[1]], "max_open_ended")),
            "dotted",
            "solid"
          ),
          linewidth = 1 / 2.13
        ) +
        ggplot2::scale_x_continuous(expand = c(0, 0.5), n.breaks = 8) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::geom_col(color = "black", linewidth = 0.2 / 2.13) +
        ggplot2::labs(x = "Length (cm)", y = "Number") +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(
          legend.position = "bottom",
          strip.background = ggplot2::element_blank()
        )
    }
  } else {
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = .data$Length,
        y = .data$number,
        fill = .data$Age,
        group = .data$Age
      )
    ) +
      ggplot2::geom_vline(
        xintercept = length_groups,
        color = "grey",
        linewidth = 0.5 / 2.13
      ) +
      ggplot2::geom_vline(
        xintercept = attr(first_length_group[[1]], "min"),
        color = "grey",
        linetype = ifelse(
          !is.null(attr(first_length_group[[1]], "min_open_ended")),
          "dotted",
          "solid"
        ),
        linewidth = 1 / 2.13
      ) +
      ggplot2::geom_vline(
        xintercept = attr(last_length_group[[1]], "max"),
        color = "grey",
        linetype = ifelse(
          !is.null(attr(last_length_group[[1]], "max_open_ended")),
          "dotted",
          "solid"
        ),
        linewidth = 1 / 2.13
      ) +
      ggplot2::geom_area(color = "black", linewidth = 0.2 / 2.13) +
      ggplot2::labs(x = "Length (cm)", y = "Number") +
      ggplot2::facet_wrap(
        ~ .data$Date,
        scales = scales,
        dir = "v",
        ncol = ncol,
        labeller = ggplot2::label_wrap_gen(multi_line = FALSE)
      ) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(
        legend.position = "bottom",
        strip.background = ggplot2::element_blank()
      )
  }
}
