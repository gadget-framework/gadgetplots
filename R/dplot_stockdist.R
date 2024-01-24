#' @title Plot stock distribution data for a gadget3 model
#' @description The dplot functions plot data passed to a gadget3 model instead of data from the model or fit objects.
#' @inheritParams dplot_aldist
#' @param stock_col Character defining the name of the column separating stocks.
#' @param proportion Logical indicating whether to plot proportion of stocks instead
#' of absolute numbers.
#' @param group_by_sex Logical indicating whether to take the proportions by sex (\code{TRUE})
#' or by all stocks (\code{FALSE}). Used only when \code{proportion = TRUE}
#' @param sexes Named vector of length two containing regular expressions separating
#' sexes in \code{stock_col}. See the default as an example.
#' @param colors A vector of colors to be used for stocks. If \code{NULL},
#' \code{scales::hue_pal()} will be used
#' @return A \link[ggplot2]{ggplot} object.
#' @export

dplot_stockdist <-
  function(
    x, stock_col = "maturity_stage", proportion = FALSE, group_by_sex = FALSE,
    sexes = c("female" = "^female", "male" = "^male"), colors = NULL,
    scales = "free_y", base_size = 8
  ) {

    length_groups <- sapply(attributes(x)$length, function(k) attr(k, "min"))

    first_length_group <- attributes(x)$length[1]
    last_length_group <- attributes(x)$length[length(length_groups)]

    if(attr(first_length_group[[1]], "min_open_ended")) {
      length_groups <- length_groups[-1]
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

    x$Stock <- x[[stock_col]]

    x <- x %>% dplyr::arrange(.data$Date, .data$Stock, .data$Length)

    ## Colors

    if(is.null(colors)) {
      colors <- scales::hue_pal()(length(unique(x$Stock)))
    }

    ## Plot

    if(!proportion) {

      ggplot2::ggplot(
        x, ggplot2::aes(x = .data$Length, y = .data$number,
                        color = .data$Stock)) +
        ggplot2::geom_vline(xintercept = length_groups, color = "grey") +
        ggplot2::geom_vline(xintercept = attr(first_length_group[[1]], "min"),
                            color = "grey",
                            linetype = ifelse(attr(first_length_group[[1]], "min_open_ended"),
                                              "dotted", "solid")) +
        ggplot2::geom_vline(xintercept = attr(last_length_group[[1]], "max"),
                            color = "grey",
                            linetype = ifelse(attr(last_length_group[[1]], "max_open_ended"),
                                              "dotted", "solid")) +
        ggplot2::geom_path() +
        ggplot2::facet_wrap(~as.character(.data$Date), scales = scales) +
        ggplot2::labs(x = "Length (cm)", y = "Number", color = "Stock") +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::scale_x_continuous(expand = c(0,0.5), n.breaks = 8) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
    } else {

      if(group_by_sex) {

        x$Sex <- ifelse(grepl(sexes[[1]], x$Stock), names(sexes[1]), names(sexes[2]))
        x$Stage <- gsub(paste(sexes[[1]], sexes[[2]], sep = "|"), "", x$Stock)

        y <- x %>%
          dplyr::group_by(.data$Date, .data$Sex, .data$Length) %>%
          dplyr::reframe(
            n_s1 = sum(.data$number[.data$Stage == unique(x$Stage)[1]]),
            n_s2 = sum(.data$number[.data$Stage == unique(x$Stage)[2]])
          ) %>%
          dplyr::mutate(
            s1 = .data$n_s1/(.data$n_s1 + .data$n_s2),
            s2 = .data$n_s2/(.data$n_s1 + .data$n_s2)
          ) %>%
          tidyr::pivot_longer(cols = c(.data$s1, .data$s2), names_to = "Stock") %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::mutate(
            Stock = dplyr::recode(
              .data$Stock,
              s1 = paste0(.data$Sex, unique(x$Stage)[1]),
              s2 = paste0(.data$Sex, unique(x$Stage)[2]),
            )
          )

      } else {
        y <- x %>%
          dplyr::group_by(.data$Date, .data$Length) %>%
          dplyr::mutate(n_total = sum(.data$number),
                        value = .data$number/.data$n_total) %>%
          dplyr::filter(!is.na(.data$value))
      }

      ggplot2::ggplot(
        y, ggplot2::aes(x = .data$Length, y = .data$value, color = .data$Stock)
      ) +
        ggplot2::geom_vline(xintercept = length_groups, color = "grey") +
        ggplot2::geom_vline(xintercept = attr(first_length_group[[1]], "min"),
                            color = "grey",
                            linetype = ifelse(attr(first_length_group[[1]], "min_open_ended"),
                                              "dotted", "solid")) +
        ggplot2::geom_vline(xintercept = attr(last_length_group[[1]], "max"),
                            color = "grey",
                            linetype = ifelse(attr(last_length_group[[1]], "max_open_ended"),
                                              "dotted", "solid")) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~as.character(.data$Date)) +
        ggplot2::labs(x = "Length (cm)", y = "Proportion", color = "Stock") +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
    }
  }
