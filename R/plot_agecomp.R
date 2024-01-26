#' @title Plot of age composition from the model
#' @inheritParams plot_annual
#' @inheritParams plot_biomass
#' @inheritParams plot_hr
#' @param type Character specifying the plot type. Options: \code{"bar"}, \code{"line"}, \code{"ggridges"} or \code{"bubble"}. See Details.
#' @details Possible plot types are:
#' \describe{
#'   \item{bar}{Facetted bar plot with year on x-axis, abundance on y-axis and ages along rows. Year classes are colored.}
#'   \item{line}{Facetted line plot with year on x-axis, abundance on y-axis and ages along rows. Stocks are colored.}
#'   \item{ggridges}{Same as above but the bars are overlapping.}
#'   \item{bubble (or any other string)}{Bubble plot with year on x-axis, age on y-axis and point size scaled to abundance. Year classes are colored.}
#'   }
#' Note that both \code{"bar"} and \code{"bubble"} contain extra information as text for \code{plotly::ggplotly}.
#' @param scales Character defining the \code{\link[ggplot2]{facet_wrap}} \code{scales} argument to use.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(fit)
#' plot_agecomp(fit)
#' plot_agecomp(fit, type = "line", biomass = TRUE)
#' plot_agecomp(fit, type = "bar")
#' plot_agecomp(fit, type = "ggridges")
#' @export

plot_agecomp <- function(fit, type = "bubble", scales = "fixed", biomass = FALSE, base_size = 8, return_data = FALSE) {

  year_span <-
    fit$stock.std %>%
    dplyr::select(.data$year,.data$age) %>%
    dplyr::distinct()

  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                    "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")

  if(type != "line") {
    dat <- fit$stock.std %>%
      dplyr::group_by(.data$year, .data$step, .data$area, .data$age) %>%
      dplyr::summarise(
        number = sum(.data$number, na.rm = TRUE)/1e6, # millions
        weight = sum(.data$number*.data$mean_weight, na.rm = TRUE), #kt
        .groups = "drop") %>%
      dplyr::mutate(yc = as.factor(.data$year - .data$age))

    if(return_data) return(dat)
  }

  if(type == "line") {

    dat <- fit$stock.std %>%
      dplyr::group_by(.data$year, .data$step, .data$area, .data$stock, .data$age) %>%
      dplyr::summarise(
        number = sum(.data$number, na.rm = TRUE)/1e6, # millions
        weight = sum(.data$number*.data$mean_weight, na.rm = TRUE), #kt
        .groups = "drop") %>%
      dplyr::mutate(yc = as.factor(.data$year - .data$age))

    if(return_data) return(dat)

    suppressWarnings({
      ggplot2::ggplot(dat) + {
        if(!biomass) ggplot2::geom_line(
          ggplot2::aes(
            .data$year,.data$number,
            color = .data$stock,
            text = paste("age:", round(.data$age))
          ),
        )} + {
          if(biomass) ggplot2::geom_line(
            ggplot2::aes(
              .data$year,.data$weight,
              color = .data$stock,
              text = paste("age:", round(.data$age))),
          )} +
        ggplot2::facet_wrap(
          ~.data$age, ncol=1, scales = scales, strip.position="right"
        ) +
        ggplot2::labs(
          x = 'Year',
          y = ifelse(biomass, 'Biomass (kt)', 'Abundance (in millions)'),
          color = 'Stock'
        ) +
        ggplot2::scale_x_continuous(breaks = seq(1900,2050,2), expand = c(0,0)) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = 'bottom',
                       panel.spacing = ggplot2::unit(0,'cm'),
                       # plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_blank())
    })

  } else if(type == "bar") {
    suppressWarnings({
      ggplot2::ggplot(dat) + {
        if(!biomass) ggplot2::geom_bar(
          stat='identity',
          ggplot2::aes(
            .data$year,.data$number,
            fill = .data$yc,
            text = paste("age:", round(.data$age))
          ),
          col = 'black', linewidth = 0.5/2.13)} + {
            if(biomass) ggplot2::geom_bar(
              stat='identity',
              ggplot2::aes(
                .data$year,.data$weight,
                fill = .data$yc,
                text = paste("age:", round(.data$age))),
              col = 'black', linewidth = 0.5/2.13)} +
        ggplot2::facet_wrap(
          ~.data$age, ncol=1, scales = scales, strip.position="right"
        ) +
        # ggplot2::geom_segment(ggplot2::aes(x=.data$year-0.5,
        #                                    xend=.data$year+.5),
        #                       y=Inf, yend=-Inf,lty=2,col='gray',
        #                       data = year_span, inherit.aes = FALSE) +
        # ggplot2::geom_text(ggplot2::aes(-Inf,Inf,label=paste('Age',.data$age)),
        #                    hjust=2,vjust=2,col='gray') +
        ggplot2::labs(
          x = 'Year',
          y = ifelse(biomass, 'Biomass (kt)', 'Abundance (in millions)')
        ) +
        ggplot2::scale_fill_manual(
          values = rep(pal, ceiling(nlevels(dat$yc) / length(pal)))) +
        ggplot2::scale_x_continuous(breaks = seq(1900,2050,2)) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position='none',
                       panel.spacing = ggplot2::unit(0,'cm'),
                       # plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_blank())
    })
  } else if(type == "ggridges") {
    # From: https://stackoverflow.com/a/47614529/1082004
    make_bar <- function(x, y, width = 0.9) {
      xoff <- width/2
      data.frame(x = c(x-xoff*(1+2e-8), x-xoff*(1+1e-8), x-xoff, x+xoff, x+xoff*(1+1e-8), x+xoff*(1+2e-8)),
                 height = c(NA, 0, y, y, 0, NA))
    }

    ggplot2::ggplot(
      dat %>%
        dplyr::group_by(.data$year) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)) %>%
        dplyr::mutate(bars = purrr::map2(.data$age, .data$p, ~make_bar(.x, .y))) %>%
        tidyr::unnest(cols = c(.data$bars)),
      ggplot2::aes(x = .data$x, y = .data$year, height = 2*.data$height,
                   group = .data$year, fill = factor(.data$year))) +
      ggridges::geom_ridgeline(alpha = 0.5, linewidth = 0.5/2.13) +
      ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
      ggplot2::scale_fill_manual(
        values = rep(pal, ceiling(length(unique(dat$year)) / length(pal)))) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(x = "Age", y = "Year") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

  } else {
    suppressWarnings({
      ggplot2::ggplot(
        dat %>%
          dplyr::mutate(number = ifelse(.data$number == 0, NA, .data$number),
                        weight = ifelse(.data$weight == 0, NA, .data$weight)) %>%
          dplyr::rename("year class" = "yc")
      ) + {
        if(!biomass) ggplot2::geom_point(
          ggplot2::aes(
            .data$year,.data$age, size = .data$number,
            color = .data$`year class`,
            text = paste(
              "abundance (1e6): ", round(.data$number))
          ),
          shape = 21, stroke = 1
        ) } + {
          if(biomass) ggplot2::geom_point(
            ggplot2::aes(
              .data$year,.data$age, size = .data$weight,
              color = .data$`year class`,
              text = paste(
                "biomass (1e6): ", round(.data$weight))
            ),
            shape = 21, stroke = 1
          ) } +
        ggplot2::labs(
          x = 'Year',
          y = 'Age',
          size = ifelse(biomass, 'Biomass (kt)', 'Abundance (in millions)')
        ) +
        ggplot2::scale_color_manual(
          values = rep(pal, ceiling(nlevels(dat$yc) / length(pal))),
          guide = 'none') +
        ggplot2::scale_x_continuous(breaks = seq(1900,2050,2)) +
        ggplot2::scale_y_reverse() +
        ggplot2::scale_size_area(guide = ggplot2::guide_legend(nrow = 1),
                                 max_size = 12) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position='bottom',
                       panel.spacing = ggplot2::unit(0,'cm'),
                       # plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_blank())
    })
  }
}
