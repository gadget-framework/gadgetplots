#' @title Plot of age composition from the model
#' @inheritParams plot_annual
#' @inheritParams plot_ldist
#' @param scales Character defining the \code{\link[ggplot2]{facet_wrap}} \code{scales} argument to use.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_agecomp <- function(fit, scales = NULL, ggridges = FALSE, base_size = 8) {

  year_span <-
    fit$stock.std %>%
    dplyr::select(.data$year,.data$age) %>%
    dplyr::distinct()

  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                    "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                    "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")

  dat <- fit$stock.std %>%
    dplyr::mutate(yc = as.factor(.data$year - .data$age))

  if(!ggridges) {
    suppressWarnings({
      ggplot2::ggplot(
        dat,
        ggplot2::aes(.data$year,.data$number/1e6),col='black') +
        ggplot2::geom_bar(stat='identity',
                          ggplot2::aes(fill = .data$yc,
                                       text = paste("age:", round(.data$age)))) +
        ggplot2::facet_wrap(~.data$age,ncol=1,scales = scales, strip.position="right") +
        # ggplot2::geom_segment(ggplot2::aes(x=.data$year-0.5,
        #                                    xend=.data$year+.5),
        #                       y=Inf, yend=-Inf,lty=2,col='gray',
        #                       data = year_span, inherit.aes = FALSE) +
        # ggplot2::geom_text(ggplot2::aes(-Inf,Inf,label=paste('Age',.data$age)),
        #                    hjust=2,vjust=2,col='gray') +
        ggplot2::labs(x='Year',y='Abundance (in millions)') +
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
  } else {
    ggplot2::ggplot(
      dat %>%
        dplyr::group_by(.data$year, .data$step, .data$area, .data$age) %>%
        dplyr::summarise(number = sum(.data$number), .groups = "drop") %>%
        dplyr::group_by(.data$year) %>%
        dplyr::mutate(p = .data$number/sum(.data$number)) %>%
        dplyr::mutate(bars = purrr::map2(.data$age, .data$p, ~make_bar(.x, .y))) %>%
        tidyr::unnest(cols = c(.data$bars)),
      ggplot2::aes(x = .data$x, y = .data$year, height = 2*.data$height,
                   group = .data$year, fill = factor(.data$year))) +
      ggridges::geom_ridgeline(alpha = 0.5, size = 0.5/2.13) +
      ggplot2::scale_y_reverse(breaks = seq(1900,2050,2)) +
      ggplot2::scale_fill_manual(
        values = rep(pal, ceiling(length(unique(dat$year)) / length(pal)))) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(x = "Age", y = "Year") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  }

}
