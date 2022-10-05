#' @title Plot of age composition from the model
#' @inheritParams plot_annual
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_agecomp <- function(fit, base_size = 8) {

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

  dat %>%
    ggplot2::ggplot(ggplot2::aes(.data$year,.data$number/1e6),col='black') +
    ggplot2::geom_bar(stat='identity',ggplot2::aes(fill = .data$yc)) +
    ggplot2::facet_wrap(~.data$age,ncol=1,scales = 'free_y') +
    ggplot2::geom_segment(ggplot2::aes(x=.data$year-0.5,
                                       xend=.data$year+.5),
                          y=Inf, yend=-Inf,lty=2,col='gray',
                          data = year_span, inherit.aes = FALSE) +
    ggplot2::geom_text(ggplot2::aes(Inf,Inf,label=paste('Age',.data$age)),
                       hjust=2,vjust=2,col='gray') +
    ggplot2::labs(x='Year',y='Abundance (in millions)') +
    ggplot2::scale_fill_manual(
      values = rep(pal, ceiling(nlevels(dat$yc) / length(pal)))) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position='none',
                   panel.spacing = ggplot2::unit(0,'cm'),
                   # plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                   strip.background = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank())

}
