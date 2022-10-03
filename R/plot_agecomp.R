#' @title Plot of age composition from the model
#' @inheritParams plot_annual
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_agecomp <- function(fit, base_size = 8) {

  year_span <-
    fit$stock.std %>%
    dplyr::select(.data$year,.data$age) %>%
    dplyr::distinct()

  fit$stock.std %>%
    dplyr::mutate(yc = as.factor(.data$year - .data$age)) %>%
    ggplot2::ggplot(ggplot2::aes(.data$year,.data$number/1e9),col='black') +
    ggplot2::geom_bar(stat='identity',ggplot2::aes(fill = .data$yc)) +
    ggplot2::facet_wrap(~.data$age,ncol=1,scales = 'free_y') +
    ggplot2::geom_segment(ggplot2::aes(x=.data$year-0.5,
                                       xend=.data$year+.5),
                          y=Inf, yend=-Inf,lty=2,col='gray',
                          data = year_span, inherit.aes = FALSE) +
    ggplot2::geom_text(ggplot2::aes(Inf,Inf,label=paste('Age',.data$age)),
                       hjust=2,vjust=2,col='gray') +
    ggplot2::labs(x='Year',y='Abundance (in billions)') +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position='none',panel.spacing = ggplot2::unit(0,'cm'),
                   plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                   strip.background = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank())

}
