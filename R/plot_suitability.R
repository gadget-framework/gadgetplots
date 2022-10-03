#' @title Plot suitability
#' @inheritParams plot_annual
#' @param fleets Character vector specifying the fleets to plot in \code{fit}. If \code{NULL}, all fleets will be plotted in separate plots. Use \code{"all"} to plot all fleets to the same plot.
#' @return A \link[ggplot2]{ggplot} object. If \code{fleet = NULL}, a list of ggplot objects.
#' @export

plot_suitability <- function(fit, fleets = "all", base_size = 8) {

  if(is.null(fleets)) {

    rlang::set_names(unique(fit$suitability$fleet)) %>%
      purrr::map(function(x){
        fit$suitability %>%
          dplyr::filter(.data$fleet == x) %>%
          ggplot2::ggplot(ggplot2::aes(.data$length,.data$suit, color = .data$stock)) +
          ggplot2::geom_line() +
          ggplot2::facet_wrap(~.data$year + .data$step) +
          ggplot2::labs(y='Suitability',x='Length', color = 'Stock') +
          ggplot2::geom_text(
            data = fit$suitability %>%
              dplyr::ungroup() %>%
              dplyr::select(.data$year,.data$step) %>%
              dplyr::mutate(y=Inf,
                            label = paste(.data$year,.data$step,sep=',')) %>%
              dplyr::select(.data$step,.data$y,.data$year,.data$label) %>%
              dplyr::distinct(),
            ggplot2::aes(-Inf,Inf,label=.data$label),
            vjust = 1.3,hjust = -.05,
            size = FS(base_size)*0.8,
            inherit.aes = FALSE) +
          ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                              ncol = max(2*length(unique(fit$suitability$step)),4)) +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         panel.spacing = ggplot2::unit(0,'cm'),
                         plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                         strip.background = ggplot2::element_blank(),
                         strip.text.x = ggplot2::element_blank())
      })

  } else {

    if(fleets == "all") fleets <- unique(fit$suitability$fleet)

    fit$suitability %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$fleet %in% fleets) %>%
      ggplot2::ggplot(ggplot2::aes(.data$length,.data$suit, color = .data$stock,
                                   lty = .data$fleet)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~.data$year + .data$step) +
      ggplot2::labs(y='Suitability',x='Length', lty = 'Fleet', color = 'Stock') +
      ggplot2::geom_text(
        data=fit$suitability %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$year,.data$step) %>%
          dplyr::mutate(y=Inf,
                        label = paste(.data$year,.data$step,sep=',')) %>%
          dplyr::select(.data$step,.data$y,.data$year,.data$label) %>%
          dplyr::distinct(),
        ggplot2::aes(-Inf,Inf,label=.data$label),
        vjust = 1.3,hjust = -.05,
        size = FS(base_size)*0.8,
        inherit.aes = FALSE) +
      ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                          ncol = max(2*length(unique(fit$suitability$step)),4)) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())


  }
}
