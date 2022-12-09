#' @title Plot catch distributions
#' @description Plots the length distribution of each \code{fit$catchdist.fleets$name}
#' @inheritParams plot_annual
#' @param name A character vector specifying the unique(fit$catchdist.fleets$name) to plot. If \code{NULL}, all names with relevant data are plotted.
#' @return A list of \link[ggplot2]{ggplot} objects consisting of a separate plot for each of \code{names}.
#' @export

plot_catchdist <- function(fit, name = NULL, base_size = 8) {

  if(is.null(name)) name <- unique(fit$catchdist.fleets$name)

  rlang::set_names(name) %>%
    purrr::map(function(x){
      dat <- fit$catchdist.fleets %>%
        dplyr::filter(.data$name == x)
      if(length(unique(dat$age))==1){
        if(any(sapply(intersect(c("stock", "stock_re"), colnames(dat)), function(k) sum(!is.na(dat[[k]])) > 0))) {
          warning("The length data appears to be allocated to stocks. The plotted length distribution is wrong.")
        }
        dat %>%
          dplyr::ungroup() %>%
          ggplot2::ggplot(ggplot2::aes(.data$avg.length,.data$predicted)) +
          ggplot2::geom_line(ggplot2::aes(.data$avg.length,.data$observed),col='gray') +
          ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                              ncol = max(2*length(unique(dat$step)),4))  +
          ggplot2::geom_line() +
          ggplot2::geom_text(
            data = dat %>%
              dplyr::ungroup() %>%
              dplyr::filter(.data$avg.length == min(.data$avg.length)) %>%
              dplyr::mutate(y=Inf,
                            label = paste(.data$year,.data$step,sep=',')) %>%
              dplyr::select(.data$step,.data$avg.length,.data$y,.data$year,.data$label),

            ggplot2::aes(.data$avg.length,.data$y,label=.data$label),
            vjust = 1.3,hjust = -.05,
            size = FS(base_size)*0.8,
            inherit.aes = FALSE) +
          ggplot2::labs(y ='Proportion', x = 'Length') +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme (axis.text.y = ggplot2::element_blank(),
                          axis.ticks.y = ggplot2::element_blank(),
                          panel.spacing = ggplot2::unit(0,'cm'),
                          plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                          strip.background = ggplot2::element_blank(),
                          strip.text.x = ggplot2::element_blank())
      } else {
        # group_cols <- intersect(c("year", "step", "area", "stock", "stock_re"))

        dat %>%
          dplyr::group_by(.data$year,.data$step,.data$age) %>%
          dplyr::summarise(predicted=sum(.data$predicted),
                           observed=sum(.data$observed,na.rm=TRUE)) %>%
          dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>%
          dplyr::ungroup() %>%
          ggplot2::ggplot(ggplot2::aes(.data$age,.data$predicted)) +
          ggplot2::geom_line(ggplot2::aes(.data$age,.data$observed),col='gray') +
          ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                              ncol = max(2*length(unique(dat$step)),4))  +
          ggplot2::geom_line() +
          ggplot2::geom_text(
            data = dat %>%
              dplyr::ungroup() %>%
              dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>%
              dplyr::filter(.data$age == min(.data$age)) %>%
              dplyr::mutate(y=Inf,
                            label = paste(.data$year,.data$step,sep=',')) %>%
              dplyr::select(.data$step,.data$age,.data$y,.data$year,.data$label) %>%
              dplyr::distinct(),
            ggplot2::aes(.data$age,.data$y,label=.data$label),
            vjust = 1.3,hjust = -.05,
            size = FS(base_size)*0.8,
            inherit.aes = FALSE) +
          ggplot2::labs(y='Proportion', x='Age') +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme (axis.text.y = ggplot2::element_blank(),
                          axis.ticks.y = ggplot2::element_blank(),
                          panel.spacing = ggplot2::unit(0,'cm'),
                          plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                          strip.background = ggplot2::element_blank(),
                          strip.text.x = ggplot2::element_blank())
      }
    })
}
