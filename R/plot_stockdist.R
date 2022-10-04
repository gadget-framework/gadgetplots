#' @title Plot stock distribution
#' @description Plots proportions of stocks by length or age in \code{fit$stockdist}
#' @inheritParams plot_annual
#' @param stocks Character vector specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted in a single plot. If \code{"separate"}, all stocks are plotted in separate plots.
#' @return A \link[ggplot2]{ggplot} object. A list of ggplot objects if \code{stocks = "separate"} or if there are multiple data sources (\code{unique(fit$stockdist$name)}).
#' @export

plot_stockdist <- function(fit, stocks = NULL, base_size = 8) {

  lenplot <- function(x) {
    ggplot2::ggplot(x, ggplot2::aes(x = .data$length, y = .data$obs.ratio,
                                    shape = .data$stock, color = .data$stock)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(
        ggplot2::aes(y=.data$pred.ratio, lty = .data$stock), color = "black") +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::labs(y = 'Stock proportion', x = 'Length', color = "Stock", lty = "Stock",
                    shape = "Stock") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  }

  if(length(unique(fit$stockdist$name)) == 1) {

    if(fit$stockdist$length %>% unique() %>% length() > 1) {

      if(is.null(stocks)) {
        fit$stockdist %>%
          dplyr::mutate(
            pred.ratio =
              ifelse(.data$pred.ratio == 0, NA,
                     ifelse(is.nan(.data$pred.ratio), 0, .data$pred.ratio)),
            obs.ratio = ifelse(is.na(.data$pred.ratio), NA, .data$obs.ratio)
          ) %>%
          lenplot()

      } else {

        if(stocks == "separate") stocks <- unique(fit$stockdist$stock)

        fit$stockdist %>%
          dplyr::filter(.data$stock %in% stocks) %>%
          dplyr::pull(.data$stock) %>% unique() %>%
          rlang::set_names() %>%
          purrr::map(function(x){
            fit$stockdist %>%
              dplyr::filter(.data$stock == x) %>%
              dplyr::mutate(
                pred.ratio =
                  ifelse(.data$pred.ratio == 0, NA,
                         ifelse(is.nan(.data$pred.ratio), 0, .data$pred.ratio)),
                obs.ratio = ifelse(is.na(.data$pred.ratio), NA, .data$obs.ratio)
              ) %>%
              lenplot()
          })
      }
    } else {
      fit$stockdist %>%
        dplyr::mutate(pred.ratio =
                        ifelse(is.nan(.data$pred.ratio),0,.data$pred.ratio),
                      age = gsub('age','',.data$age) %>% as.numeric()) %>%
        ggplot2::ggplot(ggplot2::aes(.data$age,.data$obs.ratio,col=.data$stock)) +
        ggplot2::geom_point(ggplot2::aes(pch=.data$stock)) +
        ggplot2::geom_line(ggplot2::aes(y=.data$pred.ratio,lty = .data$stock))+
        ggplot2::facet_wrap(~.data$year+.data$step) +
        ggplot2::labs(y='Stock prop.',x='Age',lty = 'Stock') +
        ggplot2::geom_label(
          data = fit$stockdist %>%
            dplyr::ungroup() %>%
            dplyr::select(.data$year,.data$step) %>%
            dplyr::distinct()%>%
            dplyr::mutate(label=paste(.data$year,.data$step, sep=',')),
          ggplot2::aes(label=.data$label,group=1),
          x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1,
          inherit.aes = FALSE) +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
    }
  } else {
    stop("Multiple names in fit$stockdist has not been implemented yet.")
  }

}
