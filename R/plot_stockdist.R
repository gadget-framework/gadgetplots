#' @title Plot stock distribution
#' @description Plots proportions of stocks by length or age in \code{fit$stockdist}
#' @inheritParams plot_annual
#' @inheritParams plot_biomass
#' @param stocks Character vector specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted in a single plot. If \code{"separate"}, all stocks are plotted in separate plots.
#' @param type Character specifying the plot type. Options: \code{"model_fit"}, \code{"stock_composition"} or \code{"sex_composition"}. See Details.
#' @param color_palette A function defining the color palette or a vector of colors to be used for stocks.
#' @details Possible plot types are:
#' \describe{
#'   \item{model_fit}{Model fit to data using likelihoods}
#'   \item{stock_composition}{Model stock composition}
#'   \item{sex_composition}{Model sex composition. Not implemented yet.}
#'   }
#' @return A \link[ggplot2]{ggplot} object. A list of ggplot objects if \code{stocks = "separate"} or if there are multiple data sources (\code{unique(fit$stockdist$name)}).
#' @export

plot_stockdist <- function(fit, stocks = NULL, type = "model_fit", color_palette = scales::hue_pal(), geom_area = FALSE, base_size = 8) {

  if(inherits(color_palette, "function")) {
    cols <- stats::setNames(color_palette(length(unique(fit$stockdist$stock))),
                            unique(fit$stockdist$stock))
  } else {
    if(length(color_palette) != length(unique(fit$stockdist$stock))) {
      stop("color_palette has to be a vector with a same length than the number of stocks in the model or a color palette function")
    } else {
      cols <- color_palette
    }
  }

  lenplot <- function(type, stocks = NULL, geom_area, color_palette) {

    if(type == "model_fit") {

      fit$stockdist %>%
        dplyr::mutate(
          pred.ratio =
            ifelse(.data$pred.ratio == 0, NA,
                   ifelse(is.nan(.data$pred.ratio), 0, .data$pred.ratio)),
          obs.ratio = ifelse(is.na(.data$pred.ratio), NA, .data$obs.ratio)
        ) %>%
        dplyr::filter(.data$stock %in% stocks) %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$length, y = .data$obs.ratio,
                       shape = .data$stock, color = .data$stock)) +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$pred.ratio, lty = .data$stock), color = "black") +
        ggplot2::geom_point(size = base_size/16) +
        ggplot2::facet_wrap(~.data$year+.data$step,
                            labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
        ggplot2::labs(y = 'Stock proportion', x = 'Length', color = "Stock",
                      lty = "Stock", shape = "Stock") +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
    } else {

      x <- fit$stock.full %>%
        dplyr::group_by(.data$year, .data$step, .data$stock, .data$length) %>%
        dplyr::summarise(n = sum(.data$number)) %>%
        dplyr::group_by(.data$year, .data$step, .data$length) %>%
        dplyr::mutate(p = .data$n/sum(.data$n)) %>%
        dplyr::filter(.data$stock %in% stocks)

      {
        if(geom_area) {
          ggplot2::ggplot(data = x, ggplot2::aes(x = .data$length, y = .data$p)) +
            ggplot2::geom_area(ggplot2::aes(fill = .data$stock)) +
            ggplot2::scale_fill_manual(values = cols)
        } else {
          ggplot2::ggplot(data = x, ggplot2::aes(x = .data$length, y = .data$p)) +
            ggplot2::geom_line(ggplot2::aes(color = .data$stock),
                               size = base_size/16) +
            ggplot2::scale_color_manual(values = cols)
        }
        } +
        ggplot2::labs(y = 'Stock proportion', x = 'Length', color = "Stock",
                      fill = "Stock") +
        ggplot2::facet_wrap(~.data$year+.data$step,
                            labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
    }
  }

  lenageplot <- function(x) {
    x %>%
      dplyr::mutate(pred.ratio =
                      ifelse(is.nan(.data$pred.ratio),0,.data$pred.ratio),
                    age = gsub('age','',.data$age) %>% as.numeric()) %>%
      ggplot2::ggplot(ggplot2::aes(.data$age,.data$obs.ratio,col=.data$stock)) +
      ggplot2::geom_point(ggplot2::aes(pch=.data$stock)) +
      ggplot2::geom_line(ggplot2::aes(y=.data$pred.ratio,lty = .data$stock))+
      ggplot2::facet_wrap(~.data$year+.data$step) +
      ggplot2::labs(y='Stock prop.',x='Age',lty = 'Stock') +
      ggplot2::geom_label(
        data = x %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$year,.data$step) %>%
          dplyr::distinct() %>%
          dplyr::mutate(label=paste(.data$year,.data$step, sep=',')),
        ggplot2::aes(label=.data$label,group=1),
        x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1,
        inherit.aes = FALSE) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank())
  }

  ## Loop over stockdist names for plots
  unique(fit$stockdist$name) %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(function(x){
      
      dat <- fit$stockdist %>% dplyr::filter(.data$name == x)
      
      if (length(unique(dat$length)) > 1){
        
        if(is.null(stocks)) {
          lenplot(type = type, stocks = unique(fit$stockdist$stock),
                  geom_area = geom_area, color_palette = cols)
        } else {
          
          if(stocks == "separate") stocks <- unique(fit$stockdist$stock)
          
          legend <- cowplot::get_legend(
            lenplot(type = type, stocks = stocks, geom_area = geom_area,
                    color_palette = cols))
          
          cowplot::plot_grid(
            cowplot::plot_grid(
              plotlist = lapply(stocks, function(k) {
                lenplot(type = type, stocks = k, geom_area = geom_area,
                        color_palette = cols) +
                  ggplot2::theme(legend.position = "none") +
                  ggplot2::ggtitle(k)
              })
            ), legend, ncol = 1, rel_heights = c(1, .1)
          )
        }
      } else {
        fit$stockdist %>% lenageplot()
      }
    })
}
