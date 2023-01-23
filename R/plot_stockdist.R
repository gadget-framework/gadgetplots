#' @title Plot stock distribution
#' @description Plots proportions of stocks by length or age in \code{fit$stockdist}
#' @inheritParams plot_annual
#' @param stocks Character vector specifying the stock to plot in \code{fit}. If \code{NULL}, all stocks are plotted in a single plot. If \code{"separate"}, all stocks are plotted in separate plots.
#' @param component_name Character vector specifying the likelihood component to plot in \code{unique(fit$stockdist$name)}. If \code{NULL}, all components are plotted as a list. .
#' @param color_palette A function defining the color palette or a vector of colors to be used for stocks.
#' @details Plots model fit to data using likelihoods. Separate plot for each \code{unique(fit$stockdist$name)}.
#' @return A \link[ggplot2]{ggplot} object or a list of such objects depending on the \code{stocks} argument.
#' @export

# Dev params
# stocks = NULL; component_name = NULL; color_palette = scales::hue_pal(); base_size = 8
plot_stockdist <- function(fit, stocks = NULL, component_name = NULL, color_palette = scales::hue_pal(), base_size = 8) {

  ## Check whether fit contains stockdist
  if (is.null(fit$stockdist)) {
    warning('The fit object does not contain a stockdist object')
    return(NULL)
  }

  all_stocks <- c(stats::na.exclude(unique(fit$stockdist$stock)))
  if(!is.null(fit$stockdist$stock_re)) {
    all_stock_res <- rev(c(stats::na.exclude(unique(fit$stockdist$stock_re))))
  } else {
    all_stock_res <- NULL
  }

  if(inherits(color_palette, "function")) {

    cols <- stats::setNames(color_palette(length(c(all_stocks, all_stock_res))),
                            c(all_stocks, all_stock_res))
  } else {
    if(length(color_palette) != length(c(all_stocks, all_stock_res))) {
      stop("color_palette has to be a vector with a same length than the number of stocks in the model or a color palette function")
    } else {
      cols <- color_palette
    }
  }

  lenplot <- function(stocks = NULL, stockdist_name = component_name, color_palette) {

  if(is.null(stockdist_name)) stockdist_name <- unique(fit$stockdist$name)[1]

      x <- fit$stockdist %>%
        #dplyr::filter(.data$predicted > 1e-4) %>%
        dplyr::filter(.data$name == stockdist_name) %>%
        dplyr::mutate(
          pred.ratio =
            ifelse(.data$pred.ratio == 0, NA,
                   ifelse(is.nan(.data$pred.ratio), 0, .data$pred.ratio)),
          obs.ratio = ifelse(is.na(.data$pred.ratio), NA, .data$obs.ratio)
        )

      if (all(is.na(x$stock_re)) & all(!is.na(x$stock))) {
        if(is.null(stocks)) stocks <- c(stats::na.exclude(unique(fit$stockdist$stock)))
        x <- x %>% dplyr::filter(.data$stock %in% stocks)
        stock_label <- "Stock"
      } else if (all(!is.na(x$stock_re)) & all(is.na(x$stock))) {
        if(is.null(stocks)) stocks <- c(stats::na.exclude(unique(fit$stockdist$stock_re)))
        x <- x %>%
          dplyr::filter(.data$stock_re %in% stocks) %>%
          dplyr::select(-.data$stock) %>%
          dplyr::rename("stock" = "stock_re")
        stock_label <- "Stock component"
      } else {
        stop("Both stock and stock_re in the same data. Don't know what to do.")
      }

      x %>%
        ggplot2::ggplot(
          ggplot2::aes(x = .data$length, y = .data$obs.ratio,
                       lty = .data$stock # shape = .data$stock, color = .data$stock
          )) +
        ggplot2::geom_line(color = "grey") +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$pred.ratio, lty = .data$stock), color = "black") +
        # ggplot2::geom_point(size = base_size/16) +
        ggplot2::facet_wrap(~.data$year+.data$step,
                            labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
        ggplot2::labs(y = 'Stock proportion', x = 'Length', lty = stock_label
                      #color = stock_label, shape = stock_label
        ) +
        # ggplot2::scale_color_manual(values = cols) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom",
                       strip.background = ggplot2::element_blank())
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
  if(is.null(component_name)) {
    component_name <- unique(fit$stockdist$name)
  }

  component_name %>%
    purrr::set_names(.,.) %>%
    purrr::map(function(x) {

      dat <- fit$stockdist %>% dplyr::filter(.data$name == x)

      if (length(unique(dat$length)) > 1){

        if(is.null(stocks)) {

          lenplot(stocks = NULL, stockdist_name = x, color_palette = cols)

        } else {

          if(stocks == "separate") {
            if (all(is.na(dat$stock_re)) & all(!is.na(dat$stock))) {
              stocks <- c(stats::na.exclude(unique(fit$stockdist$stock)))
            } else if (all(!is.na(dat$stock_re)) & all(is.na(dat$stock))) {
              stocks <- c(stats::na.exclude(unique(fit$stockdist$stock_re)))
            } else {
              stop("Both stock and stock_re in the same data. Don't know what to do.")
            }
          }

          # legend <- cowplot::get_legend(
          #   lenplot(stocks = stocks, stockdist_name = x, color_palette = cols))

          #cowplot::plot_grid(
            cowplot::plot_grid(
              plotlist = lapply(stocks, function(k) {
                lenplot(stocks = k, stockdist_name = x, color_palette = cols) +
                  ggplot2::theme(legend.position = "none") +
                  ggplot2::ggtitle(k)
              })
           # ), legend, ncol = 1, rel_heights = c(1, .1)
          )
        }
      } else {
        fit$stockdist %>% lenageplot()
      }
    })

}

