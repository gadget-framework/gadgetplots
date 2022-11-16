#' @title Plot annual ICES type of graphic returning central model data
#' @param fit A gadget fit object. See \code{\link[gadgetutils]{g3_fit}}.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @param color_palette A function defining the color palette to be used or a vector of colors which is 1 longer than the number of stocks in the model. The extra color will be used for total estimate. See \link[ggplot2]{scale_color_manual}.
#' @param ... Additional arguments passed to \code{plot_*} functions. See Details.
#' @return A \link[ggplot2]{ggplot} object. If \code{fleet = NULL}, a list of ggplot objects.
#' @examples
#' data(fit)
#' # Annual plot with custom colors
#' plot_annual(fit, color_palette = scales::brewer_pal(palette = "Spectral"))
#' @export

plot_annual <- function(fit, color_palette = scales::hue_pal(), base_size = 8, ...) {

  if(inherits(color_palette, "function")) {
    cols <- color_palette(length(unique(fit$res.by.year$stock)) + 1)
    names(cols) <- c(unique(fit$res.by.year$stock), "Total")
  } else {
    if(length(color_palette) != length(unique(fit$res.by.year$stock)) + 1) {
      stop("color_palette has to be a vector of 1 + number of stocks in the model or a color palette function")
    } else {
      cols <- color_palette
    }
  }

  p1 <- plot_catch(fit, base_size = base_size, ...) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("Catches") +
    ggplot2::scale_fill_manual(values = cols)
  p2 <- plot_rec(fit, base_size = base_size, ...) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("Recruitment") +
    ggplot2::scale_fill_manual(values = cols)
  p3 <- plot_f(fit, base_size = base_size, total = TRUE) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("F") +
    ggplot2::scale_color_manual(values = cols)
  p4 <- plot_biomass(fit, base_size = base_size, total = TRUE, ...) +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::ggtitle("Stock biomass") +
    ggplot2::scale_color_manual(values = cols)

  legend <- cowplot::get_legend(p4)

  p4 <- p4 + ggplot2::theme(legend.position = "none")

  cowplot::plot_grid(
    cowplot::plot_grid(p1, p2, p3, p4), legend, ncol = 1, rel_heights = c(1, .1)
  )
}
