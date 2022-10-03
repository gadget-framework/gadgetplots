#' @title Plot annual ICES type of graphic returning central model data
#' @param fit A gadget fit object. See \code{\link[gadgetutils]{g3_fit}}.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @param ... Additional arguments passed to \code{plot_*} functions. See Details.
#' @return A \link[ggplot2]{ggplot} object. If \code{fleet = NULL}, a list of ggplot objects.
#' @import gadgetutils
#' @export

plot_annual <- function(fit, base_size = 8, ...) {

  p1 <- plot_catch(fit, base_size = base_size, ...) +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::ggtitle("Catches")
  p2 <- plot_rec(fit, base_size = base_size, ...) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("Recruitment")
  p3 <- plot_f(fit, base_size = base_size, ...) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("F")
  p4 <- plot_biomass(fit, base_size = base_size, ..., geom_area = TRUE) +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::ggtitle("Stock biomass")

  legend <- cowplot::get_legend(p1)

  p1 <- p1 + ggplot2::theme(legend.position = "none")

  cowplot::plot_grid(
    cowplot::plot_grid(p1, p2, p3, p4), legend, ncol = 1, rel_heights = c(1, .1)
  )
}
