#' @title Plot residuals for catch distributions
#' @description Produces a residual plot for each \code{catchdist.fleets} component.
#' @inheritParams plot_annual
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(fit)
#' plot_resid(fit)
#' @export

plot_resid <- function(fit, base_size = 8) {
  if (is.null(fit$catchdist) || is.null(fit$catchdist.fleets)) {
    warning("No catchdist to plot")
    return(NULL)
  }

  ggplot2::ggplot(fit$catchdist.fleets,
                  ggplot2::aes(.data$lower, .data$observed - .data$predicted,
                               group=round(.data$lower,1))) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~.data$name)  +
    ggplot2::labs(y='Residual', x='Length') +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
}
