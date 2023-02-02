#' @title Plot exponentiall50 suitability using model parameters
#' @inheritParams plot_annual
#' @param length Numeric vector defining the lengths for which suitabilities should be plotted
#' @param alpha (Named) numeric vector defining the exponentiall50 slopes. If length > 1, multiple curves will be plotted using a different line type
#' @param l50 Numeric vector defining the exponentiall50 l50 values. Must be same length than \code{alpha}
#' @param add Logical indicating whether the function should produce a ggplot2 layer which can be added to an existing ggplot (\code{TRUE}) or whether to plot the results (\code{FALSE})
#' @param ... Additional arguments passed to \code{geom_line}
#' @export

g3plot_exponentiall50 <- function(length, alpha, l50, add = FALSE, base_size = 8, ...) {

  if(length(alpha) != length(l50)) stop("alpha and l50 has to have equal length.")

  out <- lapply(seq_along(alpha), function(i) {
    data.frame(
      model = ifelse(!is.null(names(alpha)), names(alpha)[i], paste("expl50", i, sep = "_")),
      length=length,
      y=eval(gadget3::g3_suitability_exponentiall50(alpha[i],l50[i])[[2]], list(stock__midlen=length))
    )
  }) %>% dplyr::bind_rows()

  if(!add) {

    ggplot2::ggplot(out, ggplot2::aes(.data$length,.data$y)) + {
      if(length(unique(out$model)) > 1) ggplot2::geom_line(ggplot2::aes(lty = .data$model))
    } + {
      if(length(unique(out$model)) == 1) ggplot2::geom_line()
    } +
      ggplot2::labs(x = "Length", y = "Suitability") +
      ggplot2::theme_classic(base_size = base_size)

  } else {

    if(length(unique(out$model)) > 1) {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y, lty = .data$model),
        inherit.aes = FALSE, ...)
    } else {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y),
        inherit.aes = FALSE, ...)
    }

  }
}
