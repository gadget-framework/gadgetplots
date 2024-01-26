#' @title Plot parameter values relative to their boundaries
#' @inheritParams plot_annual
#' @param fit A gadget fit object or a data frame containing model parameters. See \code{\link[gadgetutils]{g3_fit}}.
#' @param out_only Logical indicating whether only parameters outside their boundaries should be plotted.
#' @details The default plot is likely busy. Use \code{plotly::ggplotly()} to make it easier to interpret. If you are after parameters that are outside their boundaries, the \code{out_only} argument will help.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(fit)
#' plot_param(fit)
#' plot_param(fit, out_only = TRUE)
#' @export

plot_param <- function(fit, out_only = FALSE, base_size = 8) {

  if(inherits(fit, "gadget.fit")) {
    dat <- fit$params
  } else if(inherits(fit, "data.frame")) {
    dat <- fit
  } else {
    stop("Supply either a gadget.fit object or a data.frame with model parameters.")
  }

  if(all(c("optimise", "lower", "upper") %in% names(dat))) {
    if (is.list(dat$value)) dat$value <- unlist(dat$value)
    dat <- dat %>%
      dplyr::filter(.data$optimise == 1) %>%
      dplyr::mutate(rho = (.data$value-.data$lower)/(.data$upper - .data$lower))

    if(out_only) dat <- dat %>% dplyr::filter(.data$rho < 0 | .data$rho > 1)

    if(nrow(dat) > 0) {
      suppressWarnings({
        ggplot2::ggplot(
          data = dat,
          ggplot2::aes(.data$switch,.data$rho,
                       text = paste("value:", round(.data$value,3),
                                    "\nlower:", round(.data$lower,2),
                                    "\nupper:", round(.data$upper,2)))) +
          ggplot2::geom_point() +
          ggplot2::coord_flip() +
          ggplot2::labs(x = "", y = "Rho") +
          ggplot2::geom_hline(yintercept = c(0,1)) +
          ggplot2::theme_bw(base_size = base_size)
      })
    } else {

      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = paste0(ifelse(out_only, "No parameters outside\ntheir boundaries", "No parameters to plot"))) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "", y = "Rho") +
        ggplot2::theme_bw(base_size = base_size) +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    }
  } else {
    warning("Did not find optimise, lower and upper columns in fit$param. Bug in g3_fit?")
    return(NULL)
  }
}
