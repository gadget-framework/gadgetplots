#' @title Plot parameter values relative to their boundaries
#' @inheritParams plot_annual
#' @param out_only Logical indicating whether only parameters outside their boundaries should be plotted.
#' @details The default plot is likely busy. Use \code{plotly::ggplotly()} to make it easier to interpret. If you are after parameters that are outside their boundaries, the \code{out_only} argument will help.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_param <- function(fit, out_only = FALSE, base_size = 8) {

  if(all(c("optimise", "lower", "upper") %in% names(fit$params))) {
    dat <- fit$params %>%
      dplyr::filter(.data$optimise == 1) %>%
      dplyr::mutate(rho = (.data$value-.data$lower)/(.data$upper - .data$lower))

    if(out_only) dat <- dat %>% dplyr::filter(.data$rho < 0 | .data$rho > 1)

    if(nrow(dat) > 0) {
      suppressWarnings({
        ggplot2::ggplot(
          data = dat,
          ggplot2::aes(.data$switch,.data$rho,label=.data$switch,
                       text = paste("value :", round(.data$value)))) +
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
