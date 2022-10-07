#' @title Plot parameter weights
#' @inheritParams plot_annual
#' @param log_scale Logical indicating whether the axis should be log10 transformed.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_weight <- function(fit, log_scale = FALSE, base_size = 8) {

  fit$params %>%
    dplyr::filter(grepl("weight$", .data$switch)) %>%
    dplyr::mutate(switch = gsub("_weight$", "", .data$switch)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$switch, y = .data$value)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "", y = "Parameter weight") +
    ggplot2::coord_flip() + {
      if(log_scale) ggplot2::scale_y_log10()
    } +
    ggplot2::theme_bw(base_size = base_size)

}
