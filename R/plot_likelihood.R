#' @title Plot likelihood summary
#' @inheritParams plot_annual
#' @param type Character specifying the plot type. Options: \code{"direct"}, \code{"weighted"} or \code{"total"}. See Details.
#' @details Possible plot types are:
#' \describe{
#'   \item{direct}{Plots direct comparisons of data with model output.}
#'   \item{weighted}{Plots the weighted likelihood value for each component.}
#'   \item{total (or any other string)}{Plots the sums of weighted and raw likelihood values by component.}
#'   }
#' @param log_scale Logical indicating whether the value axis should be log10 transformed.
#' @param use_proportions Logical indicating whether proportions of summed likelihood scores should be used instead of summed likelihood score values when \code{type = "total"}.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_likelihood <- function(fit, type = "total", log_scale = FALSE, use_proportions = TRUE, base_size = 8) {

  if(exists('wgt',fit$likelihood)){
  x <- fit$likelihood %>%
    dplyr::mutate(value = ifelse(is.na(.data$num), .data$wgt, .data$num))
  } else(
    x <- fit$likelihood %>% 
      dplyr::mutate(value =.data$num)
  )

  if(type == "direct") {
    x %>%
      dplyr::filter(.data$year!='all') %>%
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      dplyr::filter(!is.na(.data$value), .data$value > 0) %>%
      ggplot2::ggplot(ggplot2::aes(.data$year, .data$value)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~.data$component,scales = 'free_y') +
      ggplot2::labs(x='Year',y='Score') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  } else if(type == "weighted") {
    x %>%
      dplyr::filter(.data$year!='all') %>%
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      dplyr::mutate(val = .data$weight*.data$value) %>%
      dplyr::filter(!is.na(.data$val), .data$val > 0) %>%
      ggplot2::ggplot(ggplot2::aes(.data$year, .data$val)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~.data$component,scales = 'free_y') +
      ggplot2::labs(x='Year',y='Weighted score') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  } else {
    tmp <- x %>%
      dplyr::group_by(.data$component) %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::summarise(
        weighted_score = sum(.data$value*.data$weight),
        raw_score = sum(.data$value)
      ) %>%
      dplyr::filter(!is.na(.data$weighted_score), .data$weighted_score > 0) %>% # Remove components that have been explicitly removed by setting weight to 0
      dplyr::ungroup() %>%
      tidyr::pivot_longer(cols = c("weighted_score", "raw_score")) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::mutate(prop = .data$value/sum(.data$value))


    ggplot2::ggplot(tmp) + {
      if(use_proportions)
        ggplot2::geom_col(
          ggplot2::aes(x=.data$component, y=.data$prop, fill = .data$name),
          position = ggplot2::position_dodge())
    } + {
      if(!use_proportions)
        ggplot2::geom_col(
          ggplot2::aes(x=.data$component, y=.data$value, fill = .data$name),
          position = ggplot2::position_dodge())
    } + {
      if(log_scale) ggplot2::scale_y_log10(expand = c(0, 0))
    } + {
      if(!log_scale) ggplot2::scale_y_continuous(expand = c(0, 0))
    } +
      ggplot2::labs(
        x = "Component",
        y = ifelse(use_proportions,
                   "Proportion of summed likelihood score",
                   "Summed likelihood score"),
        subtitle =
          paste("Total summed score:",
                round(sum(tmp[tmp$name == "raw_score", "value"]), 1),
                "/",
                round(sum(tmp[tmp$name == "weighted_score", "value"]), 1),
                "(raw / weighted)")
      ) +
      ggplot2::scale_fill_hue("Score type", breaks = c("raw_score", "weighted_score"), labels = c("Raw", "Weighted")) +
      ggplot2::coord_flip() +
      ggplot2::theme_classic(base_size = base_size)
  }
}
