#' @title Plot likelihood summary
#' @inheritParams plot_annual
#' @param type Character specifying the plot type. Options: \code{"direct"}, \code{"weighted"} or \code{"sums"}. See Details.
#' @details Possible plot types are:
#' \describe{
#'   \item{direct}{Default value, plots direct comparisons of data with model
#'   output.}
#'   \item{weighted}{Plots the weighted likelihood value for each component.}
#'   \item{bat}{Plots the sums of weighted likelihood values by component.}
#'   }
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_likelihood <- function(fit, type = "direct", base_size = 8) {

  x <- fit$likelihood %>%
    dplyr::mutate(value = ifelse(is.na(.data$num), .data$wgt, .data$num))

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
    x %>%
      dplyr::group_by(.data$component) %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::summarise(val = sum(.data$value*.data$weight)) %>%
      dplyr::filter(!is.na(.data$val), .data$val > 0) %>%
      dplyr::ungroup() %>%
      # dplyr::mutate(val = 100*.data$val/sum(.data$val)) %>%
      ggplot2::ggplot(ggplot2::aes(x=.data$component,y=.data$val)) +
      # ggplot2::ggplot(ggplot2::aes(x="",y=.data$val,fill = .data$component)) +
      ggplot2::geom_col(fill = "grey", color = "black") +
      ggplot2::labs(x = "Component", y = "Summed likelihood score") +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::coord_flip() +
      # ggplot2::geom_bar(stat='identity',width = 1) +
      # ggplot2::coord_polar("y",start = 0) +
      # ggplot2::geom_text(ggplot2::aes(label = paste0(round(.data$val, 0), "%")),
      #                    position = ggplot2::position_stack(vjust = 0.5)) +
      # ggplot2::scale_fill_brewer("Component (proportion\nof weighted score)",
      #                            palette="Spectral") +
      #ggplot2::theme_void(base_size = base_size)
      ggplot2::theme_classic(base_size = base_size)
  }
}
