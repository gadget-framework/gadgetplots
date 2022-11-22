#' @title Plot of survey indices
#' @inheritParams plot_annual
#' @param type Character specifying the plot type: \code{"direct"} plots the model fit to survey data by year, and \code{"scatter"} produces a x-y scatter plot for the fitted and observed index.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_si <- function(fit, base_size = 8, type = "direct") {

  x <- fit$sidat %>%
    dplyr::mutate(
      name = paste0(
        gsub("surveyindices\\.", "", .data$name), " (", .data$length, ")"))
  
  ## Covert year to year+step if multiple steps exist
  steps <- unique(x$step)
  if (length(steps) > 1){
    x <- 
      x %>% 
      mutate(year = year + (step-1)/length(steps))
  }

  if(type == "scatter") {

    x %>%
      ggplot2::ggplot(ggplot2::aes(.data$observed,.data$predicted,label=.data$year)) +
      ggplot2::geom_text(size = 0.8*FS(base_size)) +
      ggplot2::facet_wrap(~.data$name,scales = 'free') +
      ggplot2::geom_abline(slope = 1, lty = 2) +
      ggplot2::labs(y='Predicted value', x='Observed') +
      # ggplot2::geom_hline(data = dplyr::filter(x,.data$year==max(.data$year)),
      #                     ggplot2::aes(yintercept=.data$predicted),col='green') +
      # ggplot2::geom_vline(data=dplyr::filter(x,.data$year==max(.data$year)),
      #                     ggplot2::aes(xintercept=.data$predicted),col='green') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())

  } else {

    ggplot2::ggplot(x, ggplot2::aes(.data$year,.data$observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(.data$year,.data$predicted)) +
      ggplot2::geom_linerange(
        data = x %>% dplyr::filter(.data$year==max(.data$year)),
        ggplot2::aes(x= .data$year, ymax=.data$observed, ymin=.data$predicted),
        col='green') +
      ggplot2::facet_wrap(~.data$name,scales = 'free_y',ncol=2) +
      ggplot2::labs(y='Index', x='Year') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank())
  }
}
