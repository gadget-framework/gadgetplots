#' @title Plot catch distributions
#' @description Plots the length distribution of each \code{fit$catchdist.fleets$name}
#' @inheritParams plot_annual
#' @param type Character specifying the plot type: \code{"step"} uses stepwise lines to show the aggregation intervals. \code{"line"} uses lines through average values between lower and upper intervals.
#' @param name A character vector specifying the unique(fit$catchdist.fleets$name) to plot. If \code{NULL}, all names with relevant data are plotted.
#' @return A list of \link[ggplot2]{ggplot} objects consisting of a separate plot for each of \code{names}.
#' @examples
#' data(fit)
#' # Only one data set to save space:
#' plot_catchdist(fit, name = "EggaN_ldist")
#' plot_catchdist(fit, name = "EggaN_ldist", type = "line")
#' @export

# type = "step"; name = NULL; base_size = 8
plot_catchdist <- function(fit, type = "step", name = NULL, base_size = 8) {

  # Plot functions

  agelineplot <- function(dat) {
    dat %>%
      dplyr::group_by(.data$year,.data$step,.data$age) %>%
      dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                       observed = sum(.data$observed, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(.data$age,.data$predicted)) +
      ggplot2::geom_line(ggplot2::aes(.data$age,.data$observed),col='gray') +
      ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE #,
                          # ncol = max(2*length(unique(dat$step)),4)
      )  +
      ggplot2::geom_line() +
      ggplot2::geom_text(
        data = dat %>%
          dplyr::ungroup() %>%
          dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>%
          dplyr::filter(.data$age == min(.data$age)) %>%
          dplyr::mutate(y=Inf,
                        label = paste(.data$year,.data$step,sep=',')) %>%
          dplyr::select(.data$step,.data$age,.data$y,.data$year,.data$label) %>%
          dplyr::distinct(),
        ggplot2::aes(.data$age,.data$y,label=.data$label),
        vjust = 1.3,hjust = -.05,
        size = FS(base_size)*0.8,
        inherit.aes = FALSE) +
      ggplot2::labs(y='Proportion', x='Age') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())
  }

  agestepplot <- function(dat) {
    dat %>%
      dplyr::group_by(.data$year,.data$step,.data$age) %>%
      dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                       observed = sum(.data$observed, na.rm = TRUE),
                       .groups = "drop"
      ) %>%
      dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot() +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$age, y = .data$observed),
        color = "grey") +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$age, y = .data$predicted),
        color = "black") +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::labs(y = 'Proportion', x = 'Age') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  lenlineplot <- function(dat) {
    dat %>%
      ggplot2::ggplot(ggplot2::aes(.data$avg.length,.data$predicted)) +
      ggplot2::geom_line(ggplot2::aes(.data$avg.length,.data$observed),col='gray') +
      ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE #,
                          # ncol = max(2*length(unique(dat$step)),4)
      )  +
      ggplot2::geom_line() +
      ggplot2::geom_text(
        data = dat %>%
          dplyr::ungroup() %>%
          dplyr::filter(.data$avg.length == min(.data$avg.length)) %>%
          dplyr::mutate(y=Inf,
                        label = paste(.data$year,.data$step,sep=',')) %>%
          dplyr::select(.data$step,.data$avg.length,.data$y,.data$year,.data$label),

        ggplot2::aes(.data$avg.length,.data$y,label=.data$label),
        vjust = 1.3,hjust = -.05,
        size = FS(base_size)*0.8,
        inherit.aes = FALSE) +
      ggplot2::labs(y ='Proportion', x = 'Length') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())
  }

  lenstepplot <- function(dat) {

    dat %>% dplyr::select(-.data$upper) %>%
      dplyr::bind_rows(
        dat %>%
          dplyr::filter(.data$upper == max(.data$upper)) %>%
          dplyr::select(-.data$lower) %>%
          dplyr::rename("lower"= "upper")
      ) %>%
      dplyr::arrange(.data$year, .data$step, .data$lower) %>%
      ggplot2::ggplot() +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$lower, y = .data$observed),
        color = "grey") +
      ggplot2::geom_step(
        ggplot2::aes(x = .data$lower, y = .data$predicted),
        color = "black") +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::labs(y = 'Proportion', x = 'Length') +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  agelenstepplot <- function(dat) {
    dat %>%
      dplyr::group_by(.data$year, .data$length) %>%
      dplyr::mutate(
        total_obs = sum(.data$obs, na.rm = TRUE),
        total_pred = sum(.data$pred, na.rm = TRUE)) %>%
      dplyr::group_by(.data$year, .data$length, .data$age) %>%
      dplyr::mutate(
        obs_ratio = sum(.data$obs, na.rm = TRUE)/.data$total_obs,
        pred_ratio = sum(.data$pred, na.rm = TRUE)/.data$total_pred,
        diff_ratio = .data$obs_ratio - .data$pred_ratio,
        age = as.integer(gsub("age", "", .data$age))) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$age, y = .data$diff_ratio,
                                   color = .data$lower, group = factor(.data$lower))) +
      ggplot2::geom_step() +
      ggplot2::scale_color_viridis_c() +
      ggplot2::facet_wrap(~.data$year+.data$step,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
      ggplot2::labs(y = 'Difference between observed and predicted ratio',
                    x = 'Age', color = "Length bin") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(
        legend.position = "bottom",
        strip.background = ggplot2::element_blank()
      )
  }

  ## Data handling & loop

  if (is.null(fit$catchdist) || is.null(fit$catchdist.fleets)) {
    warning("No catchdist to plot")
    return(NULL)
  }

  if (!('stock_re' %in% names(fit$catchdist.fleets))) fit$catchdist.fleets$stock_re <- NA

  if(is.null(name)) name <- unique(fit$catchdist.fleets$name)

  rlang::set_names(name) %>%
    purrr::map(function(x){

      dat <- fit$catchdist.fleets %>%
        dplyr::filter(.data$name == x) %>%
        dplyr::ungroup()

      ## Plotting

      if(length(unique(dat$age)) == 1) {

        if(length(unique(dat$stock)) > 1) {

          cowplot::plot_grid(
            plotlist = lapply(unique(dat$stock), function(k) {
              suppressWarnings({
                # By age
                if(type == "line") {
                  p <- lenlineplot(dat %>% dplyr::filter(.data$stock == k))
                } else {
                  p <- lenstepplot(dat %>% dplyr::filter(.data$stock == k))
                }

                p +
                  ggplot2::theme(legend.position = "none") +
                  ggplot2::ggtitle(k)
              })
            })
          )

        } else if(length(unique(dat$stock_re)) > 1) {

          cowplot::plot_grid(
            plotlist = lapply(unique(dat$stock_re), function(k) {
              suppressWarnings({
                # By age
                if(type == "line") {
                  p <- lenlineplot(dat %>% dplyr::filter(.data$stock_re == k))
                } else {
                  p <- lenstepplot(dat %>% dplyr::filter(.data$stock_re == k))
                }

                p +
                  ggplot2::theme(legend.position = "none") +
                  ggplot2::ggtitle(k)
              })
            })
          )

        } else {
          if(type == "line") {
            lenlineplot(dat)
          } else {
            lenstepplot(dat)
          }
        }
      } else {

        if(length(unique(dat$stock)) > 1) {

          cowplot::plot_grid(
            plotlist = lapply(unique(dat$stock), function(k) {
              suppressWarnings({

                if(type == "line") {
                  p <- agelineplot(dat %>% dplyr::filter(.data$stock == k))
                } else {
                  p <- agestepplot(dat %>% dplyr::filter(.data$stock == k))
                }

                p +
                  ggplot2::theme(legend.position = "none") +
                  ggplot2::ggtitle(k)
              })
            })
          )

        } else if(length(unique(dat$stock_re)) > 1) {

          cowplot::plot_grid(
            plotlist = lapply(unique(dat$stock_re), function(k) {
              suppressWarnings({

                if(type == "line") {
                  p1 <- agelineplot(dat %>% dplyr::filter(.data$stock_re == k))
                  p2 <- lenlineplot(
                    dat %>%
                      dplyr::filter(.data$stock_re == k) %>%
                      dplyr::group_by(.data$year, .data$step, .data$stock_re,
                                      .data$lower, .data$upper, .data$avg.length) %>%
                      dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                                       observed = sum(.data$observed, na.rm = TRUE),
                                       .groups = "drop")
                  )
                } else if(type == "stratified_step") {
                  p1 <- agelenstepplot(dat %>% dplyr::filter(.data$stock_re == k)) +
                    ggplot2::ggtitle(k)
                } else {
                  p1 <- agestepplot(dat %>% dplyr::filter(.data$stock_re == k))
                  p2 <- lenstepplot(
                    dat %>%
                      dplyr::filter(.data$stock_re == k) %>%
                      dplyr::group_by(.data$year, .data$step, .data$stock_re,
                                      .data$lower, .data$upper, .data$avg.length) %>%
                      dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                                       observed = sum(.data$observed, na.rm = TRUE),
                                       .groups = "drop")
                  )
                }

                if(type != "stratified_step") {
                cowplot::plot_grid(
                  p1 +
                    ggplot2::theme(legend.position = "none") +
                    ggplot2::ggtitle(k),
                  p2, ncol = 1)
                } else {
                  p1
                }
              })
            })
          )

        } else {


          if(type == "line") {
            p1 <- agelineplot(dat)
            p2 <- lenlineplot(
              dat %>%
                dplyr::group_by(.data$year, .data$step, .data$stock_re,
                                .data$lower, .data$upper, .data$avg.length) %>%
                dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                                 observed = sum(.data$observed, na.rm = TRUE),
                                 .groups = "drop")
            )

            cowplot::plot_grid(p1, p2, ncol = 2)

            } else if(type == "stratified_step") {
              agelenstepplot(dat)
          } else {
            p1 <- agestepplot(dat)
            p2 <- lenstepplot(
              dat %>%
                dplyr::group_by(.data$year, .data$step, .data$stock_re,
                                .data$lower, .data$upper, .data$avg.length) %>%
                dplyr::summarise(predicted = sum(.data$predicted, na.rm = TRUE),
                                 observed = sum(.data$observed, na.rm = TRUE),
                                 .groups = "drop")
            )

            cowplot::plot_grid(p1, p2, ncol = 2)
          }
        }


      }
    })
}
