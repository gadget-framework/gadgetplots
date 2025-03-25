#' @title Plot suitability
#' @inheritParams plot_annual
#' @param fleets Character vector specifying the fleets to plot in \code{fit}. If \code{NULL}, all fleets will be plotted in separate plots. Use \code{"all"} to plot all fleets to the same plot.
#' @param add_models Logical indicating whether models using suitability parameters should be plotted together with the suitabilities estimated from data. The name of the parameter has to be similar to the fleet name. Uses grep and does not always work.
#' @param include_missing Logical indicating whether years with missing catch data should be plotted.
#' @return A \link[ggplot2]{ggplot} object. If \code{fleet = NULL}, a list of ggplot objects.
#' @examples
#' data(fit)
#' plot_suitability(fit)
#' plot_suitability(fit, fleets = "EggaN_survey",
#'                  include_missing = FALSE)
#' @export

plot_suitability <- function(fit, fleets = "all", add_models = TRUE, include_missing = TRUE, base_size = 8) {

  if(is.null(fleets)) {

    rlang::set_names(unique(fit$suitability$fleet)) %>%
      purrr::map(function(x){

        dat <- fit$suitability %>%
          dplyr::filter(.data$fleet == x)

        if(!include_missing) {
          include_years <- dat %>%
            dplyr::group_by(.data$year) %>%
            dplyr::summarise(suit = sum(.data$suit)) %>%
            dplyr::filter(.data$suit > 0) %>%
            dplyr::pull(.data$year)

          dat <- dat %>% dplyr::filter(.data$year %in% include_years)
        }

        nm <- gsub("_survey|_fishery", "", unique(dat$fleet))

        if(
          add_models &
          length(grep("l50", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE)) > 0 &
          length(grep("alpha", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE)) > 0 &
          length(grep("l50", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE)) ==
          length(grep("alpha", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE))
        ) {
          expl50suit <- TRUE
          alpha <- unlist(fit$params$value[fit$params$switch %in% grep("alpha", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE)])
          names(alpha) <- gsub("\\.alpha", "", names(alpha))
          l50 <- unname(unlist(fit$params$value[fit$params$switch %in% grep("l50", grep(nm, fit$params$switch, ignore.case = TRUE, value = TRUE), value = TRUE)]))
        } else {
          expl50suit <- FALSE
        }

        ggplot2::ggplot(
          data = dat,
          ggplot2::aes(.data$length,.data$suit, color = .data$stock)) +
          {
            if(expl50suit) g3plot_exponentiall50(
              length = sort(unique(dat$length)),
              alpha = alpha,
              l50 = l50,
              add = TRUE
            )
          } +
          ggplot2::geom_line(linewidth = base_size/16, alpha = 0.7) +
          ggplot2::labs(y='Suitability',x='Length', color = 'Stock') +
          if (!(all(is.na(dat$year)) && all(is.na(dat$step)))){
            ggplot2::facet_wrap(~.data$year + .data$step) +
              ggplot2::geom_text(
                data = dat %>%
                  dplyr::ungroup() %>%
                  dplyr::select(.data$year,.data$step) %>%
                  dplyr::mutate(y=Inf,
                                label = paste(.data$year,.data$step,sep=',')) %>%
                  dplyr::select(.data$step,.data$y,.data$year,.data$label) %>%
                  dplyr::distinct(),
                ggplot2::aes(-Inf,Inf,label=.data$label),
                vjust = 1.3,hjust = -.05,
                size = FS(base_size)*0.8,
                inherit.aes = FALSE) +
              ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                                  ncol = max(2*length(unique(fit$suitability$step)),4))
          } + 
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.spacing = ggplot2::unit(0,'cm'),
            plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_blank()
          )
      })

  } else {

    if(fleets == "all") fleets <- unique(fit$suitability$fleet)

    dat <- fit$suitability %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$fleet %in% fleets)

    if(!include_missing) {
      include_years <- dat %>%
        dplyr::group_by(.data$year) %>%
        dplyr::summarise(suit = sum(.data$suit)) %>%
        dplyr::filter(.data$suit > 0) %>%
        dplyr::pull(.data$year)

      dat <- dat %>% dplyr::filter(.data$year %in% include_years)
    }

    ggplot2::ggplot(
      data = dat,
      ggplot2::aes(.data$length,.data$suit, color = .data$stock)) + {
        if(length(fleets) > 1) ggplot2::geom_line(ggplot2::aes(lty = .data$fleet), size = base_size/16)
      } + {
        if(length(fleets) == 1) ggplot2::geom_line(size = base_size/16)
      } +
      ggplot2::labs(y='Suitability',x='Length', color = 'Stock', if(length(fleets) > 1) {lty = 'Fleet'}) +
      if (!all(is.na(dat$year)) && all(is.na(dat$step))){
        ggplot2::facet_wrap(~.data$year + .data$step) +
          ggplot2::geom_text(
            data = dat %>%
              dplyr::ungroup() %>%
              dplyr::select(.data$year,.data$step) %>%
              dplyr::mutate(y=Inf,
                            label = paste(.data$year,.data$step,sep=',')) %>%
              dplyr::select(.data$step,.data$y,.data$year,.data$label) %>%
              dplyr::distinct(),
            ggplot2::aes(-Inf,Inf,label=.data$label),
            vjust = 1.3,hjust = -.05,
            size = FS(base_size)*0.8,
            inherit.aes = FALSE) +
          ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                              ncol = max(2*length(unique(fit$suitability$step)),4))  
      } +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())


  }
}
