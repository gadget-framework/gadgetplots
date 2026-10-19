#' @title Plot suitability
#' @inheritParams plot_annual
#' @param fleets Character vector specifying the fleets to plot in \code{fit}. If \code{NULL}, all fleets will be plotted in separate plots. Use \code{"all"} to plot all fleets to the same plot.
#' @param add_models Logical indicating whether models using suitability parameters should be plotted together with the suitabilities estimated from data. The name of the parameter has to be similar to the fleet name. Uses grep and does not always work.
#' @param include_missing Logical indicating whether years with missing catch data should be plotted.
#' @details Fleets whose suitability depends on age (the suitability table has non-missing ages, e.g. a selectivity defined by age rather than length) are plotted against age instead of length, since such a fleet has one suitability value per age at every length. With \code{fleets = "all"} or several fleets, length- and age-based fleets cannot share an axis: if both are selected, the age-based ones are left out with a message.
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
        
        # Age-dependent suitability: one value per age at every length, plot against age
        by_age <- suit_by_age(dat)
        if(by_age) dat <- suit_age_data(dat)
        
        if(!include_missing) {
          include_years <- dat %>%
            dplyr::group_by(.data$year) %>%
            dplyr::summarise(suit = sum(.data$suit)) %>%
            dplyr::filter(.data$suit > 0) %>%
            dplyr::pull(.data$year)
          
          dat <- dat %>% dplyr::filter(.data$year %in% include_years)
        }
        
        nm <- gsub("_survey|_fishery", "", unique(dat$fleet))
        
        # Parameters of this fleet only: the fleet name is a whole dot-separated part of the
        # parameter name (e.g. "Int.reb.l50", "ghl_female.trawlnor.l50"). A plain substring
        # search would also match other fleets (e.g. "Int" in "WinterS.reb.l50")
        fleet_switches <- grep(paste0("(^|\\.)", nm, "(\\.|$)"), fit$params$switch, ignore.case = TRUE, value = TRUE)
        alpha_switches <- grep("\\.alpha$", fleet_switches, value = TRUE)
        l50_switches <- grep("\\.l50$", fleet_switches, value = TRUE)

        if(
          add_models &
          !by_age &
          length(l50_switches) > 0 &
          length(alpha_switches) == length(l50_switches)
        ) {
          expl50suit <- TRUE
          alpha <- unlist(fit$params$value[match(alpha_switches, fit$params$switch)])
          names(alpha) <- gsub("\\.alpha$", "", alpha_switches)
          l50 <- unname(unlist(fit$params$value[match(l50_switches, fit$params$switch)]))
        } else {
          expl50suit <- FALSE
        }
        
        ggplot2::ggplot(
          data = dat,
          ggplot2::aes(if(by_age) .data$age else .data$length, .data$suit, color = .data$stock)) +
          {
            if(expl50suit) g3plot_exponentiall50(
              length = sort(unique(dat$length)),
              alpha = alpha,
              l50 = l50,
              add = TRUE
            )
          } +
          ggplot2::geom_line(linewidth = base_size/16, alpha = 0.7) +
          ggplot2::labs(y='Suitability', x = if(by_age) 'Age' else 'Length', color = 'Stock') +
          {
            if (!(all(is.na(dat$year)) && all(is.na(dat$step)))){
              # NB: A list, ggplot2 (>= 4.0) can't add two layers together before they are added to the plot
              list(ggplot2::geom_text(
                data = dat %>%
                  dplyr::ungroup() %>%
                  dplyr::select(.data$year,.data$step) %>%
                  dplyr::mutate(y=Inf,
                                label = paste(.data$year,.data$step,sep=',')) %>%
                  dplyr::select(.data$step,.data$y,.data$year,.data$label) %>%
                  dplyr::distinct(),
                ggplot2::aes(-Inf,Inf,label=.data$label),
                vjust = 1.3, hjust = -.05,
                size = FS(base_size)*0.8,
                inherit.aes = FALSE),
                ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                                    ncol = max(2*length(unique(fit$suitability$step)),4)))
            }
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
    
    # Length- and age-based fleets can't share the x-axis: plot by age only if all fleets are age-based
    age_fleets <- if("age" %in% names(dat)) unique(dat$fleet[!is.na(dat$age)]) else character(0)
    by_age <- length(age_fleets) > 0 && all(fleets %in% age_fleets)
    if(by_age) {
      dat <- suit_age_data(dat)
    } else if(length(age_fleets) > 0) {
      message("Suitability of ", paste(age_fleets, collapse = ", "), " depends on age, not plotted with the length-based fleets. Plot it separately.")
      fleets <- setdiff(fleets, age_fleets)
      dat <- dat %>% dplyr::filter(!.data$fleet %in% age_fleets)
    }
    
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
      ggplot2::aes(if(by_age) .data$age else .data$length, .data$suit, color = .data$stock)) + {
        if(length(fleets) > 1) ggplot2::geom_line(ggplot2::aes(linetype = .data$fleet,), linewidth = base_size/16)
      } + {
        if(length(fleets) == 1) ggplot2::geom_line(linewidth = base_size/16)
      } +
      ggplot2::labs(y='Suitability', x = if(by_age) 'Age' else 'Length', color = 'Stock', linetype = if(length(fleets) > 1) 'Fleet' else NULL) + 
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank()) +
      if (!(all(is.na(dat$year)) && all(is.na(dat$step)))) {
        list(ggplot2::geom_text(
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
          inherit.aes = FALSE),
          ggplot2::facet_wrap(~.data$year + .data$step, drop = FALSE,
                              ncol = max(2*length(unique(fit$suitability$step)),4)) 
        )
      }
  }
}

# Whether a suitability table has age-dependent suitability (non-missing ages)
suit_by_age <- function(dat) {
  "age" %in% names(dat) && any(!is.na(dat$age))
}

# Age-dependent suitability: the same value is repeated over all lengths. One row per age
suit_age_data <- function(dat) {
  cols <- intersect(c("stock", "fleet", "age", "suit", "year", "step"), names(dat))
  dat <- dat[!is.na(dat$age), cols]
  dat[!duplicated(dat), ]
}
