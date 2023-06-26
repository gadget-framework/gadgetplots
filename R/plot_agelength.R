#' @title Plot of fitted growth for each age-length component
#' @inheritParams plot_annual
#' @inheritParams plot_catchdist
#' @return A \link[ggplot2]{ggplot} object. A list of ggplot objects if there are multiple age-length (aldist) data sources (\code{unique(fit$catchdist.fleets$name)}).
#' @export

plot_agelength <- function(fit, name = NULL, base_size = 8) {

  ## gadget2 compatability
  if (!('stock' %in% names(fit$catchdist.fleets)) & 'stocknames' %in% names(fit$catchdist.fleets)){
    fit$catchdist.fleets <- fit$catchdist.fleets %>% dplyr::rename(stock = .data$stocknames)
  }
  
  ## Plot function

  agelenplot <- function(dat) {
    if(length(unique(dat$age))>1) {
      dat %>%
        ggplot2::ggplot(ggplot2::aes(.data$age,.data$o.ml)) +
        ggplot2::geom_ribbon(
          fill='gold',
          ggplot2::aes(ymax=.data$upper,ymin=.data$lower))+
        ggplot2::geom_point(size=1) +
        ggplot2::geom_line(ggplot2::aes(y=.data$p.ml))  +
        ggplot2::geom_linerange(
          ggplot2::aes(ymax=.data$o.upper,ymin=.data$o.lower)) +
        ggplot2::facet_wrap(
          ~.data$year+.data$step, drop = FALSE,
          # ncol = max(2*length(unique(dat$step)),4),
          labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
        ggplot2::labs(x='Age', y='Average length') +
        ggplot2::expand_limits(x = 0, y = 0) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(strip.background = ggplot2::element_blank())
    }
  }

  ## Components containing age data

  if(is.null(name)) {
    name <- unique(fit$catchdist.fleets$name)

    name <- name[sapply(name, function(x) {
      fit$catchdist.fleets %>%
        dplyr::filter(.data$name == x) %>%
        dplyr::pull(.data$age) %>%
        unique() %>%
        length() > 1
    })]
  }

  ## Plot loop
  if (!('stock_re' %in% names(fit$catchdist.fleets))) fit$catchdist.fleets$stock_re <- NA

  rlang::set_names(name) %>%
    purrr::map(function(x) {

      dat <- fit$catchdist.fleets %>%
        dplyr::filter(.data$name == x) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$year,.data$step,.data$age,.data$stock_re,.data$stock) %>%
        dplyr::mutate(o=.data$observed/sum(.data$observed,na.rm=TRUE),
                      p=.data$predicted/max(sum(.data$predicted),1e-14)) %>%
        dplyr::select(.data$year,.data$step,.data$age,.data$stock_re,.data$stock,
                      length = .data$avg.length,.data$observed,
                      .data$o,.data$predicted,.data$p) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(age=as.numeric(gsub('age','',.data$age))) %>%
        dplyr::group_by(.data$year,.data$step,.data$age,.data$stock_re,.data$stock) %>%
        dplyr::summarise(
          o.ml=sum(.data$o*.data$length,na.rm=TRUE),
          o.sl=sqrt(sum(.data$o*(.data$length - .data$o.ml)^2,na.rm=TRUE)),
          p.ml=sum(.data$p*.data$length),
          p.sl=sqrt(sum(.data$p*(.data$length - .data$p.ml)^2)),
          .groups= "drop") %>%
        dplyr::mutate(o.ml=ifelse(.data$o.ml==0,NA,.data$o.ml),
                      o.sl=ifelse(.data$o.sl==0,NA,.data$o.sl),
                      upper = .data$p.ml+1.96*.data$p.sl,
                      lower = .data$p.ml-1.96*.data$p.sl,
                      o.upper = .data$o.ml+1.96*.data$o.sl,
                      o.lower = .data$o.ml-1.96*.data$o.sl) %>%
        dplyr::mutate(p.ml = dplyr::na_if(.data$p.ml, 0),
                      upper = dplyr::na_if(.data$upper, 0),
                      lower = dplyr::na_if(.data$lower, 0)) %>%
        dplyr::filter(!is.na(.data$p.ml))

      if(length(unique(dat$stock)) > 1) {

        cowplot::plot_grid(
          plotlist = lapply(unique(dat$stock), function(k) {
            suppressWarnings({
              agelenplot(dat %>% dplyr::filter(.data$stock == k)) +
                ggplot2::coord_cartesian(
                  ylim = c(0,ceiling(max(c(dat$upper, dat$o.upper), na.rm = TRUE))),
                  xlim = c(0, max(dat$age) + 1),
                  expand = FALSE) +
                ggplot2::theme(legend.position = "none") +
                ggplot2::ggtitle(k)
            })
          })
        )

      } else if(length(unique(dat$stock_re)) > 1) {

        cowplot::plot_grid(
          plotlist = lapply(unique(dat$stock_re), function(k) {
            suppressWarnings({
              agelenplot(dat %>% dplyr::filter(.data$stock_re == k)) +
                ggplot2::coord_cartesian(
                  ylim = c(0,ceiling(max(c(dat$upper, dat$o.upper), na.rm = TRUE))),
                  xlim = c(0, max(dat$age) + 1),
                  expand = FALSE) +
                ggplot2::theme(legend.position = "none") +
                ggplot2::ggtitle(k)
            })
          })
        )

      } else {
        suppressWarnings({
          agelenplot(dat) +
            ggplot2::coord_cartesian(
              ylim = c(0,ceiling(max(c(dat$upper, dat$o.upper), na.rm = TRUE))),
              xlim = c(0, max(dat$age) + 1),
              expand = FALSE)
        })
      }

    })
}

