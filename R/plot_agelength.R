#' @title Plot of fitted growth for each age-length component
#' @inheritParams plot_annual
#' @inheritParams plot_catchdist
#' @return A \link[ggplot2]{ggplot} object. A list of ggplot objects if there are multiple age-length (aldist) data sources (\code{unique(fit$catchdist.fleets$name)}).
#' @export

plot_agelength <- function(fit, name = NULL, base_size = 8) {

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

  rlang::set_names(name) %>%
    purrr::map(function(x) {
      dat <- fit$catchdist.fleets %>%
        dplyr::filter(.data$name == x)
      if(length(unique(dat$age))>1) {
        dat %>%
          dplyr::group_by(.data$year,.data$step,.data$age) %>%
          dplyr::mutate(o=.data$observed/sum(.data$observed,na.rm=TRUE),
                        p=.data$predicted/max(sum(.data$predicted),1e-14)) %>%
          dplyr::select(.data$year,.data$step,.data$age,
                        length = .data$avg.length,.data$observed,
                        .data$o,.data$predicted,.data$p) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(age=as.numeric(gsub('age','',.data$age))) %>%
          dplyr::group_by(.data$year,.data$step,.data$age) %>%
          dplyr::summarise(
            o.ml=sum(.data$o*.data$length,na.rm=TRUE),
            o.sl=sqrt(sum(.data$o*(.data$length - .data$o.ml)^2,na.rm=TRUE)),
            p.ml=sum(.data$p*.data$length),
            p.sl=sqrt(sum(.data$p*(.data$length - .data$p.ml)^2))) %>%
          dplyr::mutate(o.ml=ifelse(.data$o.ml==0,NA,.data$o.ml),
                        o.sl=ifelse(.data$o.sl==0,NA,.data$o.sl),
                        upper = .data$p.ml+1.96*.data$p.sl,
                        lower = .data$p.ml-1.96*.data$p.sl,
                        o.upper = .data$o.ml+1.96*.data$o.sl,
                        o.lower = .data$o.ml-1.96*.data$o.sl) %>%
          dplyr::mutate(p.ml = dplyr::na_if(.data$p.ml, 0),
                        upper = dplyr::na_if(.data$upper, 0),
                        lower = dplyr::na_if(.data$lower, 0)) %>%
          dplyr::filter(!is.na(.data$p.ml)) %>%
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
            ncol = max(2*length(unique(dat$step)),4),
            labeller = ggplot2::label_wrap_gen(multi_line=FALSE)) +
          ggplot2::labs(x='Age', y='Average length') +
          ggplot2::expand_limits(x = 0) +
          # ggplot2::geom_text(
          #   x=-Inf,y=Inf,
          #   ggplot2::aes(label=paste(.data$year,.data$step,sep=',')),
          #   size = FS(base_size)*0.8,
          #   data = dat %>%
          #     dplyr::select(.data$year,.data$step) %>%
          #     dplyr::distinct(),vjust = 1.5,hjust = -0.1,
          #   inherit.aes = FALSE) +
          ggplot2::theme_classic(base_size = base_size) +
          ggplot2::theme(strip.background = ggplot2::element_blank())
      }
    })
}

