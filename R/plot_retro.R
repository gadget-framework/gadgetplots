#' @title Plot model biomass from a retrospective analysis
#' @inheritParams plot_annual
#' @param retro_fit A list of \code{\link[gadgetutils]{g3_fit}} objects that differ in their terminal year, i.e. the result of a retrospective analysis. The first object should have no years removed and the objects should be sorted chronologically with increasing number of years removed.
#' @param by_stock Logical indicating whether to produce plots by stock in addition to total biomass.
#' @param ncol Number of columns to be used in the plot.
#' @return A \link[ggplot2]{ggplot} object.
#' @export

plot_retro <- function(retro_fit, by_stock = TRUE, base_size = 8, ncol = NULL) {

  if(!by_stock) {
    lapply(seq_along(retro_fit), function(i) {
      retro_fit[[i]]$res.by.year %>%
        dplyr::group_by(.data$year) %>%
        dplyr::summarise(value = sum(.data$total.biomass)/1e6) %>%
        dplyr::mutate(peel = paste0(i-1, " (", max(retro_fit[[i]]$res.by.year$year), ")"))
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(peel = factor(.data$peel)) %>%
      ggplot2::ggplot(ggplot2::aes(.data$year, .data$value, col=.data$peel)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        y = "Total model population biomass (kt)",
        x='Year',col='Years\nremoved') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  } else {
    dat <- lapply(seq_along(retro_fit), function(i) {
      retro_fit[[i]]$res.by.year %>%
        dplyr::group_by(.data$year, .data$stock) %>%
        dplyr::summarise(value = sum(.data$total.biomass)/1e6) %>%
        dplyr::mutate(peel = paste0(i-1, " (", max(retro_fit[[i]]$res.by.year$year), ")")) %>%
        dplyr::bind_rows(
          retro_fit[[i]]$res.by.year %>%
            dplyr::group_by(.data$year) %>%
            dplyr::summarise(value = sum(.data$total.biomass)/1e6) %>%
            dplyr::mutate(peel = paste0(i-1, " (", max(retro_fit[[i]]$res.by.year$year), ")"),
                          stock = "total")
        ) %>%
        dplyr::arrange(.data$peel, .data$year, .data$stock)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(peel = factor(.data$peel))

    tmp <- dat %>%
      dplyr::group_by(.data$stock, .data$peel) %>%
      dplyr::filter(.data$year == max(.data$year)) %>%
      dplyr::filter(!grepl("0 \\(", .data$peel)) %>%
      dplyr::rename("retro" = "value") %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$peel)

    mohn_dat <- tmp %>%
      dplyr::left_join(
        dat %>%
          dplyr::filter(grepl("0 \\(", .data$peel),
                        .data$year %in% unique(tmp$year)) %>%
          dplyr::rename("base" = "value") %>%
          dplyr::select(-.data$peel),
        by = c("year", "stock")
      ) %>%
      dplyr::mutate(rel_bias = (.data$retro - .data$base) / .data$base) %>%
      dplyr::group_by(.data$stock) %>%
      dplyr::summarise(rho = mean(.data$rel_bias))

    ggplot2::ggplot() +
      ggplot2::geom_line(
        data = dat,
        ggplot2::aes(.data$year, .data$value, col=.data$peel)) +
      ggplot2::geom_text(
        data = mohn_dat,
        ggplot2::aes(-Inf, Inf, label = paste("Mohn's rho = ", round(.data$rho, 3))),
        vjust = 2, hjust = -0.25, size = FS(base_size)
      ) +
      ggplot2::labs(
        y = "Model population biomass (kt)",
        x='Year',col='Years\nremoved') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::facet_wrap(~.data$stock, scales = "free_y", ncol = ncol) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::expand_limits(y = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank())
  }
}
