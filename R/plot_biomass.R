#' @title Plot stock biomasses
#' @inheritParams plot_annual
#' @inheritParams plot_hr
#' @param type Character specifying the plot type. Options: "line" or "area". See examples.
#' @param total Logical indicating whether total biomass should be plotted. Has no effect if \code{type = "area"}.
#' @param min_catch_length Numeric value defining the minimum catch length (size), which will be used to filter (\code{>=}) the model population before calculating biomass. Combines all stocks. Turn of by setting to \code{NULL} (default).
#' @param biomass Logical indicating whether biomass should be plotted instead of abundance.
#' @return A \link[ggplot2]{ggplot} object.
#' @examples
#' data(fit)
#' plot_biomass(fit)
#' plot_biomass(fit, biomass = FALSE, total = TRUE)
#' plot_biomass(fit, type = "area")
#' plot_biomass(fit, min_catch_length = 45)
#' @export

# total = FALSE;  biomass = TRUE;base_size = 8
plot_biomass <- function(fit, type = "line", total = FALSE, biomass = TRUE, stocks = NULL, min_catch_length = NULL, return_data = FALSE, base_size = 8){

  if (length(fit) == 1) fit <- fit[[1]]
  if (!inherits(fit, 'gadget.fit')) stop("fit must be a gadget fit object.")

  if(!is.null(min_catch_length)) {
    dat <- fit$stock.full %>%
      dplyr::filter(.data$length >= min_catch_length) %>%
      dplyr::mutate(biomass = .data$number * .data$mean_weight) %>%
      dplyr::group_by(.data$year, .data$step, .data$area) %>%
      dplyr::summarise(
        total.number = sum(.data$number, na.rm = TRUE),
        total.biomass = sum(.data$biomass, na.rm = TRUE),
        .groups = "drop"
      ) %>% dplyr::ungroup()
  } else {
    if(is.null(stocks)) stocks <- unique(fit$res.by.year$stock)
    dat <- fit$res.by.year %>%
      dplyr::filter(.data$stock %in% stocks)
  }

  if(biomass) {
    ylab <- "Biomass (kt)"
    dat$value <- dat$total.biomass/1e6
  } else {
    ylab <- "Abundance (millions)"
    dat$value <- dat$total.number/1e6
  }

  #dat <- dat %>%
   # dplyr::select(.data$stock, .data$year, .data$step, .data$area, .data$value)

  if(total & type != "area" & is.null(min_catch_length)) {
    dat <- dat %>% dplyr::bind_rows(
      dat %>%
        dplyr::group_by(.data$year, .data$step, .data$area) %>%
        dplyr::summarise(value = sum(.data$value)) %>%
        dplyr::mutate(stock = 'Total')
    )
  }

  if(return_data) return(dat)

  if(type == "area") {

    ggplot2::ggplot(dat) + {
      if(!is.null(min_catch_length)) {
        ggplot2::geom_area(ggplot2::aes(.data$year, .data$value))
      }
    } + {
      if(is.null(min_catch_length)) {
        ggplot2::geom_area(ggplot2::aes(.data$year, .data$value, fill = .data$stock))
      }
    } +
      ggplot2::labs(
        y = ifelse(
          is.null(min_catch_length), ylab,
          paste(ylab, "for >= ", min_catch_length, " length units")),
        x='Year',col='Stock') +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)

  } else {

    ggplot2::ggplot(dat) + {
      if(!is.null(min_catch_length)) {
        ggplot2::geom_line(ggplot2::aes(.data$year, .data$value))
      }
    } + {
      if(is.null(min_catch_length)) {
        ggplot2::geom_line(ggplot2::aes(.data$year, .data$value, color = .data$stock))
      }
    } +
      ggplot2::labs(
        y = ifelse(
          is.null(min_catch_length), ylab,
          paste(ylab, "for >= ", min_catch_length, " length units")),
        x='Year',col='Stock') +
      ggplot2::expand_limits(y = 0) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_classic(base_size = base_size)
  }
}

