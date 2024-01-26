#' @title Plot exponentiall50 suitability using model parameters
#' @inheritParams plot_annual
#' @param length Numeric vector defining the lengths for which suitabilities should be plotted
#' @param alpha (Named) numeric vector defining the exponentiall50 slopes. If length > 1, multiple curves will be plotted using a different line type. See \link[gadget3]{suitability}.
#' @param l50 Numeric vector defining the exponentiall50 l50 values. Must be same length than \code{alpha}
#' @param add Logical indicating whether the function should produce a ggplot2 layer which can be added to an existing ggplot (\code{TRUE}) or whether to plot the results (\code{FALSE})
#' @param ... Additional arguments passed to \code{geom_line}
#' @examples
#' data(fit)
#' suit <- fit$params[grepl("ghl_female.ecos.survey", fit$params$switch),]
#'
#' g3plot_exponentiall50(
#'    length = 1:60,
#'    alpha = unlist(suit[grepl("alpha", suit$switch), "value"]),
#'    l50 = unlist(suit[grepl("l50", suit$switch), "value"])
#'    )
#' @export

g3plot_exponentiall50 <- function(length, alpha, l50, add = FALSE, base_size = 8, ...) {

  if(length(alpha) != length(l50)) stop("alpha and l50 has to have equal length.")

  out <- lapply(seq_along(alpha), function(i) {
    data.frame(
      model = ifelse(!is.null(names(alpha)), names(alpha)[i], paste("expl50", i, sep = "_")),
      length = length,
      y = gadget3::g3_eval(gadget3::g3_suitability_exponentiall50(alpha[i],l50[i]),
                           stock = gadget3::g3_stock('s', length))
    )
  }) %>% dplyr::bind_rows()

  if(!add) {

    ggplot2::ggplot(out, ggplot2::aes(.data$length,.data$y)) + {
      if(length(unique(out$model)) > 1) ggplot2::geom_line(ggplot2::aes(lty = .data$model))
    } + {
      if(length(unique(out$model)) == 1) ggplot2::geom_line()
    } +
      ggplot2::labs(x = "Length", y = "Suitability") +
      ggplot2::theme_classic(base_size = base_size)

  } else {

    if(length(unique(out$model)) > 1) {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y, lty = .data$model),
        inherit.aes = FALSE, ...)
    } else {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y),
        inherit.aes = FALSE, ...)
    }

  }
}

#' @title Plot Andersen suitability using model parameters
#' @inheritParams plot_annual
#' @inheritParams g3plot_exponentiall50
#' @param p0,p1,p2,p3,p4,p5 (Named) numeric vectors defining the Andersen suitability function parameters. See \link[gadget3]{suitability}. If length > 1, multiple curves will be plotted using a different line type. All of these parameters must have the same length.
#' @examples
#' g3plot_andersen(length = 1:120, p0 = 0, p1 = 0.659, p2 = 1, p3 = 0.15, p4 = 1e4, p5 = 120)
#' @export

# length = 1:120; p0 = 0; p1 = 0.659; p2 = 1; p3 = 0.15; p4 = 1e4; p5 = 120; add = FALSE; base_size = 8
g3plot_andersen <- function(length, p0, p1, p2, p3, p4, p5, add = FALSE, base_size = 8, ...) {

  out <- lapply(seq_along(p0), function(i) {
    data.frame(
      model = ifelse(!is.null(names(p0)), names(p0)[i], paste("andersen", i, sep = "_")),
      length = length,
      y = gadget3::g3_eval(gadget3::g3_suitability_andersen(
        p0 = p0[i], p1 = p1[i], p2 = p2[i], p3 = p3[i],
        p4 = p4[i], p5 = p5[i]), stock = gadget3::g3_stock('s', length))
    )
  }) %>% dplyr::bind_rows()

  if(!add) {

    ggplot2::ggplot(out, ggplot2::aes(.data$length,.data$y)) + {
      if(length(unique(out$model)) > 1) ggplot2::geom_line(ggplot2::aes(lty = .data$model))
    } + {
      if(length(unique(out$model)) == 1) ggplot2::geom_line()
    } +
      ggplot2::labs(x = "Length", y = "Suitability") +
      ggplot2::theme_classic(base_size = base_size)

  } else {

    if(length(unique(out$model)) > 1) {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y, lty = .data$model),
        inherit.aes = FALSE, ...)
    } else {
      ggplot2::geom_line(
        data = out,
        ggplot2::aes(.data$length,.data$y),
        inherit.aes = FALSE, ...)
    }

  }
}

# ## Copy unexported gadget3 functions used by gadget3::g3_suitability_andersen
# avoid_zero <- gadget3::g3_native(r = function (a) {
#   # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
#   ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000
# }, cpp = '[](Type a) -> Type {
#     return logspace_add(a * 1000.0, (Type)0.0) / 1000.0;
# }')
#
# avoid_zero_vec <- gadget3::g3_native(r = function (a) {
#   # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
#   ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000
# }, cpp = '[](vector<Type> a) -> vector<Type> {
#     vector<Type> res(a.size());
#     for(int i = 0; i < a.size(); i++) {
#         res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
#     }
#     return res;
# }')
#
# bounded_vec <- gadget3::g3_native(r = function (x, a, b) {
#   a + (b-a)/(1+exp(x))
# }, cpp = '[](vector<Type> x, Type a, Type b) -> vector<Type> {
#     return a + (b-a)/(1+exp(x));
# }')
