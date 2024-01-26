#' @title Repeat colors from a color palette
#' @description Repeats colors from a color palette \code{n} times to
#' ensure that vector of colors passed to ggplot2 manual color functions is as
#' long as the number of levels in data.
#' @param n numeric defining how many times the length of levels in data
#' @param pal A function defining the color palette to be repeated.
#' @return Returns a character vector of colors.
#' @examples
#' scales::show_col(repeat_palette(12, scales::brewer_pal(palette = "Set1")))
#' @keywords internal
#' @export

repeat_palette <- function(n, pal) {
  if(inherits(
    tryCatch(pal(n), error=function(e) e, warning=function(w) w),
    "character")
  ) {
    pal(n)
  } else {
    tmp <- suppressWarnings(pal(1e5))
    tmp <- tmp[!is.na(tmp)]
    rep(tmp, length.out = n)
  }
}
