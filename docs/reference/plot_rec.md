# Plot recruitment

Plot recruitment

## Usage

``` r
plot_rec(
  fit,
  panelrow = FALSE,
  stocks = NULL,
  return_data = FALSE,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- panelrow:

  something here

- stocks:

  Character specifying the stock to plot in `fit`. If `NULL`, all stocks
  are plotted. If `"total"`, total recruitment will be plotted instead.

- return_data:

  Logical indicating whether to return data for the plot instead of the
  plot itself.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(fit)
plot_rec(fit)

plot_rec(fit, stocks = "total")
#> Ignoring unknown labels:
#> • fill : "Stock"
```
