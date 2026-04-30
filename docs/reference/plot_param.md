# Plot parameter values relative to their boundaries

Plot parameter values relative to their boundaries

## Usage

``` r
plot_param(fit, out_only = FALSE, base_size = 8)
```

## Arguments

- fit:

  A gadget fit object or a data frame containing model parameters. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- out_only:

  Logical indicating whether only parameters outside their boundaries
  should be plotted.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

The default plot is likely busy. Use
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) to
make it easier to interpret. If you are after parameters that are
outside their boundaries, the `out_only` argument will help.

## Examples

``` r
data(fit)
plot_param(fit)

plot_param(fit, out_only = TRUE)
```
