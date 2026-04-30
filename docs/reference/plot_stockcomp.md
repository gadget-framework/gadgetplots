# Plot model stock composition

Plots proportions of stocks in the model by length or age

## Usage

``` r
plot_stockcomp(fit, type = "line", by_age = FALSE, base_size = 8)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- type:

  Character specifying the plot type. Options: "line" or "area". See
  examples.

- by_age:

  Logical indicating whether age should be used on the x-axis instead of
  length

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
or a list of such objects depending on the `type` argument.

## Examples

``` r
data(fit)
plot_stockcomp(fit)
#> Ignoring unknown labels:
#> • fill : "Stock"

plot_stockcomp(fit, type = "area")
#> Ignoring unknown labels:
#> • colour : "Stock"

plot_stockcomp(fit, by_age = TRUE)
#> Ignoring unknown labels:
#> • fill : "Stock"
#> Warning: Removed 82 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
