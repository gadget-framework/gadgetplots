# Plot spawning stock biomass

Plot spawning stock biomass

## Usage

``` r
plot_ssb(fit, spawning_stock = NULL, base_size = 8)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- spawning_stock:

  Character specifying the name of the spawning stock in the `fit`
  object (i.e. the female mature stock). If `NULL`, the correct stock is
  guessed.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(fit)
plot_ssb(fit, spawning_stock = "ghl_female_mat")
```
