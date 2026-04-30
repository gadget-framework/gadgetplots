# Plot catches

Plot catches

## Usage

``` r
plot_catch(
  fit,
  type = "stock",
  biomass = TRUE,
  base_size = 8,
  return_data = FALSE
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- type:

  Character specifying the data type: `"stock"` plots the catches by
  stock, `"fleet"` by fleet, `"total"` catches without separating to
  stock or fleet, and `"hr"` harvest rates by fleet.

- biomass:

  Logical indicating whether biomass should be plotted instead of
  estimated abundance.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- return_data:

  Logical indicating whether to return data for the plot instead of the
  plot itself.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(fit)
plot_catch(fit)

plot_catch(fit, biomass = FALSE)

plot_catch(fit, type = "total")
#> Ignoring unknown labels:
#> • fill : "Stock"

plot_catch(fit, type = "fleet")

plot_catch(fit, type = "hr")
```
