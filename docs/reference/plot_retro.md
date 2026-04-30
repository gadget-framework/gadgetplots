# Plot model biomass from a retrospective analysis

Plot model biomass from a retrospective analysis

## Usage

``` r
plot_retro(
  retro_fit,
  var = "total.biomass",
  by_stock = TRUE,
  min_catch_length = NULL,
  base_size = 8,
  ncol = NULL
)
```

## Arguments

- retro_fit:

  A list of [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html)
  objects that differ in their terminal year, i.e. the result of a
  retrospective analysis. The first object should have no years removed
  and the objects should be sorted chronologically with increasing
  number of years removed.

- var:

  Character argument defining the variable to plot. Only
  `"total.biomass"` and `"hr"` are currently implemented.

- by_stock:

  Logical indicating whether to produce plots by stock in addition to
  total biomass.

- min_catch_length:

  Numeric value defining the minimum catch length (size), which will be
  used to filter (`>=`) the model population before calculating harvest
  rates using catches. Combines all stocks. Turn of by setting to `NULL`
  (default). Set to 0 to get HR for the entire model population. See
  Details.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ncol:

  Number of columns to be used in the plot.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
