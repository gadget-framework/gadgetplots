# Plot an age class from the model

Plot an age class from the model

Plots age class from the model at the beginning of the year

## Usage

``` r
plot_ageclass(
  fit,
  age = 2,
  biomass = FALSE,
  base_size = 8,
  return_data = FALSE
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- age:

  Integer specifying the age class to plot.

- biomass:

  Logical indicating whether biomass should be plotted instead of
  abundance.

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
plot_ageclass(fit)

plot_ageclass(fit, age = 5, biomass = TRUE)
```
