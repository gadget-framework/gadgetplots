# Plot of average growth for each stock

Plot of average growth for each stock

## Usage

``` r
plot_growth(
  fit,
  type = "annual",
  stdev = FALSE,
  add_models = FALSE,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- type:

  Character specifying the plot type. Options: `"annual"`, `"mean"` or
  `"stdev"`. See Details.

- stdev:

  Logical indicating whether standard deviation should be shown on both
  sides of mean for plots that use mean lengths.

- add_models:

  Logical indicating whether growth models extracted from growth
  parameters should be plotted together with the data. Uses grep and
  does not always work.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(fit)
plot_growth(fit)
#> Ignoring unknown labels:
#> • fill : "Stock"
#> • linetype : "Model"

plot_growth(fit, add_models = TRUE, stdev = TRUE)

plot_growth(fit, type = "mean")

plot_growth(fit, type = "stdev")
```
