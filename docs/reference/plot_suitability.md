# Plot suitability

Plot suitability

## Usage

``` r
plot_suitability(
  fit,
  fleets = "all",
  add_models = TRUE,
  include_missing = TRUE,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- fleets:

  Character vector specifying the fleets to plot in `fit`. If `NULL`,
  all fleets will be plotted in separate plots. Use `"all"` to plot all
  fleets to the same plot.

- add_models:

  Logical indicating whether models using suitability parameters should
  be plotted together with the suitabilities estimated from data. The
  name of the parameter has to be similar to the fleet name. Uses grep
  and does not always work.

- include_missing:

  Logical indicating whether years with missing catch data should be
  plotted.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
If `fleet = NULL`, a list of ggplot objects.

## Examples

``` r
data(fit)
plot_suitability(fit)

plot_suitability(fit, fleets = "EggaN_survey",
                 include_missing = FALSE)
```
