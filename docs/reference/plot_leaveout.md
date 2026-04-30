# Plot central model output from a leave-out analysis

Plot central model output from a leave-out analysis

## Usage

``` r
plot_leaveout(
  lo_fit,
  vars = c("nll.summary", "total.biomass", "hr", "recruitment"),
  ncol = NULL,
  colors = NULL,
  base_size = 8,
  legend_title = "Left-out\ncomponent"
)
```

## Arguments

- lo_fit:

  A list of [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html)
  objects originating from a leave-out analysis. See details.

- vars:

  Character vector giving the variables to plot. Only the default
  parameters are currently implemented.

- ncol:

  Number of columns to be used in the plot. Set to `NULL` for standard
  layout.

- colors:

  Vector of colors to use for leave-out component names. If a named
  vector, the component names are sorted following the order in the
  legend.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- legend_title:

  Character speficfying the title for legend (color mapping)

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
