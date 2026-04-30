# Plot central model output from a jitter analysis

Plot central model output from a jitter analysis

## Usage

``` r
plot_jitter(
  jitter_fit,
  vars = c("nll.summary", "total.biomass", "hr", "recruitment"),
  base_size = 8,
  ncol = NULL
)
```

## Arguments

- jitter_fit:

  A list of [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html)
  objects originating from
  [`g3_jitter`](https://rdrr.io/pkg/gadgetutils/man/g3_jitter.html).

- vars:

  Character vector giving the variables to plot. Only the default
  parameters are currently implemented.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ncol:

  Number of columns to be used in the plot. Set to `NULL` for standard
  layout.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
