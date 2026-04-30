# Plot model age distributions for stocks by year

Plot model age distributions for stocks by year

## Usage

``` r
plot_adist(
  fit,
  type = "bar",
  scales = "fixed",
  ncol = NULL,
  years = NULL,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- type:

  Character specifying the plot type. Options: `"line"`, `"bar"` or
  `"ggridges"`. See examples.

- scales:

  Character defining the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  `scales` argument to use.

- ncol:

  Number of columns passed to
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- years:

  Numeric vector defining which years to plot. If `NULL` (default), all
  years are plotted.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
or a list of such objects depending on the `type` argument.

## Examples

``` r
data(fit)
plot_adist(fit)

plot_adist(fit, type = "line")

plot_adist(fit, type = "ggridges")
```
