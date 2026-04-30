# Plot stock-recruitment relationship

Plots spawning stock biomass (SSB) against recruitment with a lag

## Usage

``` r
plot_sr(
  fit,
  spawning_stock = NULL,
  lag = 1,
  years = NULL,
  add_line = TRUE,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- spawning_stock:

  Either character defining the name of the stock that should be
  understood as spawning stock, or numeric giving the minimum length of
  spawning fish. If `NULL`, all stocks are used.

- lag:

  Integer giving the number of time steps that should be used as a lag
  between SSB and recruitment.

- years:

  A vector of recruitment years to include. If `NULL`, all years will be
  used.

- add_line:

  Add a line connecting years

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
or a list of such objects depending on the `stocks` argument.

## Examples

``` r
data(fit)
plot_sr(fit, spawning_stock = "ghl_female_mat")

plot_sr(fit, spawning_stock = 45)
```
