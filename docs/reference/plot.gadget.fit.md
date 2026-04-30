# Plot `gadget.fit` object

A wrapper function to plot the results from a
[gadget.fit](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html) object.

## Usage

``` r
# S3 method for class 'gadget.fit'
plot(x, param = "annual", ...)
```

## Arguments

- x:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- param:

  Character defining the parameter to plot. See Details.

- ...:

  Additional parameters passed to the separate plotting functions.

## Value

Single or a list of
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) objects
depending on the arguments.

## Details

A wrapper to plot the results from a
[gadget.fit](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html) object.
The function produces a different plots defined by `param` argument and
additional arguments passed to the respective functions.

Valid `param` options are:

- annual:

  Annual summary of central model data. Uses
  [`plot_annual`](plot_annual.md)

- biomass:

  Stock biomass. Uses [`plot_biomass`](plot_biomass.md)

- total:

  Stock biomass with total. Uses [`plot_biomass`](plot_biomass.md)(...,
  total = TRUE)

- abundance:

  Stock abundance. Uses [`plot_biomass`](plot_biomass.md)(..., biomass =
  FALSE)

- ssb:

  Spawning stock biomass. Uses [`plot_ssb`](plot_ssb.md)

- rec:

  Recruitment. Uses [`plot_rec`](plot_rec.md)

- f:

  Fishing mortality. Uses [`plot_f`](plot_f.md)

- catch:

  Catches by stock. Uses [`plot_catch`](plot_catch.md)(..., type =
  "stock")

- catch:

  Catches by fleet. Uses [`plot_catch`](plot_catch.md)(..., type =
  "fleet")

- hr:

  Harvest rate. Uses [`plot_catch`](plot_catch.md)(..., type = "hr")

- si:

  Survey indices. Uses [`plot_si`](plot_si.md)

- catchdist:

  Catch distribution comparison to data. Uses
  [`plot_catchdist`](plot_catchdist.md)

- stockdist:

  Stock distribution comparison to data. Uses
  [`plot_stockdist`](plot_stockdist.md)

- stockcomp:

  Model stock composition. Uses [`plot_stockcomp`](plot_stockcomp.md)

- suitablity:

  Suitability (fleet selection). Uses
  [`plot_suitability`](plot_suitability.md)

- growth:

  Average length by age. Uses [`plot_growth`](plot_growth.md)

- agelength:

  Age-length (growth parameter) fit. Uses
  [`plot_agelength`](plot_agelength.md)

- agecomp:

  Age composition. Uses [`plot_agecomp`](plot_agecomp.md)

- resid:

  Residuals. Uses [`plot_resid`](plot_resid.md)

- likelihood:

  Likelihood scores. Uses [`plot_likelihood`](plot_likelihood.md)

- weighted:

  Weighted likelihood scores. Uses
  [`plot_likelihood`](plot_likelihood.md)(..., type = "weighted")

- pie:

  Proportion of summed weighted likelihood scores. Uses
  [`plot_likelihood`](plot_likelihood.md)(..., type = "pie")

- weight:

  Parameter component weights

- params:

  Parameter values relative to their boundaries
