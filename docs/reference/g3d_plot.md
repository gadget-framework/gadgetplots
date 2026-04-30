# Plot of data passed into a gadget model

Plot all observational data from a gadget model

## Usage

``` r
g3d_plot(model, regexp = NULL, scales = "fixed", ncol = NULL, base_size = 8)
```

## Arguments

- model:

  R or TMB model. A model object from
  [g3_to_r](https://gadget-framework.github.io/gadget3/reference/run_r.html)
  or
  [g3_to_tmb](https://gadget-framework.github.io/gadget3/reference/run_tmb.html)
  functions.

- regexp:

  Either `NULL` (no filtering) or a character string giving a regular
  expression to filter model-data components. Useful alternatives:
  `"adist"` for abundance distribution data, `"cdist"` for catch
  distribution data, `"surveyindices"` for survey indices, and `"catch"`
  for catches.

- scales:

  Character defining the
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  `scales` argument to use.

- ncol:

  Number of columns passed to
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A list of [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
objects.

## Details

Plots data as contained in a gadget model. Helpful in checking models
for possible data issues and for documentation of models.
