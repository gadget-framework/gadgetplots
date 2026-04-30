# Plot sex ratio data for a gadget3 model

The dplot functions plot data passed to a gadget3 model instead of data
from the model or fit objects.

## Usage

``` r
dplot_sexr(x, dir = "v", base_size = 8, ...)
```

## Arguments

- x:

  A gadget3 ready data frame created using mfdb,
  [g3_data](https://rdrr.io/pkg/gadgetutils/man/g3_data.html) or
  [add_g3_attributes](https://rdrr.io/pkg/gadgetutils/man/add_g3_attributes.html).

- dir:

  Character specifying the direction of the facets ("h" for horizontal,
  "v" for vertical). See
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Defaults to "v" unlike in
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to make it easier to follow length and age groups across years.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ...:

  Additional arguments passed to
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(sexratio_example)
dplot_sexr(sexratio_example)
```
