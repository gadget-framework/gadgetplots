# Plot stock distribution data for a gadget3 model

The dplot functions plot data passed to a gadget3 model instead of data
from the model or fit objects.

## Usage

``` r
dplot_stockdist(
  x,
  stock_col = "maturity_stage",
  proportion = FALSE,
  group_by_sex = FALSE,
  sexes = c(female = "^female", male = "^male"),
  colors = NULL,
  scales = "free_y",
  dir = "v",
  base_size = 8,
  ...
)
```

## Arguments

- x:

  A gadget3 ready data frame created using mfdb,
  [g3_data](https://rdrr.io/pkg/gadgetutils/man/g3_data.html) or
  [add_g3_attributes](https://rdrr.io/pkg/gadgetutils/man/add_g3_attributes.html).

- stock_col:

  Character defining the name of the column separating stocks.

- proportion:

  Logical indicating whether to plot proportion of stocks instead of
  absolute numbers.

- group_by_sex:

  Logical indicating whether to take the proportions by sex (`TRUE`) or
  by all stocks (`FALSE`). Used only when `proportion = TRUE`

- sexes:

  Named vector of length two containing regular expressions separating
  sexes in `stock_col`. See the default as an example.

- colors:

  A vector of colors to be used for stocks. If `NULL`,
  [`scales::hue_pal()`](https://scales.r-lib.org/reference/pal_hue.html)
  will be used

- scales:

  Character specifying the `scales` argument passed to
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Defaults to `"free_y"`.

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
data(stockdist_example)
dplot_stockdist(stockdist_example)

dplot_stockdist(stockdist_example, proportion = TRUE)

dplot_stockdist(stockdist_example, proportion = TRUE, group_by_sex = TRUE)
```
