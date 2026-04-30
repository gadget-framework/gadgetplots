# Plot age-length distribution data for a gadget3 model

The dplot functions plot data passed to a gadget3 model instead of data
from the model or fit objects.

## Usage

``` r
dplot_aldist(
  x,
  type = "bar",
  facet_age = FALSE,
  dir = "v",
  color_palette = scales::brewer_pal(palette = "Set1"),
  base_size = 8,
  ...
)
```

## Arguments

- x:

  A gadget3 ready data frame created using mfdb,
  [g3_data](https://rdrr.io/pkg/gadgetutils/man/g3_data.html) or
  [add_g3_attributes](https://rdrr.io/pkg/gadgetutils/man/add_g3_attributes.html).

- type:

  Character specifying the plot type: "bar", "step", or "area". "step"
  produces a similar plot where age and length distributions are plotted
  separately as in [plot_catchdist](plot_catchdist.md). "area" produces
  an area plot instead of a bar plot, but works poorly when there are
  many age groups.

- facet_age:

  Logical indicating whether ages should be plotted in separate facets.
  Does not apply for `type = "step"`.

- dir:

  Character specifying the direction of the facets ("h" for horizontal,
  "v" for vertical). See
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Defaults to "v" unlike in
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to make it easier to follow length and age groups across years.

- color_palette:

  A function defining the color palette to be used for fill of bars when
  `facet_age = TRUE`. See
  [scale_color_manual](https://ggplot2.tidyverse.org/reference/scale_manual.html).
  To adjust color when `facet_age = FALSE`, use the standard
  `ggplot2::scale_fill_*` functions.

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ...:

  Additional arguments passed to
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(aldist_example)
dplot_aldist(aldist_example)

dplot_aldist(aldist_example, type = "step") # as in plot_catchdist

dplot_aldist(aldist_example, facet_age = TRUE)

dplot_aldist(aldist_example, type = "area") # works poorly
```
