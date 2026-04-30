# Wrapper for plot.gadget.fit that saves all diagnostic graphs to a directory

Wrapper for plot.gadget.fit that saves all diagnostic graphs to a
directory

## Usage

``` r
gadget_plots(
  fit,
  path,
  file_type = "png",
  quiet = FALSE,
  width = NULL,
  height = NULL,
  units = "cm",
  res = 300,
  ...
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- path:

  Directory path for saving figures.

- file_type:

  Character. Either one of the `device`s in
  [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html), in
  which case files of the defined type are printed to `path`, or
  `"html"` which compiles all plots into one html file using
  [`knit`](https://rdrr.io/pkg/knitr/man/knit.html) and the
  [`make_html`](make_html.md) function.

- quiet:

  Logical indicating whether to print messages about the plotting
  process. Set to `FALSE` to suppress the messages.

- width, height:

  Plot size given in `units`. If `NULL`, reasonable standard values are
  used.

- units:

  Units for plot size. See
  [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html)

- res:

  Plot resolution. See the `dpi` argument in
  [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html)

- ...:

  Additional arguments to plotting functions (only used for plot_annual
  at the moment)

## Value

Returns nothing, but makes the requested files.

## See also

make_html
