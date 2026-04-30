# Plot fishing mortality

Plot fishing mortality

## Usage

``` r
plot_f(
  fit,
  stocks = NULL,
  fbar_ages = NULL,
  return_data = FALSE,
  base_size = 8
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- stocks:

  Character specifying the substock to plot in `fit`. If `NULL`, all
  stocks are plotted. Not applicable if `fbar_ages` is defined.

- fbar_ages:

  Either `NULL` or a numeric vector of ages to include to calculate Fbar
  (averaged F over age ranges) for selected ages instead of F for each
  stock.

- return_data:

  Logical indicating whether to return data for the plot instead of the
  plot itself.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

The function calculates either average fishing mortality per substock or
average fishing mortality over an age range depending on the `fbar_ages`
argument.

## See also

[plot_hr](plot_hr.md)

## Examples

``` r
data(fit)
plot_f(fit)

plot_f(fit, fbar_ages = 5:10)
```
