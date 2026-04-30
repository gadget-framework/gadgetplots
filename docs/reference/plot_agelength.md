# Plot of fitted growth for each age-length component

Plot of fitted growth for each age-length component

## Usage

``` r
plot_agelength(fit, name = NULL, base_size = 8)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- name:

  A character vector specifying the unique(fit\$catchdist.fleets\$name)
  to plot. If `NULL`, all names with relevant data are plotted.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
A list of ggplot objects if there are multiple age-length (aldist) data
sources (`unique(fit$catchdist.fleets$name)`).

## Examples

``` r
data(fit)
plot_agelength(fit)
#> $EggaN_aldist_female
#> Warning: Removed 16 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 3 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

#> 
#> $EggaN_aldist_male
#> Warning: Removed 58 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 12 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 9 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

#> 
```
