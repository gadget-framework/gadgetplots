# Repeat colors from a color palette

Repeats colors from a color palette `n` times to ensure that vector of
colors passed to ggplot2 manual color functions is as long as the number
of levels in data.

## Usage

``` r
repeat_palette(n, pal)
```

## Arguments

- n:

  numeric defining how many times the length of levels in data

- pal:

  A function defining the color palette to be repeated.

## Value

Returns a character vector of colors.

## Examples

``` r
scales::show_col(repeat_palette(12, scales::brewer_pal(palette = "Set1")))
```
