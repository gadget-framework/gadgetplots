# Plot exponentiall50 suitability using model parameters

Plot exponentiall50 suitability using model parameters

## Usage

``` r
g3plot_exponentiall50(length, alpha, l50, add = FALSE, base_size = 8, ...)
```

## Arguments

- length:

  Numeric vector defining the lengths for which suitabilities should be
  plotted

- alpha:

  (Named) numeric vector defining the exponentiall50 slopes. If length
  \> 1, multiple curves will be plotted using a different line type. See
  suitability.

- l50:

  Numeric vector defining the exponentiall50 l50 values. Must be same
  length than `alpha`

- add:

  Logical indicating whether the function should produce a ggplot2 layer
  which can be added to an existing ggplot (`TRUE`) or whether to plot
  the results (`FALSE`)

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ...:

  Additional arguments passed to `geom_line`

## Examples

``` r
data(fit)
suit <- fit$params[grepl("ghl_female.ecos.survey", fit$params$switch),]

g3plot_exponentiall50(
   length = 1:60,
   alpha = unlist(suit[grepl("alpha", suit$switch), "value"]),
   l50 = unlist(suit[grepl("l50", suit$switch), "value"])
   )
```
