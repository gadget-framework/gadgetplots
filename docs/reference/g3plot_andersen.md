# Plot Andersen suitability using model parameters

Plot Andersen suitability using model parameters

## Usage

``` r
g3plot_andersen(
  length,
  p0,
  p1,
  p2,
  p3,
  p4,
  p5,
  add = FALSE,
  base_size = 8,
  ...
)
```

## Arguments

- length:

  Numeric vector defining the lengths for which suitabilities should be
  plotted

- p0, p1, p2, p3, p4, p5:

  (Named) numeric vectors defining the Andersen suitability function
  parameters. See suitability. If length \> 1, multiple curves will be
  plotted using a different line type. All of these parameters must have
  the same length.

- add:

  Logical indicating whether the function should produce a ggplot2 layer
  which can be added to an existing ggplot (`TRUE`) or whether to plot
  the results (`FALSE`)

- base_size:

  Base size parameter for ggplot. See ggtheme.

- ...:

  Additional arguments passed to `plot_*` functions.

## Examples

``` r
g3plot_andersen(length = 1:120, p0 = 0, p1 = 0.659, p2 = 1, p3 = 0.15, p4 = 1e4, p5 = 120)
```
