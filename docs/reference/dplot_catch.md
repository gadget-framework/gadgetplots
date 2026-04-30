# Plot catch data for a gadget3 model

The dplot functions plot data passed to a gadget3 model instead of data
from the model or fit objects.

## Usage

``` r
dplot_catch(x, base_size = 8)
```

## Arguments

- x:

  A gadget3 ready data frame created using mfdb,
  [g3_data](https://rdrr.io/pkg/gadgetutils/man/g3_data.html) or
  [add_g3_attributes](https://rdrr.io/pkg/gadgetutils/man/add_g3_attributes.html).
  Can be a single data frame or a named list of data frames in which
  case multiple catches are plotted in the same figure.

- base_size:

  Base size parameter for ggplot. See ggtheme.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data(example_catches)
dplot_catch(TrawlNor_catches)

dplot_catch(
  list("TrawlNor" = TrawlNor_catches,
       "OtherNor" = OtherNor_catches,
       "TrawlRus" = TrawlRus_catches,
       "OtherRus" = OtherRus_catches,
       "Internat" = Internat_catches)
       )
```
