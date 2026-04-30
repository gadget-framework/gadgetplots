# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**gadgetplots** is an R package that creates diagnostic graphics for the [gadget3](https://github.com/gadget-framework/gadget3) stock assessment model framework. It operates on `gadget.fit` objects produced by `gadgetutils::g3_fit()`.

## Build and Development Commands

All commands run in R (or via `Rscript`):

```r
# Document (regenerates man/*.Rd and NAMESPACE from Roxygen2 comments)
devtools::document()

# Build and check the package
devtools::check()

# Load package for interactive development
devtools::load_all()

# Install locally
devtools::install()

# Build pkgdown website
pkgdown::build_site()
```

There is no formal test suite (`testthat` is not set up). Testing is done interactively using the example datasets (`fit`, `aldist_example`, `ldist_example`, etc.) available after `devtools::load_all()`.

## Documentation

All function documentation is written using **Roxygen2** inline with the source in `R/`. After editing `@param`, `@title`, `@description`, or `@examples` tags, run `devtools::document()` — never edit `man/*.Rd` files directly.

## Code Architecture

### Two function families

**`plot_*(fit, ...)`** — plot model output. These accept a `gadget.fit` object (returned by `gadgetutils::g3_fit()`), extract the relevant slot (e.g. `fit$catchdist.fleets`, `fit$res.by.year`), and return a `ggplot` or `cowplot` object.

**`dplot_*(data, ...)`** — plot raw input data before fitting. These accept tibbles/data frames with gadget-style columns (e.g. `age`, `length`, `number`, `step`, `area`) and are used for data exploration and diagnostics.

### Additional function families

- **`g3d_plot()`** — plots data directly from a gadget3 model object (not a fit).
- **`g3plot_*()`** — plots selectivity/suitability functions independently of a fit.
- **`make_html(fit, ...)`** — renders one of the Rmd templates in `inst/` into a self-contained HTML diagnostic report using flexdashboard.
- **`gadget_plots(fit, path, ...)`** — batch-saves all diagnostic plots to a directory.
- **`plot.gadget.fit()`** — S3 dispatch method; routes `plot(fit, param = "biomass")` calls to the correct `plot_*()` function.

### Shared utilities

`R/internal_functions.R` provides `FS(x)` (font size scaling) and `LS(x)` (line size scaling), used throughout the plotting functions to maintain consistent sizing. Pass them whenever setting `ggplot2` text or line size aesthetics.

### HTML templates

`inst/standard.Rmd`, `inst/iceland.Rmd`, and `inst/nea_ghl.Rmd` are flexdashboard templates called by `make_html()`. They `source()` gadgetplots functions and expect a `fit` object in the environment.

### Key dependencies

- `ggplot2` / `cowplot` — all output is ggplot-based; multi-panel layouts use `cowplot::plot_grid()`
- `dplyr` / `tidyr` / `purrr` — data manipulation inside plotting functions
- `gadget3` — model object structure (slots, naming conventions)
- `flexdashboard` / `rmarkdown` / `DT` — HTML report generation
