# Save diagnostic graphs to a single html document

Save diagnostic graphs to a single html document

## Usage

``` r
make_html(
  fit,
  path = getwd(),
  harvest_rate = TRUE,
  file_name = "model_output_figures.html",
  template = "standard",
  model_name = "Gadget model output"
)
```

## Arguments

- fit:

  A gadget fit object. See
  [`g3_fit`](https://rdrr.io/pkg/gadgetutils/man/g3_fit.html).

- path:

  Directory path for saving the html output. Defaults to working
  directory.

- harvest_rate:

  A Logical value, if `TRUE` the harvest rate will be plotted, if
  `FALSE` the fishing mortality will be plotted.

- file_name:

  Character specifying the name of the html file without path. Must
  include the file extension.

- template:

  Character specifying the html template to use. See details.

- model_name:

  Character specifying the name of the model when "standard `template`
  is used.

## Value

Returns nothing, but makes the requested file. See the [webpage for
example
output](https://gadget-framework.github.io/gadgetplots/articles/make_html_output.html).

## Details

The package contains html templates tailored for different stocks needed
by the authors. Specify the name in the `template` argument. Current
alternatives are:

- standard:

  Standard model output figures. Use the `harvest_rate` argument to
  switch between F and HR figures.

- iceland:

  Output tailored for Icelandic stock assessment.

- nea_ghl:

  Output tailored for Northeast Arctic Greenland halibut assessment.
