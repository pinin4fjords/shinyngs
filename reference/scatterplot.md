# Server function for the scatterplot module

This module uses [Plotly](https://plot.ly/) to create scatter plots (see
[`plot_ly`](https://rdrr.io/pkg/plotly/man/plot_ly.html)), of both 2D
and 3D varieties.

## Usage

``` r
scatterplot(
  id,
  getDatamatrix,
  getThreedee = NULL,
  getXAxis = NULL,
  getYAxis = NULL,
  getZAxis = NULL,
  getShowLabels = NULL,
  getPointSize = NULL,
  getPalette = NULL,
  colorBy = NULL,
  getTitle = reactive({
     ""
 }),
  getLabels = reactive({
     rownames(getDatamatrix())
 }),
  allow_3d = TRUE,
  x = NA,
  y = NA,
  z = NA,
  getLines = reactive({
     NULL
 })
)
```

## Arguments

- id:

  Module namespace

- getDatamatrix:

  Reactive supplying a matrix. If using external controls this should
  match the one supplied to `scatterplotcontrols`

- getThreedee:

  A reactive defining whether to plot in 3D. If set to NULL (default),
  the `scatterplotcontrols` module will be called to create a set of
  inputs to supply this value and the axes etc.

- getXAxis:

  NULL, or if `getThreedee` is a reactive, a reactive supplying an
  integer specifying which column of the matrix supplied by
  `getDatamatrix` should be used for this axis.

- getYAxis:

  NULL, or if `getThreedee` is a reactive, a reactive supplying an
  integer specifying which column of the matrix supplied by
  `getDatamatrix` should be used for this axis.

- getZAxis:

  NULL, or if `getThreedee` is a reactive, a reactive supplying an
  integer specifying which column of the matrix supplied by
  `getDatamatrix` should be used for this axis.

- getShowLabels:

  NULL, or if `getThreedee` is a reactive, a reactive supplying a
  logical defining whether labels should be shown on points.

- getPointSize:

  NULL, or if `getThreedee` is a reactive, a reactive supplying an
  integer point size to pass to plotly.

- getPalette:

  An optional palette of colors, one for each level of colorBy.

- colorBy:

  A reactive returning a factor definining the groups in which points
  should be colored.

- getTitle:

  A reactive expression supplying a title.

- getLabels:

  A reactive supplying a list of labels to use instead of row names from
  `getDatamatrix()`

- allow_3d:

  Passed to
  [`scatterplotcontrolsInput`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrolsInput.md)
  to dermine if the user will be allowed to create 3D plots.

- x:

  Passed to
  [`scatterplotcontrolsInput`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrolsInput.md)
  to determine how it produces an input field for selecting the x axis.
  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA).

- y:

  Passed to
  [`scatterplotcontrolsInput`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrolsInput.md)
  to determine how it produces an input field for selecting the y axis.
  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA).

- z:

  Passed to
  [`scatterplotcontrolsInput`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrolsInput.md)
  to determine how it produces an input field for selecting the z axis.
  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA).

- getLines:

  Reactive returning a data frame defining lines to be drawn. Three
  columns required: name, x and y, with two rows for every value of
  name. These two rows represent the start and end of a line.

## Details

Controls for this module are provided by the `scatterplotcontrols`
module, which is automatically called if reactives are not supplied to
the server function. This setup allows the same set of controls to power
multiple scatter plots.
