# Make scatterplots with `plot_ly()`

Make scatterplots with `plot_ly()`

## Usage

``` r
plotly_scatterplot(
  x,
  y,
  z = NULL,
  colorby = NULL,
  plot_type = "scatter",
  title = "",
  legend_title = "",
  xlab = "x",
  ylab = "y",
  zlab = "z",
  palette = NULL,
  point_size = 5,
  labels = NULL,
  show_labels = FALSE,
  lines = NULL,
  hline_thresholds = NULL,
  vline_thresholds = NULL,
  xrange = NULL,
  yrange = NULL,
  showlegend = TRUE,
  palette_name = COLORBLIND_PALETTE_NAME,
  colorby_menu = NULL
)
```

## Arguments

- x:

  X coordinates

- y:

  Y coordinates

- z:

  Optional Z coordinates

- colorby:

  String vector or factor specifying value groups

- plot_type:

  Plot type: 'scatter' or 'scatter3d'

- title:

  Plot title

- legend_title:

  Legend title

- xlab:

  X label

- ylab:

  Y label

- zlab:

  Z label

- palette:

  Color palette correct for the number of groups in 'colorby'

- point_size:

  Main point size

- labels:

  Point labels

- show_labels:

  Permanently show labels for labelled points (default is just on
  hoverover). Ignored when `colorby_menu` is supplied.

- lines:

  3 column data-frame (name, x, y) with two rows, one for the start and
  end of each named line

- hline_thresholds:

  Named list of horizontal lines with y coordinates

- vline_thresholds:

  Named list of vertical lines x coordinates

- xrange:

  Optional fixed c(min, max) x axis range, e.g. to keep a volcano plot
  symmetric around zero. Only applied when lines/thresholds are drawn;
  otherwise plotly's own autorange is used. Defaults to NULL (derive the
  range from the data).

- yrange:

  Optional fixed c(min, max) y axis range. Same caveats as `xrange`.

- showlegend:

  Boolean: show a legend?

- palette_name:

  Valid R color palette name

- colorby_menu:

  Opt-in, named list of alternative colour vectors (one per entry, same
  length as x/y(/z)). When supplied, the plot is coloured using
  [`addColorbyMenu`](https://pinin4fjords.github.io/shinyngs/reference/addColorbyMenu.md)
  instead of `colorby`, and a plotly dropdown is added letting the
  reader switch which variable colours the points; `colorby` is not
  shown itself unless also included as an entry in `colorby_menu`. This
  is intended for standalone/static-HTML report use only: the Shiny
  scatterplot modules already offer their own colour-by `selectInput`,
  and must NOT set this argument, since an always-on in-widget dropdown
  alongside that control would give readers two competing ways to change
  the colouring. Default is NULL (off).

## Value

output Plotly plot object

## Examples

``` r
set.seed(1)
x <- rnorm(10)
y <- rnorm(10)
colorby <- factor(rep(c("treated", "control"), each = 5))
plotly_scatterplot(x, y, colorby = colorby, xlab = "PC1", ylab = "PC2")

{"x":{"visdat":{"29b36dfd16d":["function () ","plotlyVisDat"]},"cur_data":"29b36dfd16d","attrs":{"29b36dfd16d":{"mode":"markers","colors":"#E69F00","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"29b36dfd16d.1":{"mode":"markers","colors":"#E69F00","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","x":[-0.62645381074233242,0.18364332422208224,-0.83562861241004716,1.5952808021377916,0.32950777181536051,-0.82046838411801526,0.48742905242848528,0.73832470512921733,0.57578135165349231,-0.30538838715635602],"y":[1.511781168450848,0.38984323641143109,-0.62124058054180376,-2.2146998871774999,1.1249309181431082,-0.044933609015230851,-0.016190263098946087,0.94383621068529922,0.82122119509808855,0.59390132121750883],"showlegend":true,"name":"unselected rows","hoverinfo":"none","marker":{"size":3,"color":"gray"},"inherit":true},"29b36dfd16d.2":{"mode":"markers","colors":"#E69F00","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","x":[],"y":[],"showlegend":true,"hoverinfo":"text","marker":{"size":5},"color":[],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"hovermode":"closest","title":"","xaxis":{"domain":[0,1],"automargin":true,"title":"PC1"},"yaxis":{"domain":[0,1],"automargin":true,"title":"PC2"},"legend":{"title":{"text":""},"y":0.80000000000000004},"showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"mode":"markers","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"mode":"markers","type":"scatter","x":[-0.62645381074233242,0.18364332422208224,-0.83562861241004716,1.5952808021377916,0.32950777181536051,-0.82046838411801526,0.48742905242848528,0.73832470512921733,0.57578135165349231,-0.30538838715635602],"y":[1.511781168450848,0.38984323641143109,-0.62124058054180376,-2.2146998871774999,1.1249309181431082,-0.044933609015230851,-0.016190263098946087,0.94383621068529922,0.82122119509808855,0.59390132121750883],"showlegend":true,"name":"unselected rows","hoverinfo":["none","none","none","none","none","none","none","none","none","none"],"marker":{"color":"gray","size":3,"line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"mode":"markers","type":"scatter","showlegend":true,"hoverinfo":"text","marker":{"color":"rgba(44,160,44,1)","size":5,"line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
