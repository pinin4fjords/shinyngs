# Make an interactive clustering dendrogram colored by experimental variable

A `plotly` counterpart to
[`clusteringDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/clusteringDendrogram.md):
the branch geometry is derived from the same
[`hclust()`](https://rdrr.io/r/stats/hclust.html) tree, and the leaves
are placed as markers colored by an experimental variable. Sample names
sit on the x-axis (rotated, with plotly reserving the label margin), and
hovering a leaf reveals the sample name and its group value.

## Usage

``` r
plotly_clusteringDendrogram(
  plotmatrix,
  experiment,
  colorby = NULL,
  cor_method = "pearson",
  cluster_method = "ward.D",
  plot_title = "",
  palette = NULL,
  palette_name = COLORBLIND_PALETTE_NAME,
  hidden_groups = character(0),
  source = NULL
)
```

## Arguments

- plotmatrix:

  Expression/ other data matrix

- experiment:

  Annotation for the columns of plotmatrix

- colorby:

  Column name in `experiment` specifying how leaves should be colored

- cor_method:

  Correlation method, passed to cor() (default: pearson).

- cluster_method:

  Clustering method, passed to hclust() (default: ward.D).

- plot_title:

  Plot title

- palette:

  Palette of colors, one for each unique value derived from `colorby`.

- palette_name:

  Valid R color palette name

- hidden_groups:

  Values of `colorby` to exclude: their samples are dropped and the tree
  is recomputed on the remainder, while the groups stay in the legend
  (as `legendonly`) so they can be toggled back on.

- source:

  A plotly event source string, used to route legend-click
  (`plotly_restyle`) events back to a Shiny session.

## Value

output A `plotly` plot object

## Examples

``` r
data(airway, package = "airway")
mymatrix <- assays(airway)[[1]]
mymatrix <- mymatrix[order(apply(mymatrix, 1, var), decreasing = TRUE)[1:1000], ]
plotly_clusteringDendrogram(mymatrix, data.frame(colData(airway)), colorby = "dex")

{"x":{"visdat":{"29d632fee963":["function () ","plotlyVisDat"]},"cur_data":"29d632fee963","attrs":{"29d632fee963":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":[3.875,1.875,null,1.875,1.875,null,3.875,5.875,null,5.875,5.875,null,1.875,1,null,1,1,null,1.875,2.75,null,2.75,2.75,null,2.75,2,null,2,2,null,2.75,3.5,null,3.5,3.5,null,3.5,3,null,3,3,null,3.5,4,null,4,4,null,5.875,5,null,5,5,null,5.875,6.75,null,6.75,6.75,null,6.75,6,null,6,6,null,6.75,7.5,null,7.5,7.5,null,7.5,7,null,7,7,null,7.5,8,null,8,8,null],"y":[0.47741627293664257,0.47741627293664257,null,0.47741627293664257,0.13191943666025832,null,0.47741627293664257,0.47741627293664257,null,0.47741627293664257,0.15878420344832089,null,0.13191943666025832,0.13191943666025832,null,0.13191943666025832,0,null,0.13191943666025832,0.13191943666025832,null,0.13191943666025832,0.078357357546113018,null,0.078357357546113018,0.078357357546113018,null,0.078357357546113018,0,null,0.078357357546113018,0.078357357546113018,null,0.078357357546113018,0.049672259562300258,null,0.049672259562300258,0.049672259562300258,null,0.049672259562300258,0,null,0.049672259562300258,0.049672259562300258,null,0.049672259562300258,0,null,0.15878420344832089,0.15878420344832089,null,0.15878420344832089,0,null,0.15878420344832089,0.15878420344832089,null,0.15878420344832089,0.11827699947690484,null,0.11827699947690484,0.11827699947690484,null,0.11827699947690484,0,null,0.11827699947690484,0.11827699947690484,null,0.11827699947690484,0.086797262696912902,null,0.086797262696912902,0.086797262696912902,null,0.086797262696912902,0,null,0.086797262696912902,0.086797262696912902,null,0.086797262696912902,0,null],"type":"scatter","mode":"lines","line":{"color":"black","width":1},"hoverinfo":"none","showlegend":false,"inherit":true},"29d632fee963.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":[1,2,3,4],"y":[0,0,0,0],"type":"scatter","mode":"markers","text":["SRR1039520<br>Dex: untrt","SRR1039516<br>Dex: untrt","SRR1039508<br>Dex: untrt","SRR1039512<br>Dex: untrt"],"hoverinfo":"text","marker":{"color":"#E69F00","symbol":"circle","size":9,"line":{"color":"black","width":0.5}},"name":"untrt","inherit":true},"29d632fee963.2":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":[5,6,7,8],"y":[0,0,0,0],"type":"scatter","mode":"markers","text":["SRR1039517<br>Dex: trt","SRR1039521<br>Dex: trt","SRR1039509<br>Dex: trt","SRR1039513<br>Dex: trt"],"hoverinfo":"text","marker":{"color":"#56B4E9","symbol":"square","size":9,"line":{"color":"black","width":0.5}},"name":"trt","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"","xaxis":{"domain":[0,1],"automargin":true,"title":"","tickmode":"array","tickvals":[1,2,3,4,5,6,7,8],"ticktext":["SRR1039520","SRR1039516","SRR1039508","SRR1039512","SRR1039517","SRR1039521","SRR1039509","SRR1039513"],"tickangle":-90,"zeroline":false,"showgrid":false,"range":[0.5,8.5]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Height","zeroline":false,"showgrid":false,"range":[-0.023870813646832131,0.50128708658347476]},"hovermode":"closest","legend":{"title":{"text":"Dex"}},"showlegend":true},"source":null,"config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1.875,3.875,null,1.875,1.875,null,3.875,5.875,null,5.875,5.875,null,1,1.875,null,1,1,null,1.875,2.75,null,2.75,2.75,null,2,2.75,null,2,2,null,2.75,3.5,null,3.5,3.5,null,3,3.5,null,3,3,null,3.5,4,null,4,4,null,5,5.875,null,5,5,null,5.875,6.75,null,6.75,6.75,null,6,6.75,null,6,6,null,6.75,7.5,null,7.5,7.5,null,7,7.5,null,7,7,null,7.5,8,null,8,8],"y":[0.47741627293664257,0.47741627293664257,null,0.47741627293664257,0.13191943666025832,null,0.47741627293664257,0.47741627293664257,null,0.47741627293664257,0.15878420344832089,null,0.13191943666025832,0.13191943666025832,null,0.13191943666025832,0,null,0.13191943666025832,0.13191943666025832,null,0.13191943666025832,0.078357357546113018,null,0.078357357546113018,0.078357357546113018,null,0.078357357546113018,0,null,0.078357357546113018,0.078357357546113018,null,0.078357357546113018,0.049672259562300258,null,0.049672259562300258,0.049672259562300258,null,0.049672259562300258,0,null,0.049672259562300258,0.049672259562300258,null,0.049672259562300258,0,null,0.15878420344832089,0.15878420344832089,null,0.15878420344832089,0,null,0.15878420344832089,0.15878420344832089,null,0.15878420344832089,0.11827699947690484,null,0.11827699947690484,0.11827699947690484,null,0.11827699947690484,0,null,0.11827699947690484,0.11827699947690484,null,0.11827699947690484,0.086797262696912902,null,0.086797262696912902,0.086797262696912902,null,0.086797262696912902,0,null,0.086797262696912902,0.086797262696912902,null,0.086797262696912902,0],"type":"scatter","mode":"lines","line":{"color":"black","width":1},"hoverinfo":["none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none",null,"none","none"],"showlegend":false,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1,2,3,4],"y":[0,0,0,0],"type":"scatter","mode":"markers","text":["SRR1039520<br>Dex: untrt","SRR1039516<br>Dex: untrt","SRR1039508<br>Dex: untrt","SRR1039512<br>Dex: untrt"],"hoverinfo":["text","text","text","text"],"marker":{"color":"#E69F00","symbol":"circle","size":9,"line":{"color":"black","width":0.5}},"name":"untrt","error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[5,6,7,8],"y":[0,0,0,0],"type":"scatter","mode":"markers","text":["SRR1039517<br>Dex: trt","SRR1039521<br>Dex: trt","SRR1039509<br>Dex: trt","SRR1039513<br>Dex: trt"],"hoverinfo":["text","text","text","text"],"marker":{"color":"#56B4E9","symbol":"square","size":9,"line":{"color":"black","width":0.5}},"name":"trt","error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
