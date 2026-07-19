# Make a PCA-vs-metadata association heatmap with `heatmaply()`

Runs
[`anova_pca_metadata`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
on the supplied PCA coordinates and sample metadata, then renders the
resulting p value matrix with
[`interactiveHeatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md):
-log10(p) sets the cell color, the raw p values are shown on hover, and
variables (rows) with a single, uninformative value across all shown
cells are dropped when `cluster_rows` is TRUE.

## Usage

``` r
plotly_pca_metadata_heatmap(
  pca_coords,
  pcameta,
  fraction_explained,
  cluster_rows = TRUE,
  plot_height = NULL,
  ...
)
```

## Arguments

- pca_coords:

  Data frame of PCA coordinates, with samples by row and components by
  column (e.g. the `x` element of
  [`runPCA`](https://pinin4fjords.github.io/shinyngs/reference/runPCA.md)'s
  `prcomp` result).

- pcameta:

  Data frame of sample metadata with sample identifiers by row and
  variables by column.

- fraction_explained:

  Numeric vector containing the percent contribution to variance of each
  component (e.g. from
  [`calculatePCAFractionExplained`](https://pinin4fjords.github.io/shinyngs/reference/calculatePCAFractionExplained.md)).

- cluster_rows:

  Cluster variables (rows) by their p value profile across components?

- plot_height:

  The total rendered height of the plot in pixels, passed through to
  [`interactiveHeatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md).
  Defaults to a height scaled to the number of variables.

- ...:

  Additional arguments passed to
  [`interactiveHeatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md)

## Value

output A plotly htmlwidget as produced by
[`interactiveHeatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md)

## Examples

``` r
pcameta <- data.frame(
  row.names = paste0("sample", 1:6),
  treatment = rep(c("control", "treated"), each = 3),
  batch = rep(c("a", "b"), 3)
)
pca_coords <- matrix(rnorm(6 * 4), nrow = 6, dimnames = list(rownames(pcameta), paste0("PC", 1:4)))

plotly_pca_metadata_heatmap(pca_coords, pcameta, fraction_explained = c(45, 25, 20, 10))

{"x":{"data":[{"colorbar":{"ticklen":2,"lenmode":"fraction","xanchor":"left","x":1.1000000000000001,"y":0,"yanchor":"bottom","len":0.29999999999999999,"thickness":30},"colorscale":[["0","rgba(253,231,37,1)"],["0.0416666666666667","rgba(227,228,25,1)"],["0.0833333333333332","rgba(199,224,33,1)"],["0.125","rgba(171,220,50,1)"],["0.166666666666667","rgba(143,215,68,1)"],["0.208333333333333","rgba(117,208,85,1)"],["0.25","rgba(93,200,98,1)"],["0.291666666666667","rgba(72,192,111,1)"],["0.333333333333333","rgba(53,183,121,1)"],["0.375","rgba(40,174,128,1)"],["0.416666666666667","rgba(32,163,134,1)"],["0.458333333333333","rgba(30,154,138,1)"],["0.5","rgba(33,144,141,1)"],["0.541666666666667","rgba(37,134,142,1)"],["0.583333333333333","rgba(40,124,142,1)"],["0.625","rgba(44,113,142,1)"],["0.666666666666667","rgba(49,104,142,1)"],["0.708333333333333","rgba(53,94,141,1)"],["0.75","rgba(59,82,139,1)"],["0.791666666666667","rgba(63,70,136,1)"],["0.833333333333333","rgba(69,58,131,1)"],["0.875","rgba(71,45,123,1)"],["0.916666666666667","rgba(72,31,113,1)"],["0.958333333333333","rgba(71,17,99,1)"],["1","rgba(68,1,84,1)"]],"showscale":false,"z":[[-0.02186212774359696,-2.2017770674010055,-0.39435168296065237,-0.42466027661265676],[-1.5179981219392849,-0.76250132405573134,-0.10796883319560495,-0.11743927753394584]],"x":[1,2,3,4],"y":[1,2],"text":[["Row: batch<br>Column: PC1 (45%)<br>Value: 0.95091","Row: batch<br>Column: PC2 (25%)<br>Value: 0.0062838","Row: batch<br>Column: PC3 (20%)<br>Value: 0.40332","Row: batch<br>Column: PC4 (10%)<br>Value: 0.37613"],["Row: treatment<br>Column: PC1 (45%)<br>Value: 0.030339","Row: treatment<br>Column: PC2 (25%)<br>Value: 0.17278","Row: treatment<br>Column: PC3 (20%)<br>Value: 0.77989","Row: treatment<br>Column: PC4 (10%)<br>Value: 0.76306"]],"showlegend":false,"hoverinfo":"text","type":"heatmap","xaxis":"x","yaxis":"y","frame":null,"xgap":1,"ygap":1},{"x":[1.2,1.2,null,1.2,0,null,1.2,1.2,null,1.2,0],"y":[1.5,1,null,1,1,null,1.5,2,null,2,2],"type":"scatter","mode":"lines","showlegend":false,"hoverinfo":["x","x",null,"x","x",null,"x","x",null,"x","x"],"name":"1","marker":{"color":"rgba(0,0,0,1)","line":{"color":"rgba(0,0,0,1)"}},"textfont":{"color":"rgba(0,0,0,1)"},"error_y":{"color":"rgba(0,0,0,1)"},"error_x":{"color":"rgba(0,0,0,1)"},"line":{"color":"rgba(0,0,0,1)","width":1},"xaxis":"x2","yaxis":"y","frame":null}],"layout":{"xaxis":{"domain":[0,0.79000000000000004],"automargin":true,"tickfont":{"size":10},"tickangle":45,"tickvals":[1,2,3,4],"ticktext":["PC1 (45%)","PC2 (25%)","PC3 (20%)","PC4 (10%)"],"linecolor":"#ffffff","range":[0.5,4.5],"showticklabels":true,"title":"","anchor":"y"},"xaxis2":{"domain":[0.81000000000000005,1],"automargin":true,"title":"","range":[0,1.2],"linecolor":"#ffffff","showticklabels":false,"showgrid":false,"anchor":"y"},"yaxis":{"domain":[0,1],"automargin":true,"title":"","range":[0.5,2.5],"linecolor":"#ffffff","showticklabels":true,"showgrid":false,"anchor":"x","tickfont":{"size":10},"tickangle":0,"tickvals":[1,2],"ticktext":["batch","treatment"]},"annotations":[],"shapes":[],"images":[],"margin":{"b":54,"l":54,"t":0,"r":null},"scene":{"zaxis":{"title":[]}},"hovermode":"closest","showlegend":false,"title":"","legend":{"y":1,"yanchor":"top"}},"attrs":{"291826527f30":{"z":[[-0.02186212774359696,-2.2017770674010055,-0.39435168296065237,-0.42466027661265676],[-1.5179981219392849,-0.76250132405573134,-0.10796883319560495,-0.11743927753394584]],"x":[1,2,3,4],"y":[1,2],"text":[["Row: batch<br>Column: PC1 (45%)<br>Value: 0.95091","Row: batch<br>Column: PC2 (25%)<br>Value: 0.0062838","Row: batch<br>Column: PC3 (20%)<br>Value: 0.40332","Row: batch<br>Column: PC4 (10%)<br>Value: 0.37613"],["Row: treatment<br>Column: PC1 (45%)<br>Value: 0.030339","Row: treatment<br>Column: PC2 (25%)<br>Value: 0.17278","Row: treatment<br>Column: PC3 (20%)<br>Value: 0.77989","Row: treatment<br>Column: PC4 (10%)<br>Value: 0.76306"]],"showlegend":false,"hoverinfo":"text","zmin":null,"zmax":null,"colors":["#FDE725FF","#F7E620FF","#F1E51DFF","#EBE51AFF","#E4E419FF","#DDE318FF","#D7E219FF","#D0E11CFF","#C9E020FF","#C2DF23FF","#BBDE28FF","#B4DE2CFF","#ADDC30FF","#A7DB35FF","#A0DA39FF","#99D83DFF","#92D741FF","#8CD646FF","#85D54AFF","#7FD34EFF","#78D152FF","#73D056FF","#6DCD59FF","#67CC5CFF","#61CA60FF","#5BC863FF","#56C667FF","#51C56AFF","#4CC26CFF","#47C06FFF","#41BE71FF","#3DBC74FF","#39BA76FF","#35B779FF","#31B67BFF","#2EB37CFF","#2BB07FFF","#28AE80FF","#25AC82FF","#24AA83FF","#22A785FF","#20A486FF","#1FA287FF","#1FA088FF","#1F9E89FF","#1E9B8AFF","#1F998AFF","#1F968BFF","#20938CFF","#20928CFF","#218F8DFF","#228D8DFF","#238A8DFF","#24878EFF","#25858EFF","#26828EFF","#26818EFF","#277E8EFF","#287C8EFF","#29798EFF","#2A768EFF","#2B748EFF","#2C718EFF","#2D708EFF","#2E6D8EFF","#2F6B8EFF","#31688EFF","#32658EFF","#33638DFF","#34608DFF","#355E8DFF","#375B8DFF","#38598CFF","#39558CFF","#3B528BFF","#3C508BFF","#3D4D8AFF","#3E4A89FF","#3F4788FF","#404587FF","#424186FF","#433E85FF","#443B84FF","#453882FF","#453581FF","#46327EFF","#472F7CFF","#472C7AFF","#482878FF","#482576FF","#482173FF","#481E70FF","#481B6DFF","#481769FF","#481467FF","#471063FF","#470D60FF","#46085CFF","#450558FF","#440154FF"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"heatmap"},"2918471b339e":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"xend":{},"yend":{},"type":"scatter","mode":"lines","color":{},"showlegend":false,"colors":"1","hoverinfo":"x","inherit":true}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displaylogo":false,"modeBarButtonsToRemove":["sendDataToCloud","select2d","lasso2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","sendDataToCloud"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
