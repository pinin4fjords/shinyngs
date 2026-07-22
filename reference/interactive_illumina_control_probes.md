# Make an Illumina microarray control-probe QC plot with `plot_ly()`

Averages a fixed set of Illumina control probe groups (cy3 dilution
series, stringency, negative, biotin, labeling and housekeeping
controls) across samples and plots them as lines, following the standard
QC relationships between groups (see
[`illuminaarrayqcInput`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarrayqcInput.md)'s
displayed checklist: cy3_high \> cy3_med \> cy3_low, low stringency =~
0, high stringency \<= cy3 high, housekeeping/biotin = high, negative =~
0).

## Usage

``` r
interactive_illumina_control_probes(
  control_annotation,
  controls,
  sample_order = NULL
)
```

## Arguments

- control_annotation:

  Data frame of control probe annotation, with an `Array_Address_Id`
  column matching the row names of `controls` and a `Reporter_Group_id`
  column identifying each control probe group

- controls:

  Matrix of control probe intensities, control probes (by
  `Array_Address_Id`) by row, samples by column

- sample_order:

  Character vector giving the sample (x axis) display order. Defaults to
  `colnames(controls)`

## Value

output A plotly htmlwidget

## Examples

``` r
control_annotation <- data.frame(
  Array_Address_Id = paste0("probe", 1:9),
  Reporter_Group_id = c(
    "phage_lambda_genome:low", "phage_lambda_genome:med", "phage_lambda_genome:high",
    "phage_lambda_genome:pm", "phage_lambda_genome:mm2", "permuted_negative",
    "phage_lambda_genome", "thrB", "housekeeping"
  )
)
controls <- matrix(runif(9 * 3, 1, 1000),
  nrow = 9, dimnames = list(control_annotation$Array_Address_Id, paste0("sample", 1:3))
)

interactive_illumina_control_probes(control_annotation, controls)

{"x":{"visdat":{"29c846484ca6":["function () ","plotlyVisDat"]},"cur_data":"29c846484ca6","attrs":{"29c846484ca6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","color":{},"colors":["red","red","red","orange","orange","black","purple","blue","green"],"linetype":{},"linetypes":["dot","dash","solid","dash","solid","solid","solid","solid","solid"],"inherit":true}},"layout":{"margin":{"b":200,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"categoryarray":["sample1","sample2","sample3"],"categoryorder":"array","title":"","type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Intensity"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["sample1","sample2","sample3"],"y":[649.92988103930838,511.44873865460977,390.68885474302806],"type":"scatter","mode":"lines","name":"cy3_low","line":{"color":"rgba(255,0,0,1)","dash":"dot"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[953.40209566336125,258.36363956471905,689.9382209496107],"type":"scatter","mode":"lines","name":"cy3_med","line":{"color":"rgba(255,0,0,1)","dash":"dash"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[953.77891737851314,47.414426064584404,689.72399906162173],"type":"scatter","mode":"lines","name":"cy3_high","line":{"color":"rgba(255,0,0,1)","dash":"solid"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[340.63922421447933,418.43840208183974,555.34572253632359],"type":"scatter","mode":"lines","name":"low_stringency_pm","line":{"color":"rgba(255,165,0,1)","dash":"dash"},"marker":{"color":"rgba(255,165,0,1)","line":{"color":"rgba(255,165,0,1)"}},"textfont":{"color":"rgba(255,165,0,1)"},"error_y":{"color":"rgba(255,165,0,1)"},"error_x":{"color":"rgba(255,165,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[263.21163600706495,854.14750077133067,430.1947833772283],"type":"scatter","mode":"lines","name":"low_stringency_mm","line":{"color":"rgba(255,165,0,1)","dash":"solid"},"marker":{"color":"rgba(255,165,0,1)","line":{"color":"rgba(255,165,0,1)"}},"textfont":{"color":"rgba(255,165,0,1)"},"error_y":{"color":"rgba(255,165,0,1)"},"error_x":{"color":"rgba(255,165,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[166.28847924619913,347.88344704802148,453.26734274439514],"type":"scatter","mode":"lines","name":"negative","line":{"color":"rgba(0,0,0,1)","dash":"solid"},"marker":{"color":"rgba(0,0,0,1)","line":{"color":"rgba(0,0,0,1)"}},"textfont":{"color":"rgba(0,0,0,1)"},"error_y":{"color":"rgba(0,0,0,1)"},"error_x":{"color":"rgba(0,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[322.84588856366463,132.31087821256369,307.13681562826969],"type":"scatter","mode":"lines","name":"biotin","line":{"color":"rgba(160,32,240,1)","dash":"solid"},"marker":{"color":"rgba(160,32,240,1)","line":{"color":"rgba(160,32,240,1)"}},"textfont":{"color":"rgba(160,32,240,1)"},"error_y":{"color":"rgba(160,32,240,1)"},"error_x":{"color":"rgba(160,32,240,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[510.61508134403266,375.11237767990679,578.77559005771764],"type":"scatter","mode":"lines","name":"labeling","line":{"color":"rgba(0,0,255,1)","dash":"solid"},"marker":{"color":"rgba(0,0,255,1)","line":{"color":"rgba(0,0,255,1)"}},"textfont":{"color":"rgba(0,0,255,1)"},"error_y":{"color":"rgba(0,0,255,1)"},"error_x":{"color":"rgba(0,0,255,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[924.04450288251974,631.78880816907622,910.45993394497782],"type":"scatter","mode":"lines","name":"housekeeping","line":{"color":"rgba(0,255,0,1)","dash":"solid"},"marker":{"color":"rgba(0,255,0,1)","line":{"color":"rgba(0,255,0,1)"}},"textfont":{"color":"rgba(0,255,0,1)"},"error_y":{"color":"rgba(0,255,0,1)"},"error_x":{"color":"rgba(0,255,0,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
