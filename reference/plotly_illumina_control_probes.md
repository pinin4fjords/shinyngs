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
plotly_illumina_control_probes(
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

plotly_illumina_control_probes(control_annotation, controls)

{"x":{"visdat":{"29c359696606":["function () ","plotlyVisDat"]},"cur_data":"29c359696606","attrs":{"29c359696606":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","color":{},"colors":["red","red","red","orange","orange","black","purple","blue","green"],"linetype":{},"linetypes":["dot","dash","solid","dash","solid","solid","solid","solid","solid"],"inherit":true}},"layout":{"margin":{"b":200,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"categoryarray":["sample1","sample2","sample3"],"categoryorder":"array","title":"","type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Intensity"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["sample1","sample2","sample3"],"y":[724.00222067348659,741.33757006749511,758.34494933462702],"type":"scatter","mode":"lines","name":"cy3_low","line":{"color":"rgba(255,0,0,1)","dash":"dot"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[338.27771816402674,605.6981430829037,724.77439377596602],"type":"scatter","mode":"lines","name":"cy3_med","line":{"color":"rgba(255,0,0,1)","dash":"dash"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[630.78370836027898,903.17852988583036,943.78109347657301],"type":"scatter","mode":"lines","name":"cy3_high","line":{"color":"rgba(255,0,0,1)","dash":"solid"},"marker":{"color":"rgba(255,0,0,1)","line":{"color":"rgba(255,0,0,1)"}},"textfont":{"color":"rgba(255,0,0,1)"},"error_y":{"color":"rgba(255,0,0,1)"},"error_x":{"color":"rgba(255,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[840.77393945259973,294.43642494408414,548.09894042904489],"type":"scatter","mode":"lines","name":"low_stringency_pm","line":{"color":"rgba(255,165,0,1)","dash":"dash"},"marker":{"color":"rgba(255,165,0,1)","line":{"color":"rgba(255,165,0,1)"}},"textfont":{"color":"rgba(255,165,0,1)"},"error_y":{"color":"rgba(255,165,0,1)"},"error_x":{"color":"rgba(255,165,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[856.27553304540925,192.06884977850132,712.03212385554798],"type":"scatter","mode":"lines","name":"low_stringency_mm","line":{"color":"rgba(255,165,0,1)","dash":"solid"},"marker":{"color":"rgba(255,165,0,1)","line":{"color":"rgba(255,165,0,1)"}},"textfont":{"color":"rgba(255,165,0,1)"},"error_y":{"color":"rgba(255,165,0,1)"},"error_x":{"color":"rgba(255,165,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[391.96792174666189,886.56449236092158,389.51619472610764],"type":"scatter","mode":"lines","name":"negative","line":{"color":"rgba(0,0,0,1)","dash":"solid"},"marker":{"color":"rgba(0,0,0,1)","line":{"color":"rgba(0,0,0,1)"}},"textfont":{"color":"rgba(0,0,0,1)"},"error_y":{"color":"rgba(0,0,0,1)"},"error_x":{"color":"rgba(0,0,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[381.11339171906002,503.83614624524489,101.77225305652246],"type":"scatter","mode":"lines","name":"biotin","line":{"color":"rgba(160,32,240,1)","dash":"solid"},"marker":{"color":"rgba(160,32,240,1)","line":{"color":"rgba(160,32,240,1)"}},"textfont":{"color":"rgba(160,32,240,1)"},"error_y":{"color":"rgba(160,32,240,1)"},"error_x":{"color":"rgba(160,32,240,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[895.54998056869954,877.18048548139632,927.3747864998877],"type":"scatter","mode":"lines","name":"labeling","line":{"color":"rgba(0,0,255,1)","dash":"solid"},"marker":{"color":"rgba(0,0,255,1)","line":{"color":"rgba(0,0,255,1)"}},"textfont":{"color":"rgba(0,0,255,1)"},"error_y":{"color":"rgba(0,0,255,1)"},"error_x":{"color":"rgba(0,0,255,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["sample1","sample2","sample3"],"y":[644.67144714808092,190.0044288195204,283.94926781067625],"type":"scatter","mode":"lines","name":"housekeeping","line":{"color":"rgba(0,255,0,1)","dash":"solid"},"marker":{"color":"rgba(0,255,0,1)","line":{"color":"rgba(0,255,0,1)"}},"textfont":{"color":"rgba(0,255,0,1)"},"error_y":{"color":"rgba(0,255,0,1)"},"error_x":{"color":"rgba(0,255,0,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
