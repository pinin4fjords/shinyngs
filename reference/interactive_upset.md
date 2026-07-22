# Make an UpSet-style set intersection plot with `plot_ly()`

A reimplementation of the `upset` tool of Lex, Gehlenborg et al: a bar
chart of set sizes, a bar chart of intersection sizes, and a grid
showing which sets make up each intersection - drawn with `plotly`
rather than base graphics so it stays interactive outside of a fixed set
count, and so all elements of a given intersection can be plotted
(rather than assigning every item to its highest-order intersection).

## Usage

``` r
interactive_upset(
  sets,
  nintersects = 20,
  minorder = 1,
  set_sort = TRUE,
  bar_numbers = FALSE,
  show_empty_intersections = TRUE,
  intersection_assignment_type = c("upset", "all")
)
```

## Arguments

- sets:

  A named list of character vectors, one per set (e.g. up/down
  differential gene sets per contrast)

- nintersects:

  Maximum number of intersections to display, ordered by size

- minorder:

  Minimum number of sets that must be involved in an intersection for it
  to be shown

- set_sort:

  Sort sets by size (ascending) before plotting?

- bar_numbers:

  Add value labels above the intersection size bars?

- show_empty_intersections:

  Include intersections/set combinations with zero members?

- intersection_assignment_type:

  'upset' assigns each member to its highest-order intersection only (as
  in the original UpSet); 'all' counts a member in every intersection it
  belongs to

## Value

output A plotly htmlwidget

## References

Lex and Gehlenborg (2014). Points of view: Sets and intersections.
\<em\>Nature Methods\</em\> 11, 779 (2014).
[http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html](http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.md)

Gehlenborg N (2016). \<em\>UpSetR: A More Scalable Alternative to Venn
and Euler Diagrams for Visualizing Intersecting Sets\</em\>. R package
version 1.3.0, <https://CRAN.R-project.org/package=UpSetR>

## Examples

``` r
sets <- list(a = paste0("gene", 1:6), b = paste0("gene", 4:10), c = paste0("gene", 8:12))
interactive_upset(sets)
#> Warning: Ignoring 13 observations

{"x":{"data":[{"mode":"markers","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y4","frame":null},{"mode":"markers","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x2","yaxis":"y3","frame":null},{"mode":"markers","type":"scatter","marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x3","yaxis":"y2","frame":null},{"x":[5,6,7],"y":["c","a","b"],"orientation":"h","marker":{"color":"black","line":{"color":"rgba(214,39,40,1)"}},"type":"bar","error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"xaxis":"x4","yaxis":"y","frame":null},{"showlegend":false,"x":[1,2,3,4,5,6,7],"y":[3,3,3,2,1,0,0],"type":"bar","marker":{"color":"black","hoverinfo":"none","line":{"color":"rgba(148,103,189,1)"}},"error_y":{"color":"rgba(148,103,189,1)"},"error_x":{"color":"rgba(148,103,189,1)"},"xaxis":"x5","yaxis":"y6","frame":null},{"mode":"markers","marker":{"color":"lightgrey","size":8,"line":{"color":"rgba(140,86,75,1)"}},"type":"scatter","error_y":{"color":"rgba(140,86,75,1)"},"error_x":{"color":"rgba(140,86,75,1)"},"line":{"color":"rgba(140,86,75,1)"},"xaxis":"x5","yaxis":"y5","frame":null},{"mode":"markers","marker":{"color":"lightgrey","size":8,"line":{"color":"rgba(227,119,194,1)"}},"type":"scatter","x":[1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7],"y":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5],"hoverinfo":["none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none"],"error_y":{"color":"rgba(227,119,194,1)"},"error_x":{"color":"rgba(227,119,194,1)"},"line":{"color":"rgba(227,119,194,1)"},"xaxis":"x5","yaxis":"y5","frame":null},{"mode":"lines+markers","marker":{"color":"black","size":10,"line":{"color":"rgba(127,127,127,1)"}},"type":"scatter","x":[1,1,null,2,2,null,3,3,null,4,4,null,5,5,null,6,6,null,7,7,7],"y":[1.5,1.5,null,2.5,0.5,null,1.5,0.5,null,2.5,2.5,null,0.5,0.5,null,2.5,1.5,null,2.5,1.5,0.5],"line":{"color":"black","width":3},"hoverinfo":["text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text",null,"text","text","text"],"text":["a","a",null,"c","b",null,"a","b",null,"c","c",null,"b","b",null,"c","a",null,"c","a","b"],"error_y":{"color":"rgba(127,127,127,1)"},"error_x":{"color":"rgba(127,127,127,1)"},"xaxis":"x5","yaxis":"y5","frame":null}],"layout":{"xaxis":{"domain":[0,0.16239999999999996],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"y4"},"xaxis2":{"domain":[0.17359999999999998,0.27999999999999997],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"y3"},"xaxis3":{"domain":[0,0.16239999999999996],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"y2"},"xaxis4":{"domain":[0.17359999999999998,0.27999999999999997],"automargin":true,"anchor":"y"},"xaxis5":{"domain":[0.32000000000000001,1],"automargin":true,"anchor":"y5","showticklabels":false,"showgrid":false,"zeroline":false},"yaxis5":{"domain":[0,0.47999999999999998],"automargin":true,"showticklabels":false,"showgrid":false,"range":[0,3],"zeroline":false,"anchor":"x5"},"yaxis6":{"domain":[0.52000000000000002,1],"automargin":true,"anchor":"x5"},"yaxis":{"domain":[0,0.47999999999999998],"automargin":true,"categoryarray":["b","a","c"],"categoryorder":"array","type":"category","anchor":"x4"},"yaxis2":{"domain":[0,0.47999999999999998],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"x3"},"yaxis3":{"domain":[0.52000000000000002,1],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"x2"},"yaxis4":{"domain":[0.52000000000000002,1],"automargin":true,"showticklabels":false,"showgrid":false,"zeroline":false,"anchor":"x"},"annotations":[],"shapes":[],"images":[],"margin":{"b":40,"l":60,"t":0,"r":10},"hovermode":"closest","showlegend":false,"bargap":0.40000000000000002},"attrs":{"2903135dc472":{"mode":"markers","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"2903526b3ae2":{"mode":"markers","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"29034ef25882":{"mode":"markers","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"290373c4b14e":{"x":[5,6,7],"y":["c","a","b"],"orientation":"h","marker":{"color":"black"},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"2903538c7c16":{"showlegend":false,"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],"y":[3,3,3,2,1,0,0,null,null,null,null,null,null,null,null,null,null,null,null,null],"type":"bar","marker":{"color":"black","hoverinfo":"none"},"inherit":true},"2903ad3959f":{"mode":"markers","marker":{"color":"lightgrey","size":8},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"2903ad3959f.1":{"mode":"markers","marker":{"color":"lightgrey","size":8},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","x":[1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7],"y":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5],"hoverinfo":"none","inherit":true},"290319b6dc80":{"mode":"lines+markers","marker":{"color":"black","size":10},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter","x":[1,1,2,2,3,3,4,4,5,5,6,6,7,7,7],"y":[1.5,1.5,2.5,0.5,1.5,0.5,2.5,2.5,0.5,0.5,2.5,1.5,2.5,1.5,0.5],"line":{"color":"black","width":3},"hoverinfo":"text","text":{},"inherit":true}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
