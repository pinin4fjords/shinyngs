


valid_sets <- readRDS("/tmp/valid_sets.rds")
nsets <- length(valid_sets)
nintersections <- 20
setnames <- sub('vs', '<br />vs ', gsub('_', ' ', names(valid_sets)))
upset <- 1

combinations <- function(items, pick){
  x <- combn(items, pick)
  lapply(seq_len(ncol(x)), function(i) x[,i])
}

        
combos <- lapply(1:nsets, function(x){
  combinations(1:length(valid_sets), x)
})

intersects <- lapply(combos, function(combonos){
  lapply(combonos, function(combo){
    Reduce(intersect, valid_sets[combo])
  })
})



  # For UpSet-ness, intersections between larger numbers of sets take ownership over
  # their members
  
  intersects <- lapply(1:length(intersects), function(i){
      intersectno <- intersects[[i]]
      members_in_higher_levels <- unlist(intersects[(i+1):length(intersects)])
      lapply(intersectno, function(intersect){
        if (upset ==1){
          length(setdiff(intersect, members_in_higher_levels))
        }else{
          length(intersect) 
        }
      })
  })


combos <- unlist(combos, recursive = FALSE)
intersects <- unlist(intersects)

combos <- combos[which(intersects > 0)]
intersects <- intersects[which(intersects >0)]


# Sort by intersect size
        
combos <- combos[order(intersects, decreasing = TRUE)]
intersects <- intersects[order(intersects, decreasing = TRUE)]





lines <- data.table::rbindlist(lapply(1:nintersections, function(combono){
  data.frame(combo = combono, x = rep(combono, max(2,length(combos[[combono]]))), y = (nsets-combos[[combono]])+1, name = setnames[combos[[combono]]])
}))

grid <- plot_ly(type = 'scatter', mode = 'markers', marker = list (color = 'lightgrey', size = 10)) %>% add_trace(x = rep(1:nintersections, length(valid_sets)), y =unlist(lapply(1:length(valid_sets), function(x) rep(x, nintersections))), hoverinfo = 'none') %>% add_trace(data = group_by(lines, combo), mode = 'lines+markers', x = lines$x, y = lines$y, line = list(color = 'black', width =3), marker = list(color = 'black', size = 12), hoverinfo = 'text', text = ~ name)%>% layout(xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE), yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE), margin = list(l=0, b = 0), showlegend = FALSE)


setsize <- plot_ly(x = unlist(lapply(valid_sets, length)), y = setnames, type = 'bar', orientation = 'h', marker = list(color = 'black')) %>% layout(margin = list(l = 300), bargap = 0.4, yaxis = list(categoryarray = rev(setnames), categoryorder = "array"))

intersectsize <- plot_ly(x = 1:nintersections, y = unlist(intersects[1:nintersections]), type = 'bar', marker = list(color = 'black'), hoverinfo = 'none') %>% layout(margin = list(b = 0), bargap = 0.4, xaxis = list(showticklabels = FALSE))

subplot(subplot(plotly_empty(), setsize, nrows = 2), subplot(intersectsize, grid, nrows = 2, shareX = TRUE), widths = c(0.33, 0.66)) %>% layout(margin = list(l = 200, b = 20), showlegend = FALSE)

 