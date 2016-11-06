colormakerInput <- function(id) {

  ns <- NS(id)
  
  palettes <- c('Dark2', 'Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Accent')
  
  selectInput(inputId = ns('palette_name'), label = 'Color palette', choices = palettes, selected = 'Dark2')
}

#' @param ncolors Reactive returning an integer specifying the number of colors
#'   to generate.

colormaker <- function(input, output, session, eselist, getNumberCategories) {
 
  getPaletteName <- reactive({
    validate(need(! is.null(input$palette_name), 'Waiting for palette'))
    input$palette_name
  })
  
  reactive({
    palette <- getPaletteName()
    n_colors <- getNumberCategories()
    
    makeColorScale(n_colors, palette)
  })
   
}

#' Make a color palette of a specified length
#' 
#' Given an integer, make a palette with a specified number of colors using
#' palettes from RColorBrewer, and interpolation where necessary.
#'
#' @param ncolors Integer specifying the number of colors
#' @param palette RColorBrewer palette name. (default: 'Set1')
#'
#' @return output Character vector of colors
#' @export
#'
#' @examples
#' makeColorScale(10)
#' [1] '#999999' '#EC83BA' '#B75F49' '#E1C62F' '#FFB716' '#D16948' '#7E6E85' '#48A462' '#4A72A6' '#E41A1C'

makeColorScale <- function(ncolors, palette = "Dark2") {
  
  paletteinfo <- RColorBrewer::brewer.pal.info
  
  if (ncolors > paletteinfo["Set1", "maxcolors"]) {
    cols <- colorRampPalette(RColorBrewer::brewer.pal(paletteinfo[palette, "maxcolors"], palette))(ncolors)
  } else if (ncolors < 3) {
    cols <- colorRampPalette(RColorBrewer::brewer.pal(paletteinfo[palette, "maxcolors"], palette))(3)
    cols[1:ncolors]
  } else {
    cols <- RColorBrewer::brewer.pal(ncolors, palette)
  }
  rev(cols)
}