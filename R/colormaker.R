#' The input function of the colorby module
#'
#' This module provides a drop-down for picking an RColorBrewer color palette
#' and provides that palette given a reactive which supplied the required
#' number of colors.
#'
#' This funcion provides the form elements to control the display
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' colormakerInput("myid")
#'
colormakerInput <- function(id) {
  ns <- NS(id)

  palettes <- c("Dark2", "Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Accent")

  selectInput(inputId = ns("palette_name"), label = "Color palette", choices = palettes, selected = "Dark2")
}

#' The output function of the colorby module
#'
#' This module provides a drop-down for picking an RColorBrewer color palette
#' and provides that palette given a reactive which supplied the required
#' number of colors.
#'
#' This function is not called directly, but rather via callModule() (see
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getNumberCategories A reactive supplying the number of categories
#' that require a color.
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' callModule(colormaker, "myid", getNumberCategories)
#'
colormaker <- function(input, output, session, getNumberCategories) {
  getPaletteName <- reactive({
    validate(need(!is.null(input$palette_name), "Waiting for palette"))
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
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' makeColorScale(10)
#'
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
