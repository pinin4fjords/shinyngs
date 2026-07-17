#' shinyngs' fixed categorical colour palette
#'
#' An Okabe & Ito (2008) colour-blind-safe categorical palette, in the order
#' that keeps colour-vision-deficiency separation highest between
#' neighbouring entries. The black swatch of the original palette is
#' replaced with a mid grey so it stays visible against the app's dark theme
#' background as well as the light one.
#'
#' @keywords internal
COLORBLIND_PALETTE <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#595959" # neutral grey
)

# Picker value selecting the fixed colour-blind-safe palette rather than an
# RColorBrewer one. The selectInput choice and the makeColorScale branch check
# must agree on this string, so it is defined once.
COLORBLIND_PALETTE_NAME <- "colorblind"

#' The input function of the colorby module
#'
#' Provides a drop-down for picking the plot colour palette. The default is
#' shinyngs' fixed colour-blind-safe palette (see \code{\link{makeColorScale}});
#' the remaining choices are RColorBrewer qualitative palettes for users who
#' prefer them.
#'
#' This function provides the form elements to control the display
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

  palettes <- c(
    `Colour-blind safe` = COLORBLIND_PALETTE_NAME,
    Dark2 = "Dark2", Set1 = "Set1", Set2 = "Set2", Set3 = "Set3",
    Pastel1 = "Pastel1", Pastel2 = "Pastel2", Paired = "Paired", Accent = "Accent"
  )

  selectInput(inputId = ns("palette_name"), label = "Color palette", choices = palettes, selected = COLORBLIND_PALETTE_NAME)
}

#' The output function of the colorby module
#'
#' Supplies a reactive returning the palette selected in the drop-down, sized
#' to the number of categories the caller reports (see
#' \code{\link{makeColorScale}}).
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param getNumberCategories A reactive supplying the number of categories
#' that require a color.
#'
#' @return output A reactive returning a character vector of hex colors
#'
#' @keywords shiny
#'
#' @examples
#' colormaker("myid", getNumberCategories)
#'
colormaker <- function(id, getNumberCategories) {
  moduleServer(id, function(input, output, session) {
    getPaletteName <- reactive({
      validate(need(!is.null(input$palette_name), "Waiting for palette"))
      input$palette_name
    })

    reactive({
      makeColorScale(getNumberCategories(), palette = getPaletteName())
    })
  })
}

#' Make a categorical colour scale of a specified length
#'
#' With the default \code{palette = "colorblind"} (or \code{NULL}), returns
#' colours drawn, in order, from shinyngs' fixed colour-blind-safe categorical
#' palette (\code{\link{COLORBLIND_PALETTE}}), so the same position always gets
#' the same colour and a group keeps its colour across plots; when more colours
#' are requested than the base palette provides, additional shades are
#' interpolated between them and a message is emitted, since colour-blind
#' separation can no longer be guaranteed for every pair. Any other value is
#' treated as an RColorBrewer palette name and expanded with interpolation
#' where necessary.
#'
#' @param ncolors Integer specifying the number of colors
#' @param palette \code{"colorblind"} (or \code{NULL}) for the colour-blind-safe
#'   palette, otherwise an RColorBrewer palette name. (default: 'colorblind')
#'
#' @return output Character vector of colors
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' makeColorScale(10)
#'
makeColorScale <- function(ncolors, palette = COLORBLIND_PALETTE_NAME) {
  if (is.null(palette) || palette == COLORBLIND_PALETTE_NAME) {
    if (ncolors > length(COLORBLIND_PALETTE)) {
      message(
        "makeColorScale: ", ncolors, " categories requested, more than the ",
        length(COLORBLIND_PALETTE), " colours in shinyngs' colour-blind-safe ",
        "palette. Interpolating additional shades, which will be harder to ",
        "tell apart than the base palette."
      )
      return(colorRampPalette(COLORBLIND_PALETTE)(ncolors))
    }

    return(COLORBLIND_PALETTE[seq_len(ncolors)])
  }

  paletteinfo <- RColorBrewer::brewer.pal.info

  if (ncolors > paletteinfo[palette, "maxcolors"]) {
    cols <- colorRampPalette(RColorBrewer::brewer.pal(paletteinfo[palette, "maxcolors"], palette))(ncolors)
  } else if (ncolors < 3) {
    cols <- colorRampPalette(RColorBrewer::brewer.pal(paletteinfo[palette, "maxcolors"], palette))(3)
    cols <- cols[seq_len(ncolors)]
  } else {
    cols <- RColorBrewer::brewer.pal(ncolors, palette)
  }
  rev(cols)
}
