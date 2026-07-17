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

#' The input function of the colorby module
#'
#' shinyngs colours every plot from the single, fixed colour-blind-safe
#' palette in \code{\link{makeColorScale}}, so there is no per-plot palette
#' choice to expose. Modules that build their form from a list of inputs can
#' include this call unconditionally; it contributes no UI element.
#'
#' @param id Submodule namespace
#'
#' @return output NULL
#'
#' @keywords shiny
#'
#' @examples
#' colormakerInput("myid")
#'
colormakerInput <- function(id) {
  NULL
}

#' The output function of the colorby module
#'
#' Supplies a reactive returning shinyngs' fixed colour-blind-safe palette
#' (see \code{\link{makeColorScale}}), sized to the number of categories the
#' caller reports.
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
    reactive({
      makeColorScale(getNumberCategories())
    })
  })
}

#' Make a colour-blind-safe categorical colour scale of a specified length
#'
#' Returns colours drawn, in order, from shinyngs' fixed colour-blind-safe
#' categorical palette (\code{\link{COLORBLIND_PALETTE}}), so the same
#' position always gets the same colour and a group keeps its colour across
#' plots. When more colours are requested than the base palette provides,
#' additional shades are interpolated between them and a message is emitted,
#' since colour-blind separation can no longer be guaranteed for every pair.
#'
#' @param ncolors Integer specifying the number of colors
#' @param palette Ignored. Accepted so callers can pass a palette name
#'   argument without it being an error.
#'
#' @return output Character vector of colors
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' makeColorScale(10)
#'
makeColorScale <- function(ncolors, palette = NULL) {
  if (ncolors > length(COLORBLIND_PALETTE)) {
    message(
      "makeColorScale: ", ncolors, " categories requested, more than the ",
      length(COLORBLIND_PALETTE), " colours in shinyngs' colour-blind-safe ",
      "palette. Interpolating additional shades, which will be harder to ",
      "tell apart than the base palette."
    )
    return(colorRampPalette(COLORBLIND_PALETTE)(ncolors))
  }

  COLORBLIND_PALETTE[seq_len(ncolors)]
}
