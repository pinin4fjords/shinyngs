#' Make UI and server functions for apps based on supplied data
#'
#' Draws on various components (heatmaps, tables etc) to produce the UI and server
#' components of a variety of shiny apps, based on the type and data specified.
#'
#'
#' @param type A string specifying the type of shiny app required (options: heatmap)
#' @param params A list containing data and display options for the Shiny app
#'
#' @return output A list of length 2 containing: the UI and server components
#'
#' @keywords shiny
#'
#' @export
#'
#' @examples
#' app <- prepareApp("heatmap", params)
#' shinyApp(app$ui, app$server)


prepareApp <- function(type, params = list()) {

    if (type == "heatmap") {

        source("heatmap.R")

        ui <- fluidPage(useShinyjs(), navbarPage(id = "pages", title = "Heatmap builder:",
            tabPanel("Home", heatmapLayout(params))))

        server <- function(input, output, session) {
            heatmapModuleCall()
        }

    }

    return(list(ui = ui, server = server))

}

heatmapLayout <- function(params) {
    sidebarLayout(sidebarPanel(heatmapInput("heatmap", params$experiment, params$group_vars,
        params$default_groupvar, params$gene_set_dir, params$gene_set_type_names)),
        mainPanel(heatmapOutput("heatmap")))
}

heatmapModuleCall <- function(params) {
    callModule(heatmap, "heatmap", params$experiment, params$expression, params$annotation,
        params$transcriptfield, params$entrezgenefield, params$genefield, params$gene_set_dir,
        params$gene_set_type_names)
}
