heatmapInput <- function(id, experiment, group_vars, default_groupvar, gene_set_dir,
    gene_set_type_names) {

    ns <- NS(id)

    tagList(div(h5("Clustering"), checkboxInput(ns("cluster_rows"), "Cluster rows?",
        TRUE), checkboxInput(ns("cluster_cols"), "Cluster columns?", FALSE)), radioButtons(ns("scale"),
        "Scale by:", c(Row = "row", Column = "column", None = "none")), selectInput(ns("sampleSelect"),
        "Select samples by", c("name", "group"), selected = "group"), conditionalPanel(condition = paste0("input['",
        ns("sampleSelect"), "'] == 'name' "), checkboxGroupInput(ns("samples"), "Samples:",
        rownames(experiment), selected = rownames(experiment), inline = TRUE)), conditionalPanel(condition = paste0("input['",
        ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"),
        "Selects samples by group defined by:", group_vars, selected = default_groupvar),
        uiOutput(ns("groupSamples"))), checkboxGroupInput(ns("groupVars"), "Variables:",
        group_vars, selected = group_vars, inline = TRUE), h5("Select genes"), selectInput(ns("geneSelect"),
        "Select genes by", c("variance", "list", "gene set"), selected = "variance"),
        conditionalPanel(condition = paste0("input['", ns("geneSelect"), "'] == 'variance' "),
            sliderInput(ns("obs"), "Show top N most variant rows:", min = 10, max = 500,
                value = 50)), conditionalPanel(condition = paste0("input['", ns("geneSelect"),
            "'] == 'list' "), tags$textarea(id = ns("geneList"), rows = 3, cols = 30,
            "Paste gene list here, one per line")), conditionalPanel(condition = paste0("input['",
            ns("geneSelect"), "'] == 'gene set' "), genesetInput(ns("heatmap"), gene_set_dir,
            gene_set_type_names)), downloadButton("downloadHeatMap", "Download Plot")  #,
)

}

heatmapOutput <- function(id) {
    ns <- NS(id)
    plotOutput(ns("heatMap"))
}

heatmap <- function(input, output, session, experiment, expression, annotation, transcriptfield,
    entrezgenefield, genefield, gene_set_dir, gene_set_type_names) {

    geneset_functions <- callModule(geneset, "heatmap", annotation, entrezgenefield,
        genefield, gene_set_dir, gene_set_type_names)

    getPathwayGenes <- geneset_functions$getPathwayGenes
    getPathwayNames <- geneset_functions$getPathwayNames

    # Calculate the plot height

    heatmapPlotheight <- reactive({

        nrows <- 0
        titleheight = 25
        labelsheight = 80

        if (input$geneSelect == "variance") {
            nrows <- input$obs
        } else {
            if (input$geneSelect == "gene set") {
                heatmap_genes <- getPathwayGenes()
                titleheight <- (length(geneset_functions$getPathwayNames()) + 1) *
                  titleheight
            } else {
                heatmap_genes <- unlist(strsplit(input$geneList, "\\n"))
            }
            nrows <- length(heatmap_genes)
        }

        return(titleheight + (length(input$groupVars) * 12) + (nrows * 12) + labelsheight)
    })
    plotwidth <- reactive({
        return(400 + (18 * ncol(expression)))
    })

    # Select out the expression values we need for a heatmap

    getHeatmapExpression <- reactive({

        withProgress(message = "Getting values for heatmap", value = 0, {

            validate(need(!is.null(input$samples), "Waiting for form to provide samples"),
                need(!is.null(input$sampleGroupVar), "Waiting for form to provide groupVar"),
                need(!is.null(input$sampleGroupVal), "Waiting for form to provide sampleGroupVal"))

            heatmap_expression <- expression

            # Select the specified columns

            if (input$sampleSelect == "name") {
                heatmap_expression <- heatmap_expression[, input$samples]
            } else {
                heatmap_expression <- heatmap_expression[, rownames(experiment)[experiment[[input$sampleGroupVar]] %in%
                  input$sampleGroupVal]]
            }

            # Select the specified rows

            if (input$geneSelect == "variance") {
                heatmap_expression <- heatmap_expression[order(apply(heatmap_expression,
                  1, var), decreasing = TRUE)[1:input$obs], ]
            } else {
                if (input$geneSelect == "gene set") {
                  heatmap_genes <- getPathwayGenes()
                } else {
                  heatmap_genes <- unlist(strsplit(input$geneList, "\\n"))
                }

                heatmap_rows <- as.character(annotation[which(tolower(annotation[[genefield]]) %in%
                  tolower(heatmap_genes)), transcriptfield])
                heatmap_expression <- heatmap_expression[rownames(heatmap_expression) %in%
                  heatmap_rows, ]

            }

            # Name the rows

            if (nrow(heatmap_expression) > 0) {

                write(transcriptfield, file = "~shiny/foo.txt")

                rownames(heatmap_expression) <- paste(annotation[match(rownames(heatmap_expression),
                  annotation[[transcriptfield]]), genefield], rownames(heatmap_expression),
                  sep = " / ")

                return(heatmap_expression)
            } else {
                return(NULL)
            }
        })
    })

    # Make a title

    title <- reactive({
        title <- ""
        if (input$geneSelect == "variance") {
            title <- paste(paste("Top", input$obs, "rows"), "by variance")
        } else if (input$geneSelect == "gene set") {
            title <- paste0("Genes in sets:\n", paste(getPathwayNames(), collapse = "\n"))
        } else if (input$geneSelect == "list") {
            title <- "Rows for specifified gene list"
        }
        title
    })

    # Supply a rendered heatmap for display

    output$heatMap <- renderPlot({
        expression <- getHeatmapExpression()
        makeHeatmap(input, expression, experiment, main = title())
    }, height = heatmapPlotheight)

    # Provide the heatmap for download using the following two functions

    plotInput <- reactive({
        makeHeatmap(input, getHeatmapExpression(), experiment, main = title())
    })

    output$downloadHeatMap <- downloadHandler(filename = "heatmap.png", content = function(file) {
        png(file, height = heatmapPlotheight(), width = plotwidth(), units = "px")
        print(plotInput())
        dev.off()
    })

    output$groupSamples <- renderUI({
        group_values <- unique(experiment[[input$sampleGroupVar]])
        ns <- session$ns
        checkboxGroupInput(ns("sampleGroupVal"), "Groups", group_values, selected = group_values)
    })

}



######################################################## Accessory functions

prettifyVariablename <- function(vn) {
    gsub("_", " ", ucfirst(tolower(vn)))
}

ucfirst <- function(string) {
    paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
}

# Just an adapter to extract bits of the input object to pass to the more
# generically useful annotated_heatmap function

makeHeatmap <- function(input, heatmap_expression, experiment, ...) {

    if (!is.null(heatmap_expression)) {

        annotated_heatmap(log2(heatmap_expression + 1), experiment, group_vars = input$groupVars,
            cluster_rows = input$cluster_rows, cluster_cols = input$cluster_cols,
            scale = input$scale, ...)
    }
}

annotated_heatmap <- function(plotmatrix, sample_annotation, group_vars = NULL, ...) {

    if (!is.null(plotmatrix)) {

        require(pheatmap)
        sample_annotation <- data.frame(apply(sample_annotation[colnames(plotmatrix),
            rev(group_vars), drop = F], 2, as.factor))

        colors <- make_annotation_colors(sample_annotation)

        pheatmap(plotmatrix, show_rownames = T, fontsize = 12, fontsize_row = 10,
            cellheight = 12, annotation = sample_annotation, annotation_colors = colors,
            border_color = NA, legend = FALSE, ...)
    }
}

make_annotation_colors <- function(sample_annotation) {

    palettes <- rep(c("Set1", "Set2", "Set3", "Dark2", "Accent"), 100)

    colors = list()
    colors_so_far = 0
    for (i in 1:ncol(sample_annotation)) {
        if (!is.factor(sample_annotation[, i])) {
            sample_annotation[, i] <- as.factor(sample_annotation[, i])
        }

        categories <- unique(as.numeric(sample_annotation[, i]))

        # If the palette is longer than the number of colors required, then we're good,
        # otherwise we have to do some interpolation

        if (brewer.pal.info[palettes[i], "maxcolors"] >= length(categories) && length(categories) >
            2) {
            colcolors <- brewer.pal(length(categories), palettes[i])
        } else {
            colcolors <- sample(colorRampPalette(brewer.pal(brewer.pal.info[palettes[i],
                "maxcolors"], palettes[i]))(length(categories)), length(categories))
        }

        names(colcolors) <- levels(sample_annotation[, i])

        colors[[colnames(sample_annotation)[i]]] <- colcolors
    }
    colors
}
