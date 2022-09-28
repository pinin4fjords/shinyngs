#' The input function of the upset module
#'
#' This module illustrates the intersection of differential sets using a
#' reimplementation of the \code{\link[UpSetR]{upset}} tool of Lex,
#' Gehlenborg et al. The reimplementation was done to allow use of more dynamic
#' components, and to allow plotting of all elements in given intersections
#' (rather than assigning every item to its highest-order intersection). It
#' also seems to have sped things up.
#'
#' This function provides the form elements to control the display
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @references
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#'
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'
#' @examples
#' upsetInput("myid", eselist)
#'
upsetInput <- function(id, eselist) {
  ns <- NS(id)

  upset_fields <- list(
    uiOutput(ns("nsets")), sliderInput(ns("nintersects"), label = "Number of intersections", min = 2, max = 40, step = 1, value = 20), uiOutput(ns("minorder")),
    checkboxInput(ns("separate_by_direction"), label = "Separate by direction of change?", value = TRUE), checkboxInput(ns("set_sort"), "Sort sets by size?", value = TRUE), checkboxInput(ns("bar_numbers"), "Show bar numbers?", value = FALSE), checkboxInput(ns("show_empty_intersections"),
      label = "Show empty intersections?", value = TRUE
    ), selectInput(ns("intersection_assignment_type"), "Intersection type", choices = c(
      `Highest-order (UpSet)` = "upset",
      `Complete` = "all"
    ), selected = "upset")
  )

  fieldSets(ns("fieldset"), list(
    intersections = upset_fields, expression = selectmatrixInput(ns("upset"), eselist), contrasts = contrastsInput(ns("upset")),
    export = plotdownloadInput(ns("upset"), "UpSet Plot")
  ))
}

#' The output function of the clustering module
#'
#' This module illustrates the intersection of differential sets using a
#' reimplementation of the \code{\link[UpSetR]{upset}} tool of Lex,
#' Gehlenborg et al. The reimplementation was done to allow use of more dynamic
#' components, and to allow plotting of all elements in given intersections
#' (rather than assigning every item to its highest-order intersection). It
#' also seems to have sped things up.
#'
#' This function provides the form elements to control the display
#'
#' This provides actual plot element for display by applications
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @references
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#'
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'
#' @examples
#' upsetOutput("myid", eselist)
#'
upsetOutput <- function(id, eselist) {
  ns <- NS(id)

  list(
    modalInput(ns("upset"), "help", "help"), modalOutput(ns("upset"), "Intersection plots with UpSet", includeMarkdown(system.file("inlinehelp", "upset.md",
      package = packageName()
    ))), h3("Intersection of differential sets"), uiOutput(ns("subset_notice")), plotlyOutput(ns("plotly_upset"), height = "600px"),
    h4("Differential set summary"), uiOutput(ns("differential_parameters")), simpletableOutput(ns("upset"))
  )
}

#' The server function of the upstart module
#'
#' This module illustrates the intersection of differential sets using a
#' reimplementation of the \code{\link[UpSetR]{upset}} tool of Lex,
#' Gehlenborg et al. The reimplementation was done to allow use of more dynamic
#' components, and to allow plotting of all elements in given intersections
#' (rather than assigning every item to its highest-order intersection). It
#' also seems to have sped things up.
#'
#' This function is not called directly, but rather via callModule() (see
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param setlimit Maximum number of sets
#'
#' @keywords shiny
#'
#' @references
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#'
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'
#' @examples
#' callModule(upstart, "myid", eselist)
#'
upset <- function(input, output, session, eselist, setlimit = 16) {
  ns <- session$ns

  # Call the selectmatrix module and unpack the reactives it sends back

  selectmatrix_reactives <- callModule(selectmatrix, "upset", eselist,
    var_n = 1000, select_samples = FALSE, select_genes = TRUE, provide_all_genes = TRUE,
    select_meta = FALSE
  )
  unpack.list(selectmatrix_reactives)

  # Pass the matrix to the contrasts module for processing

  unpack.list(callModule(contrasts, "upset", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = TRUE, select_all_contrasts = TRUE))

  ############################################################################# Render dynamic fields

  output$nsets <- renderUI({
    max_sets <- getMaxSets()
    sliderInput(ns("nsets"), label = "Number of sets", min = 2, max = max_sets, step = 1, value = max_sets)
  })

  output$minorder <- renderUI({
    assignment_type <- getIntersectionAssignmentType()
    max_order <- getMaxIntersectionOrder()

    # No point starting at size 1 in a non-upset plot, since the areas associated with each individual set will be the same size as the sets themselves

    if (assignment_type == "upset") {
      startsize <- 1
    } else {
      startsize <- 2
    }

    sliderInput(ns("minorder"), label = "Minimum number of sets in an intersection", min = 1, max = max_order, step = 1, value = startsize)
  })

  output$differential_parameters <- renderUI({
    query_strings <- getQueryStrings()
    HTML(query_strings[1])
  })

  output$subset_notice <- renderUI({
    valid_sets <- getValidSets()
    max_sets <- ifelse(length(valid_sets) > setlimit, setlimit, length(valid_sets))

    message <- paste(length(valid_sets), "valid differential sets found (count > 0)")
    if (length(valid_sets) > setlimit) {
      message <- paste(message, "<br />(Note: the set selection used to produce the below plot has been limited to", max_sets, "sets")
      if (input$separate_by_direction) {
        message <- paste(message, paste("(", floor((max_sets / 2)), " contrasts)"))
      }
      message <- paste(message, "due to computational limitations. You may wish to refine your choice of contrasts)")
    }
    HTML(message)
  })

  ############################################################################# Form accessors


  # Accessor for the nsets parameter

  getNsets <- reactive({
    validate(need(!is.null(input$nsets), "Waiting for nsets"))
    input$nsets
  })

  # Accessor for the minorder parameter

  getMinOrder <- reactive({
    validate(need(!is.null(input$minorder), "Waiting for minorder"))
    input$minorder
  })

  # Accessor for the nintersections parameter

  getNintersections <- reactive({
    validate(need(!is.null(input$nintersects), "Waiting for nintersects"))
    input$nintersects
  })

  # Accessor for the groupby parameter

  getGroupby <- reactive({
    validate(need(!is.null(input$group_by), "Waiting for group_by"))
    input$group_by
  })

  getShowEmptyIntersections <- reactive({
    validate(need(!is.null(input$show_empty_intersections), "Waiting for empty intersections option"))
    input$show_empty_intersections
  })

  # Accessor for the intersection assignment type

  getIntersectionAssignmentType <- reactive({
    validate(need(!is.null(input$intersection_assignment_type), "Waiting for group_by"))
    input$intersection_assignment_type
  })

  # Set sorting

  getSetSort <- reactive({
    validate(need(!is.null(input$set_sort), "Waiting for set_sort"))
    input$set_sort
  })

  # Bar numbers

  getBarNumbers <- reactive({
    validate(need(!is.null(input$bar_numbers), "Waiting for bar numbers"))
    input$bar_numbers
  })

  ############################################################################# The business end- derive sets and pass for intersection

  # Look at the contrasts and remove any contrast with no differential features

  getValidSets <- reactive({
    withProgress(message = "Deriving input sets", value = 0, {
      fcts <- unlist(filteredContrastsTables(), recursive = FALSE)
      names(fcts) <- unlist(getSafeSelectedContrastNames(), recursive = FALSE)

      fcts <- fcts[unlist(lapply(fcts, function(x) nrow(x) > 0))]

      # If specified by the user, separate gene sets into 'up' and 'down' for each contrast

      if (input$separate_by_direction) {
        fcts <- unlist(lapply(fcts, function(x) {
          y <- split(x, x[["Fold change"]] > 0)
          names(y)[names(y) == "TRUE"] <- "up"
          names(y)[names(y) == "FALSE"] <- "down"
          y
        }), recursive = FALSE)

        fcts <- fcts[unlist(lapply(fcts, function(x) nrow(x) > 0))]
      }

      fcts <- fcts[unlist(lapply(fcts, function(x) nrow(x) > 0))]
      fcts <- lapply(fcts, rownames)

      setsort <- getSetSort()
      if (setsort) {
        fcts <- fcts[order(unlist(lapply(fcts, length)))]
      }

      fcts
    })
  })

  # Get the maximum number of sets

  getMaxSets <- reactive({
    valid_sets <- getValidSets()
    ifelse(length(valid_sets) > setlimit, setlimit, length(valid_sets))
  })

  # Get the sets we're going to use based on nsets

  getSets <- reactive({
    valid_sets <- getValidSets()
    nsets <- getNsets()
    valid_sets[1:nsets]
  })

  # Calculate intersections between sets

  calculateIntersections <- reactive({
    selected_sets <- getSets()

    withProgress(message = "Calculating set intersections", value = 0, {
      nsets <- getNsets()

      # Get all possible combinations of sets

      combinations <- function(items, pick) {
        x <- combn(items, pick)
        lapply(seq_len(ncol(x)), function(i) x[, i])
      }

      combos <- lapply(1:nsets, function(x) {
        combinations(1:length(selected_sets), x)
      })

      # Calculate the intersections of all these combinations

      withProgress(message = "Running intersect()", value = 0, {
        intersects <- lapply(combos, function(combonos) {
          lapply(combonos, function(combo) {
            Reduce(intersect, selected_sets[combo])
          })
        })
      })

      # For UpSet-ness, membership of higher-order intersections takes priority. Otherwise just return the number of entries in each intersection

      assignment_type <- getIntersectionAssignmentType()

      intersects <- lapply(1:length(intersects), function(i) {
        intersectno <- intersects[[i]]
        members_in_higher_levels <- unlist(intersects[(i + 1):length(intersects)])
        lapply(intersectno, function(intersect) {
          if (assignment_type == "upset") {
            length(setdiff(intersect, members_in_higher_levels))
          } else {
            length(intersect)
          }
        })
      })

      combos <- unlist(combos, recursive = FALSE)
      intersects <- unlist(intersects)

      if (!getShowEmptyIntersections()) {
        combos <- combos[which(intersects > 0)]
        intersects <- intersects[which(intersects > 0)]
      }

      # Sort by intersect size

      combos <- combos[order(intersects, decreasing = TRUE)]
      intersects <- intersects[order(intersects, decreasing = TRUE)]

      list(combinations = combos, intersections = intersects)
    })
  })

  # Post-process intersections to remove lower-order interactions from display if requested

  getIntersections <- reactive({
    minorder <- getMinOrder()
    ints <- calculateIntersections()

    selected <- unlist(lapply(ints$combinations, length)) >= minorder
    lapply(ints, function(x) x[selected])
  })

  ########################################################################### Render the plot with it separate components

  output$plotly_upset <- renderPlotly({
    grid <- upsetGrid()
    set_size_chart <- upsetSetSizeBarChart()
    intersect_size_chart <- upsetIntersectSizeBarChart()

    intersect_size_chart <- intersect_size_chart %>% layout(yaxis = list(title = "Intersections size"))

    s1 <- subplot(plotly_empty(type = "scatter", mode = "markers"), plotly_empty(type = "scatter", mode = "markers"), plotly_empty(type = "scatter", mode = "markers"),
      set_size_chart,
      nrows = 2, widths = c(0.6, 0.4)
    )
    s2 <- subplot(intersect_size_chart, grid, nrows = 2, shareX = TRUE) %>% layout(showlegend = FALSE)

    subplot(s1, s2, widths = c(0.3, 0.7))
  })

  # Calculate the maximum number of sets in an intersection

  getMaxIntersectionOrder <- reactive({
    ints <- calculateIntersections()
    max(unlist(lapply(ints$combinations[ints$intersections > 0], length)))
  })

  # Add some line returns to contrast names

  getSetNames <- reactive({
    selected_sets <- getSets()
    gsub("_", " ", names(selected_sets))
  })

  # Make the grid of points indicating set membership in intersections

  upsetGrid <- reactive({
    selected_sets <- getSets()
    ints <- getIntersections()

    intersects <- ints$intersections
    combos <- ints$combinations

    # Reduce the maximum number of intersections if we don't have that many

    nintersections <- getNintersections()
    nintersections <- min(nintersections, length(combos))

    # Fetch the number of sets

    nsets <- getNsets()
    setnames <- getSetNames()

    lines <- data.table::rbindlist(lapply(1:nintersections, function(combono) {
      data.frame(combo = combono, x = rep(combono, max(2, length(combos[[combono]]))), y = (nsets - combos[[combono]]) + 1, name = setnames[combos[[combono]]])
    }))

    plot_ly(type = "scatter", mode = "markers", marker = list(color = "lightgrey", size = 8)) %>%
      add_trace(type = "scatter", x = rep(
        1:nintersections,
        length(selected_sets)
      ), y = unlist(lapply(1:length(selected_sets), function(x) rep(x - 0.5, nintersections))), hoverinfo = "none") %>%
      add_trace(
        type = "scatter",
        data = group_by(lines, combo), mode = "lines+markers", x = lines$x, y = lines$y - 0.5, line = list(color = "black", width = 3), marker = list(
          color = "black",
          size = 10
        ), hoverinfo = "text", text = ~name
      ) %>%
      layout(xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE), yaxis = list(showticklabels = FALSE, showgrid = FALSE, range = c(0, nsets), zeroline = FALSE, range = 1:nsets), margin = list(t = 0, b = 40))
  })

  # Make the bar chart illustrating set sizes

  upsetSetSizeBarChart <- reactive({
    setnames <- getSetNames()
    selected_sets <- getSets()

    plot_ly(x = unlist(lapply(selected_sets, length)), y = setnames, type = "bar", orientation = "h", marker = list(color = "black")) %>% layout(bargap = 0.4, yaxis = list(categoryarray = rev(setnames), categoryorder = "array"))
  })

  # Make the bar chart illustrating intersect size

  upsetIntersectSizeBarChart <- reactive({
    ints <- getIntersections()
    intersects <- ints$intersections
    combos <- ints$combinations
    nintersections <- getNintersections()

    p <- plot_ly(showlegend = FALSE) %>% add_trace(x = 1:nintersections, y = unlist(intersects[1:nintersections]), type = "bar", marker = list(
      color = "black",
      hoverinfo = "none"
    ))

    bar_numbers <- getBarNumbers()

    if (bar_numbers) {
      p <- p %>% add_trace(
        type = "scatter", mode = "text", x = 1:nintersections, y = unlist(intersects[1:nintersections]) + (max(intersects) * 0.05),
        text = unlist(intersects[1:nintersections]), textfont = list(color = "black")
      )
    }

    p
  })

  # Provide the differential set summary for download

  callModule(simpletable, "upset",
    downloadMatrix = makeDifferentialSetSummary, displayMatrix = makeDifferentialSetSummary, filter = "none", filename = "differential_summary",
    rownames = FALSE
  )
}
