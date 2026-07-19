upset_modal <- list(id = "upset", title = "Intersection plots with UpSet")

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
    uiOutput(ns("nsets_ui")), sliderInput(ns("nintersects"), label = "Number of intersections", min = 2, max = 40, step = 1, value = 20), uiOutput(ns("minorder_ui")),
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

  moduleMain(
    "Intersection of differential sets",
    uiOutput(ns("subset_notice")),
    shinycssloaders::withSpinner(plotlyOutput(ns("plotly_upset"), height = "600px"), color = shinyngsSpinnerColor()),
    h4("Differential set summary"),
    uiOutput(ns("differential_parameters")),
    simpletableOutput(ns("upset")),
    help = modalInput(ns(upset_modal$id), "help", "help")
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
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
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
#' upstart("myid", eselist)
#'
upset <- function(id, eselist, setlimit = 16) {
  moduleServer(id, function(input, output, session) {
    modalServer(upset_modal$id, upset_modal$title)

    ns <- session$ns

    # Call the selectmatrix module and hold on to the reactives it sends back

    selectmatrix_reactives <- selectmatrix("upset", eselist, var_n = 1000, select_samples = FALSE, select_genes = TRUE, provide_all_genes = TRUE, select_meta = FALSE)

    # Pass the matrix to the contrasts module for processing

    contrast_reactives <- contrasts("upset", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = TRUE, select_all_contrasts = TRUE)

    ############################################################################# Render dynamic fields

    output$nsets_ui <- renderUI({
      max_sets <- getMaxSets()
      sliderInput(ns("nsets"), label = "Number of sets", min = 2, max = max_sets, step = 1, value = max_sets)
    })

    output$minorder_ui <- renderUI({
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
      query_strings <- contrast_reactives$getQueryStrings()
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
    }) %>% debounce(300)

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
        fcts <- unlist(contrast_reactives$filteredContrastsTables(), recursive = FALSE)
        names(fcts) <- unlist(contrast_reactives$getSafeSelectedContrastNames(), recursive = FALSE)

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
      valid_sets[seq_len(nsets)]
    })

    # Calculate intersections between sets. Only used to bound the minorder
    # slider (getMaxIntersectionOrder()) - the plot itself is built straight
    # from plotly_upset() below, which recomputes intersections internally.

    calculateIntersections <- reactive({
      withProgress(message = "Calculating set intersections", value = 0, {
        upset_calculate_intersections(getSets(), getShowEmptyIntersections(), getIntersectionAssignmentType())
      })
    }) %>% bindCache(getSets(), getShowEmptyIntersections(), getIntersectionAssignmentType())

    ########################################################################### Render the plot

    # Cached on exactly the inputs plotly_upset() reads, so toggling e.g. the
    # plot download format doesn't recompute the intersection enumeration.

    getUpsetPlot <- reactive({
      display_sets <- getSets()
      names(display_sets) <- getSetNames()

      plotly_upset(
        display_sets,
        nintersects = getNintersections(), minorder = getMinOrder(), set_sort = FALSE, bar_numbers = getBarNumbers(),
        show_empty_intersections = getShowEmptyIntersections(), intersection_assignment_type = getIntersectionAssignmentType()
      )
    }) %>% bindCache(
      getSets(), getShowEmptyIntersections(), getIntersectionAssignmentType(), getNintersections(), getMinOrder(), getBarNumbers()
    )

    output$plotly_upset <- renderPlotly({
      getUpsetPlot() %>% shinyngsPlotlyConfig("upset", format = session$userData$plotFormat())
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

    # Provide the differential set summary for download

    simpletable("upset", downloadMatrix = contrast_reactives$makeDifferentialSetSummary, displayMatrix = contrast_reactives$makeDifferentialSetSummary, filter = "none", filename = "differential_summary", rownames = FALSE)
  })
}

#' Compute set intersections and sizes for an UpSet-style plot
#'
#' Enumerates every combination of the supplied sets (at every order from
#' single sets up to all of them), and sizes each one. Under
#' \code{intersection_assignment_type = "upset"} a member is only counted at
#' its highest-order intersection (as in the original UpSet), so lower-order
#' combinations only report members not already claimed by a higher one;
#' under \code{"all"} a member is counted in every intersection it belongs to.
#'
#' @param sets A named list of character vectors, one per set (e.g. gene
#'   identifiers)
#' @param show_empty_intersections Include intersections/set combinations
#'   with zero members?
#' @param intersection_assignment_type 'upset' or 'all', see above
#'
#' @return output A list with \code{combinations} (a list of integer vectors,
#'   each giving the positions in \code{sets} making up one intersection) and
#'   \code{intersections} (the matching sizes), ordered by size, descending
#'
#' @keywords internal

upset_calculate_intersections <- function(sets, show_empty_intersections = TRUE, intersection_assignment_type = "upset") {
  nsets <- length(sets)

  combinations <- function(items, pick) {
    x <- combn(items, pick)
    lapply(seq_len(ncol(x)), function(i) x[, i])
  }

  combos <- lapply(seq_len(nsets), function(x) {
    combinations(seq_along(sets), x)
  })

  intersects <- lapply(combos, function(combonos) {
    lapply(combonos, function(combo) {
      Reduce(intersect, sets[combo])
    })
  })

  # For UpSet-ness, membership of higher-order intersections takes priority. Otherwise just return the number of entries in each intersection

  intersects <- lapply(seq_along(intersects), function(i) {
    intersectno <- intersects[[i]]
    members_in_higher_levels <- unlist(intersects[(i + 1):length(intersects)])
    lapply(intersectno, function(intersect) {
      if (intersection_assignment_type == "upset") {
        length(setdiff(intersect, members_in_higher_levels))
      } else {
        length(intersect)
      }
    })
  })

  combos <- unlist(combos, recursive = FALSE)
  intersects <- unlist(intersects)

  if (!show_empty_intersections) {
    combos <- combos[which(intersects > 0)]
    intersects <- intersects[which(intersects > 0)]
  }

  # Sort by intersect size

  combos <- combos[order(intersects, decreasing = TRUE)]
  intersects <- intersects[order(intersects, decreasing = TRUE)]

  list(combinations = combos, intersections = intersects)
}

#' Drop intersections involving fewer than a minimum number of sets
#'
#' @param ints A list with \code{combinations} and \code{intersections}, as
#'   returned by \code{\link{upset_calculate_intersections}}
#' @param minorder Minimum number of sets that must be involved in an
#'   intersection for it to be kept
#'
#' @return output \code{ints}, with both elements filtered to the same subset
#'
#' @keywords internal

upset_filter_intersections_by_order <- function(ints, minorder) {
  selected <- unlist(lapply(ints$combinations, length)) >= minorder
  lapply(ints, function(x) x[selected])
}

#' Make the grid of points indicating set membership in intersections
#'
#' @param sets A named list of character vectors, one per set
#' @param ints A list with \code{combinations} and \code{intersections}, as
#'   returned by \code{\link{upset_calculate_intersections}} (optionally
#'   filtered by \code{\link{upset_filter_intersections_by_order}})
#' @param nintersects Maximum number of intersections to display, taken from
#'   the front of \code{ints}
#'
#' @return output A plotly htmlwidget
#'
#' @keywords internal

upset_grid_plot <- function(sets, ints, nintersects) {
  intersects <- ints$intersections
  combos <- ints$combinations

  # Reduce the maximum number of intersections if we don't have that many

  nintersections <- min(nintersects, length(combos))
  nsets <- length(sets)
  setnames <- names(sets)

  lines <- dplyr::bind_rows(lapply(seq_len(nintersections), function(combono) {
    data.frame(combo = combono, x = rep(combono, max(2, length(combos[[combono]]))), y = (nsets - combos[[combono]]) + 1, name = setnames[combos[[combono]]])
  }))

  plot_ly(type = "scatter", mode = "markers", marker = list(color = "lightgrey", size = 8)) %>%
    add_trace(type = "scatter", x = rep(
      seq_len(nintersections),
      length(sets)
    ), y = unlist(lapply(seq_along(sets), function(x) rep(x - 0.5, nintersections))), hoverinfo = "none") %>%
    add_trace(
      type = "scatter",
      data = dplyr::group_by(lines, combo), mode = "lines+markers", x = lines$x, y = lines$y - 0.5, line = list(color = "black", width = 3), marker = list(
        color = "black",
        size = 10
      ), hoverinfo = "text", text = ~name
    ) %>%
    layout(xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE), yaxis = list(showticklabels = FALSE, showgrid = FALSE, range = c(0, nsets), zeroline = FALSE), margin = list(t = 0, b = 40))
}

#' Make the bar chart illustrating set sizes
#'
#' @param sets A named list of character vectors, one per set
#'
#' @return output A plotly htmlwidget
#'
#' @keywords internal

upset_set_size_chart <- function(sets) {
  setnames <- names(sets)

  plot_ly(x = unlist(lapply(sets, length)), y = setnames, type = "bar", orientation = "h", marker = list(color = "black")) %>%
    layout(bargap = 0.4, yaxis = list(categoryarray = rev(setnames), categoryorder = "array"))
}

#' Make the bar chart illustrating intersect size
#'
#' @param ints A list with \code{combinations} and \code{intersections}, as
#'   returned by \code{\link{upset_calculate_intersections}} (optionally
#'   filtered by \code{\link{upset_filter_intersections_by_order}})
#' @param nintersects Maximum number of intersections to display
#' @param bar_numbers Add value labels above the bars?
#'
#' @return output A plotly htmlwidget
#'
#' @keywords internal

upset_intersect_size_chart <- function(ints, nintersects, bar_numbers = FALSE) {
  intersects <- ints$intersections

  p <- plot_ly(showlegend = FALSE) %>% add_trace(x = seq_len(nintersects), y = unlist(intersects[seq_len(nintersects)]), type = "bar", marker = list(
    color = "black",
    hoverinfo = "none"
  ))

  if (bar_numbers) {
    p <- p %>% add_trace(
      type = "scatter", mode = "text", x = seq_len(nintersects), y = unlist(intersects[seq_len(nintersects)]) + (max(intersects) * 0.05),
      text = unlist(intersects[seq_len(nintersects)]), textfont = list(color = "black")
    )
  }

  p
}

#' Make an UpSet-style set intersection plot with \code{plot_ly()}
#'
#' A reimplementation of the \code{\link[UpSetR]{upset}} tool of Lex,
#' Gehlenborg et al: a bar chart of set sizes, a bar chart of intersection
#' sizes, and a grid showing which sets make up each intersection - drawn
#' with \code{plotly} rather than base graphics so it stays interactive
#' outside of a fixed set count, and so all elements of a given intersection
#' can be plotted (rather than assigning every item to its highest-order
#' intersection).
#'
#' @param sets A named list of character vectors, one per set (e.g. up/down
#'   differential gene sets per contrast)
#' @param nintersects Maximum number of intersections to display, ordered by
#'   size
#' @param minorder Minimum number of sets that must be involved in an
#'   intersection for it to be shown
#' @param set_sort Sort sets by size (ascending) before plotting?
#' @param bar_numbers Add value labels above the intersection size bars?
#' @param show_empty_intersections Include intersections/set combinations
#'   with zero members?
#' @param intersection_assignment_type 'upset' assigns each member to its
#'   highest-order intersection only (as in the original UpSet); 'all' counts
#'   a member in every intersection it belongs to
#'
#' @return output A plotly htmlwidget
#'
#' @references
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#'
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'
#' @export
#'
#' @examples
#' sets <- list(a = paste0("gene", 1:6), b = paste0("gene", 4:10), c = paste0("gene", 8:12))
#' plotly_upset(sets)
#'
plotly_upset <- function(sets, nintersects = 20, minorder = 1, set_sort = TRUE, bar_numbers = FALSE, show_empty_intersections = TRUE,
                          intersection_assignment_type = c("upset", "all")) {
  intersection_assignment_type <- match.arg(intersection_assignment_type)

  if (set_sort) {
    sets <- sets[order(lengths(sets))]
  }

  ints <- upset_filter_intersections_by_order(
    upset_calculate_intersections(sets, show_empty_intersections, intersection_assignment_type),
    minorder
  )

  set_size_chart <- upset_set_size_chart(sets)
  intersect_size_chart <- upset_intersect_size_chart(ints, nintersects, bar_numbers) %>%
    layout(yaxis = list(title = "Intersections size"))
  grid <- upset_grid_plot(sets, ints, nintersects)

  s1 <- subplot(
    plotly_empty(type = "scatter", mode = "markers"), plotly_empty(type = "scatter", mode = "markers"), plotly_empty(type = "scatter", mode = "markers"),
    set_size_chart,
    nrows = 2, widths = c(0.6, 0.4)
  )
  s2 <- subplot(intersect_size_chart, grid, nrows = 2, shareX = TRUE) %>% layout(showlegend = FALSE)

  subplot(s1, s2, widths = c(0.3, 0.7))
}
