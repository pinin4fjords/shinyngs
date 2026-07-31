enrichmentoverview_modal <- list(id = "enrichmentoverview", title = "Gene set overview")

enrichment_analysis_eselist <- function(eselist) {
  eselist <- eselist[vapply(eselist, function(ese) {
    has_slot_data(ese, "gene_set_analyses") &&
      length(intersect(names(assays(ese)), names(ese@gene_set_analyses))) > 0
  }, logical(1))]
  for (experiment in names(eselist)) {
    valid_assays <- intersect(names(assays(eselist[[experiment]])), names(eselist[[experiment]]@gene_set_analyses))
    assays(eselist[[experiment]]) <- assays(eselist[[experiment]])[valid_assays]
  }
  eselist
}

has_cross_contrast_enrichment <- function(eselist) {
  if (!has_slot_data(eselist, "contrasts") || length(eselist@contrasts) < 2) {
    return(FALSE)
  }

  any(vapply(eselist, function(ese) {
    if (!has_slot_data(ese, "gene_set_analyses")) {
      return(FALSE)
    }
    valid_assays <- intersect(names(assays(ese)), names(ese@gene_set_analyses))
    any(vapply(valid_assays, function(assay) {
      any(vapply(names(ese@gene_set_analyses[[assay]]), function(gene_set_type) {
        resolved <- lapply(seq_along(eselist@contrasts), function(contrast_number) {
          resolve_enrichment(ese, assay, gene_set_type, contrast_number, eselist@contrasts[[contrast_number]])
        })
        resolved <- Filter(Negate(is.null), resolved)
        methods <- unique(vapply(resolved, function(result) enrichment_tool_label(result$tool), character(1)))
        length(resolved) >= 2 && length(methods) == 1
      }, logical(1)))
    }, logical(1)))
  }, logical(1)))
}

#' Input function for the gene set overview module
#'
#' @param id Module namespace.
#' @param eselist ExploratorySummarizedExperimentList object.
#'
#' @return Controls for the experiment, assay, gene set type, ranking, and export.
#' @keywords shiny
enrichmentoverviewInput <- function(id, eselist) {
  ns <- NS(id)
  eselist <- enrichment_analysis_eselist(eselist)
  expression_filters <- selectmatrixInput(ns("expression"), eselist)

  field_sets <- list(
    overview = list(
      uiOutput(ns("geneSetType_ui")),
      sliderInput(ns("top_n"), "Number of gene sets", min = 5, max = 50, value = 20, step = 1),
      numericInput(ns("max_fdr"), "Maximum FDR", value = 0.1, min = 0, max = 1, step = 0.01)
    )
  )
  naked_fields <- list()
  if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
    field_sets$data <- expression_filters
  } else {
    naked_fields <- expression_filters
  }
  field_sets <- c(field_sets, list(export = simpletableInput(ns("table"), tabletitle = "Gene set overview")))

  list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' Output function for the gene set overview module
#'
#' @param id Module namespace.
#'
#' @return Outputs for the method label, plot, and backing table.
#' @keywords shiny
enrichmentoverviewOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    "Gene set enrichment across contrasts",
    uiOutput(ns("enrichmentMethod")),
    uiOutput(ns("plot_ui")),
    h4("Overview data"),
    simpletableOutput(ns("table")),
    help = modalInput(ns(enrichmentoverview_modal$id), "help", "help")
  )
}

#' Server function for the gene set overview module
#'
#' @param id Module namespace.
#' @param eselist ExploratorySummarizedExperimentList object.
#'
#' @keywords shiny
enrichmentoverview <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(enrichmentoverview_modal$id, enrichmentoverview_modal$title)
    eselist <- enrichment_analysis_eselist(eselist)

    selectmatrix_reactives <- selectmatrix(
      "expression", eselist,
      var_n = 50, select_assays = TRUE, select_samples = FALSE,
      select_genes = FALSE, select_meta = FALSE
    )

    output$geneSetType_ui <- renderUI({
      ese <- selectmatrix_reactives$getExperiment()
      assay <- selectmatrix_reactives$getAssay()
      gene_set_types <- names(ese@gene_set_analyses[[assay]])
      selectInput(session$ns("gene_set_type"), "Gene set type", gene_set_types)
    })

    getEnrichmentOverviewData <- reactive({
      validate(need(input$gene_set_type, "Waiting for gene set type"))
      compile_enrichment_overview(
        selectmatrix_reactives$getExperiment(), selectmatrix_reactives$getAssay(),
        input$gene_set_type, eselist@contrasts
      )
    })

    getPreparedEnrichmentOverview <- reactive({
      validate(
        need(input$top_n, "Waiting for number of gene sets"),
        need(!is.null(input$max_fdr) && input$max_fdr >= 0 && input$max_fdr <= 1, "Maximum FDR must be between 0 and 1")
      )
      data <- getEnrichmentOverviewData()
      methods <- unique(stats::na.omit(data$method))
      validate(need(length(methods) <= 1, "Cross-contrast overview requires one enrichment method for the selected gene set type"))
      resolved_contrasts <- unique(data$contrast[is.finite(data$fdr)])
      validate(need(length(resolved_contrasts) >= 2, "At least two contrasts with enrichment results are required"))

      prepare_enrichment_overview(data, top_n = input$top_n, max_fdr = input$max_fdr)
    })

    output$enrichmentMethod <- renderUI({
      methods <- unique(stats::na.omit(getEnrichmentOverviewData()$method))
      if (length(methods) == 1) helpText(paste0("Method: ", methods)) else NULL
    })

    output$plot_ui <- renderUI({
      height <- min(1100, max(460, length(unique(getPreparedEnrichmentOverview()$gene_set_id)) * 34 + 190))
      shinycssloaders::withSpinner(
        plotlyOutput(session$ns("plot"), height = paste0(height, "px")),
        color = shinyngsSpinnerColor()
      )
    })

    getEnrichmentOverviewPlot <- reactive({
      interactive_enrichment_overview(getPreparedEnrichmentOverview(), prepared = TRUE)
    }) %>% bindCache(getPreparedEnrichmentOverview())

    output$plot <- renderPlotly({
      getEnrichmentOverviewPlot() %>%
        shinyngsPlotlyConfig("gene_set_overview", format = session$userData$plotFormat())
    })

    getEnrichmentOverviewTable <- reactive({
      overview <- getPreparedEnrichmentOverview()
      data.frame(
        `Gene set` = overview$gene_set_id,
        Contrast = overview$contrast,
        `p value` = overview$pvalue,
        FDR = overview$fdr,
        Direction = overview$direction,
        Method = overview$method,
        check.names = FALSE
      )
    })

    simpletable(
      "table", downloadMatrix = getEnrichmentOverviewTable,
      displayMatrix = getEnrichmentOverviewTable,
      filename = "gene_set_overview", rownames = FALSE, server = FALSE
    )
  })
}

compile_enrichment_overview <- function(ese, assay, gene_set_type, contrasts) {
  contrast_labels <- unlist(makeContrastNamesFor(contrasts), use.names = FALSE)
  rows <- lapply(seq_along(contrasts), function(contrast_number) {
    enrichment <- resolve_enrichment(ese, assay, gene_set_type, contrast_number, contrasts[[contrast_number]])
    if (is.null(enrichment)) {
      return(NULL)
    }
    data.frame(
      gene_set_id = rownames(enrichment$gst),
      contrast = contrast_labels[contrast_number],
      pvalue = suppressWarnings(as.numeric(enrichment$gst[[enrichment$col_map$pvalue]])),
      fdr = suppressWarnings(as.numeric(enrichment$gst[[enrichment$col_map$fdr]])),
      direction = as.character(enrichment$gst[[enrichment$col_map$direction]]),
      method = enrichment_tool_label(enrichment$tool),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    result <- data.frame(
      gene_set_id = character(), contrast = character(), pvalue = numeric(),
      fdr = numeric(), direction = character(), method = character()
    )
  } else {
    result <- do.call(rbind, rows)
    rownames(result) <- NULL
  }
  attr(result, "contrast_levels") <- contrast_labels
  result
}

prepare_enrichment_overview <- function(data, top_n = 20, max_fdr = 0.1) {
  required_columns <- c("gene_set_id", "contrast", "pvalue", "fdr", "direction", "method")
  if (!all(required_columns %in% colnames(data))) {
    stop("prepare_enrichment_overview(): data is missing required columns")
  }
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("prepare_enrichment_overview(): 'top_n' must be a positive number")
  }
  if (!is.numeric(max_fdr) || length(max_fdr) != 1 || is.na(max_fdr) || max_fdr < 0 || max_fdr > 1) {
    stop("prepare_enrichment_overview(): 'max_fdr' must be one number between 0 and 1")
  }

  methods <- unique(stats::na.omit(data$method))
  if (length(methods) > 1) {
    stop("prepare_enrichment_overview(): results use more than one enrichment method")
  }
  eligible <- is.finite(data$fdr) & data$fdr <= max_fdr
  if (!any(eligible)) {
    stop("prepare_enrichment_overview(): no gene sets meet the maximum FDR")
  }

  minimum_fdr <- tapply(data$fdr[eligible], data$gene_set_id[eligible], min)
  ranked_ids <- names(sort(minimum_fdr, method = "radix"))
  selected_ids <- head(ranked_ids, as.integer(top_n))
  contrast_levels <- attr(data, "contrast_levels")
  if (is.null(contrast_levels)) {
    contrast_levels <- unique(data$contrast)
  }

  grid <- expand.grid(
    gene_set_id = selected_ids, contrast = contrast_levels,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid$.order <- seq_len(nrow(grid))
  overview <- merge(grid, data, by = c("gene_set_id", "contrast"), all.x = TRUE, sort = FALSE)
  overview <- overview[order(overview$.order), c(required_columns), drop = FALSE]
  attr(overview, "contrast_levels") <- contrast_levels
  attr(overview, "gene_set_levels") <- selected_ids
  overview
}

#' Plot gene set enrichment across contrasts
#'
#' Gene sets are ranked by their smallest FDR across contrasts. Marker size
#' represents \code{-log10(FDR)} (capped at 16), marker colour represents the
#' reported direction, and absent results remain blank.
#'
#' @param data Long-format enrichment data with \code{gene_set_id},
#'   \code{contrast}, \code{pvalue}, \code{fdr}, \code{direction}, and
#'   \code{method} columns.
#' @param top_n Maximum number of gene sets to display.
#' @param max_fdr Gene sets must meet this FDR in at least one contrast.
#' @param prepared Whether \code{data} has already been processed by
#'   \code{prepare_enrichment_overview}.
#' @param title Plot title.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' enrichment <- data.frame(
#'   gene_set_id = rep(c("SET_A", "SET_B"), each = 2),
#'   contrast = rep(c("treated A vs control", "treated B vs control"), 2),
#'   pvalue = c(0.001, 0.02, 0.03, 0.4),
#'   fdr = c(0.01, 0.04, 0.08, 0.5),
#'   direction = c("Up", "Up", "Down", "Down"),
#'   method = "ROAST"
#' )
#' interactive_enrichment_overview(enrichment)
interactive_enrichment_overview <- function(data, top_n = 20, max_fdr = 0.1, prepared = FALSE, title = "Gene set enrichment across contrasts") {
  if (!prepared) {
    data <- prepare_enrichment_overview(data, top_n = top_n, max_fdr = max_fdr)
  }
  plot_data <- data[is.finite(data$fdr) & !is.na(data$direction), , drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop("interactive_enrichment_overview(): no finite enrichment results to plot")
  }

  direction_levels <- unique(plot_data$direction)
  direction_colors <- stats::setNames(make_color_scale(length(direction_levels)), direction_levels)
  if ("Up" %in% direction_levels) direction_colors[["Up"]] <- "#D55E00"
  if ("Down" %in% direction_levels) direction_colors[["Down"]] <- "#0072B2"
  plot_data$significance <- pmin(-log10(pmax(plot_data$fdr, 1e-16)), 16)
  significance_range <- range(plot_data$significance)
  plot_data$marker_size <- if (diff(significance_range) == 0) {
    rep(18, nrow(plot_data))
  } else {
    8 + 20 * (plot_data$significance - significance_range[1]) / diff(significance_range)
  }

  hover <- paste0(
    "<b>", htmltools::htmlEscape(plot_data$gene_set_id), "</b>",
    "<br>", htmltools::htmlEscape(plot_data$contrast),
    "<br>Direction: ", htmltools::htmlEscape(as.character(plot_data$direction)),
    "<br>p value: ", signif(plot_data$pvalue, 4),
    "<br>FDR: ", signif(plot_data$fdr, 4),
    "<br>Method: ", htmltools::htmlEscape(plot_data$method)
  )

  contrast_levels <- attr(data, "contrast_levels")
  gene_set_levels <- attr(data, "gene_set_levels")
  if (is.null(contrast_levels)) contrast_levels <- unique(data$contrast)
  if (is.null(gene_set_levels)) gene_set_levels <- unique(data$gene_set_id)

  p <- plot_ly()
  for (direction in direction_levels) {
    rows <- plot_data$direction == direction
    p <- add_markers(
      p,
      x = plot_data$contrast[rows], y = plot_data$gene_set_id[rows],
      name = direction, text = hover[rows],
      hovertemplate = "%{text}<extra></extra>",
      marker = list(color = direction_colors[[direction]], size = plot_data$marker_size[rows], opacity = 0.85)
    )
  }

  layout(
      p,
      title = htmltools::htmlEscape(title),
      xaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = contrast_levels),
      yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = rev(gene_set_levels)),
      legend = list(orientation = "h", x = 0, y = -0.22, itemsizing = "constant"),
      margin = list(l = 40, r = 20, b = 120, t = 70)
    )
}
