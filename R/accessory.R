#' Resolve the default grouping variable for an experiment list
#'
#' Returns \code{default_groupvar} if set, otherwise the first of
#' \code{group_vars}, or \code{NULL} when no grouping variables are defined.
#'
#' @param eselist ExploratorySummarizedExperimentList object
#'
#' @return A single grouping-variable name, or \code{NULL}
#'
#' @keywords internal
#'
defaultGroupvar <- function(eselist) {
  if (length(eselist@default_groupvar) > 0) {
    eselist@default_groupvar
  } else if (length(eselist@group_vars) > 0) {
    eselist@group_vars[1]
  } else {
    NULL
  }
}

#' Convenience interface to strsplit()
#'
#' @param string Input string
#' @param sep Separator
#'
#' @return output Character vector of strings
#' @export

simpleSplit <- function(string, sep = ",") {
  if (is.na(string)) {
    NA
  } else {
    unlist(strsplit(string, sep))
  }
}

#' Extract the extension of a file
#'
#' @param file Input file path
#'
#' @return extension Extension like 'csv' or 'tsv'
#' @export

getExtension <- function(file) {
  ex <- strsplit(basename(file), split = "\\.")[[1]]
  return(tolower(tail(ex, 1)))
}

#' Infer a separator from the extension of an input file
#'
#' @param file Input file path
#'
#' @return output Separator character like tab or ','
#' @export

getSeparator <- function(file) {
  ext <- getExtension(file)
  if (ext == "tsv" || ext == "txt") {
    separator <- "\t"
  } else if (ext == "csv") {
    separator <- ","
  } else {
    stop(paste("Unknown separator for", ext))
  }
  separator
}

#' Take two delimiter-separated strings and generate a named vector
#'
#' @param elements_string String to be converted to vector elements
#' @param names_string String to be converted to vector names by default taken
#'   from elements_string.
#' @param sep Separator to use
#' @param prettify_names Boolean. Prettify element names?
#' @param simplify_files If elements are file and to be used to generate names,
#'   should we simplify by striping path and extension?
#'
#' @return output Named character vector
#' @export

stringsToNamedVector <- function(elements_string, names_string = NULL, sep = ",", prettify_names = TRUE, simplify_files = FALSE) {
  elements <- simpleSplit(elements_string, sep = sep)

  if (is.null(names_string)) {
    element_names <- elements
    if (simplify_files) {
      element_names <- tools::file_path_sans_ext(gsub("_", " ", basename(element_names)))
    }
  } else {
    element_names <- simpleSplit(names_string, sep = sep)
  }

  if (length(elements) != length(element_names)) {
    stop(paste("List in", elements_string, "a different length to", names_string))
  }
  if (prettify_names) {
    element_names <- prettifyVariablename(element_names)
  }

  names(elements) <- element_names

  elements
}


#' Make machine variable names pretty for display
#'
#' Just split out words using '_' and capitalise first letter
#'
#' @param vn A string to be prettified
#' @param tolower Convert to lower case first? (Default: FALSE)
#'
#' @return output Prettified string
#'
#' @export
#'
#' @examples
#' vn <- "ugly_name_of_thing"
#' prettyifyVariablename(vn)
#'
prettifyVariablename <- function(vn, tolower = FALSE) {
  if (tolower) {
    vn <- tolower(vn)
  }
  gsub("_", " ", ucfirst(vn))
}

#' Capitalise the first letter of a string
#'
#' @param string A string
#'
#' @return output String with capitalised first letter
#'
#' @export
#'
#' @examples
#' ucfirst("Example")
#'
ucfirst <- function(string) {
  paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
}

#' Count the number of lines in a string
#'
#' @param string A string
#'
#' @return output Integer supplying the number of lines in a string
#'
#' @export
#'
#' @examples
#' nlines("foo\nbar")
nlines <- function(string) {
  length(simpleSplit(string, "\n"))
}

#' Make a hidden input field. Handy for replacing superfluous single-value
#' selects etc
#'
#' @param id An HTML id
#' @param values The value the input should return
#'
#' @return output HTML as output by Shiny's \code{HTML()}
#'
#' @export
#'
#' @examples
#' hiddenInput("myid", "iamavalue")
#'
hiddenInput <- function(id, values) {
  HTML(paste0(unlist(lapply(values, function(value) paste0("<input type='text' id='", id, "' value='", value, "' style='display: none;'>")))))

  # HTML(paste0('<input type='text' id='', id, '' value='', value, '' style='display: none;'>'))
}

#' Simple list push
#'
#' @param input_list A list to push to
#' @param element The element to push
#'
#' @return list with element pushed
#'
#' @export
#'
#' @examples
#' mylist <- pushToList(mylist, "new element")
#'
pushToList <- function(input_list, element) {
  input_list[[length(input_list) + 1]] <- element
  input_list
}

#' Create sets of fields for display
#'
#' Shiny apps can get cluttered with many inputs. This method wraps sets of
#' fields in collapsible Bootstrap panels, one per named element, using
#' Bootstrap's own \code{data-toggle="collapse"} markup rather than a
#' separate collapsible-panel package. Every panel collapses/expands
#' independently (there is no "close others on open" behaviour).
#'
#' @param id ID field to apply to the overall container
#' @param fieldset_list A named list, each element containing one or more
#' fields.
#' @param open Controls which panels are open by default, as a character vector
#' of panel names. In most cases all should be left open (the default), since
#' fields in collapsed panels may be less discoverable.
#'
#' @return A \code{tagList} of Bootstrap panels

fieldSets <- function(id, fieldset_list, open = NULL) {
  if (is.null(open)) {
    open <- names(fieldset_list)
  }

  ns <- NS(id)

  panels <- lapply(names(fieldset_list), function(listname) {
    panel_id <- ns(make.names(listname))
    heading_id <- paste0(panel_id, "-heading")
    collapse_id <- paste0(panel_id, "-collapse")
    is_open <- listname %in% open

    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        role = "tab",
        id = heading_id,
        tags$h4(
          class = "panel-title",
          tags$a(
            role = "button",
            `data-toggle` = "collapse",
            href = paste0("#", collapse_id),
            `aria-expanded` = tolower(as.character(is_open)),
            `aria-controls` = collapse_id,
            prettifyVariablename(listname)
          )
        )
      ),
      tags$div(
        id = collapse_id,
        class = paste("panel-collapse collapse", if (is_open) "in" else ""),
        role = "tabpanel",
        `aria-labelledby` = heading_id,
        tags$div(class = "panel-body", fieldset_list[[listname]])
      )
    )
  })

  tags$div(id = id, class = "panel-group", role = "tablist", panels)
}

#' Reshape a matrix into long format
#'
#' Produces a three-column long format for a matrix: a row identifier, a
#' column identifier and the value, with rows ordered so the column
#' identifier varies slowest. Row and column identifiers are factors levelled
#' in the matrix's original row/column order when dimnames are present, or
#' plain integer indices otherwise.
#'
#' @param m A matrix
#' @param varnames Column names for the row and column identifiers
#' @param value.name Column name for the value
#'
#' @return A data frame with columns \code{varnames[1]}, \code{varnames[2]}
#'   and \code{value.name}
#'
#' @noRd
melt_matrix <- function(m, varnames = c("Var1", "Var2"), value.name = "value") {
  dn <- dimnames(m)
  if (is.null(dn)) dn <- list(NULL, NULL)
  dimnames(m) <- list(
    if (is.null(dn[[1]])) seq_len(nrow(m)) else dn[[1]],
    if (is.null(dn[[2]])) seq_len(ncol(m)) else dn[[2]]
  )

  out <- as.data.frame(as.table(m), stringsAsFactors = TRUE)
  colnames(out) <- c(varnames, value.name)

  if (is.null(dn[[1]])) out[[varnames[1]]] <- as.integer(as.character(out[[varnames[1]]]))
  if (is.null(dn[[2]])) out[[varnames[2]]] <- as.integer(as.character(out[[varnames[2]]]))
  out
}

#' Reshape data to the way \code{ggplot2} likes it
#'
#' @param plotmatrices A matrix of values, e.g. expression data
#' @param experiment A data frame with rows matching the columns of
#' \code{matrix}
#' @param colorby An optional string specifying a column from \code{experiment}
#' that will be used to set a color column in the reshaped output.
#' @param value_type Type of data to assemble. By default this is just expression
#'   values, but can be 'density' to calculate expression densities.
#' @param annotate_samples Add a suffix to sample labels reflecting their group?
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL (default), a conditional transformation based on threshold is applied.
#' @return A reshaped data frame
#'
#' @export
#'
#' @examples
#' plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)
#'
ggplotify <- function(plotmatrices, experiment, colorby = NULL, value_type = "expression", annotate_samples = FALSE, should_transform = NULL) {
  # If color grouping is specified, sort by the coloring variable so the groups will be plotted together

  if (!is.null(colorby)) {
    colnames(experiment)[colnames(experiment) == colorby] <- prettifyVariablename(colorby)
    colorby <- prettifyVariablename(colorby)

    experiment[[colorby]] <- na.replace(experiment[[colorby]], "N/A")

    # Group samples by the coloring variable while maintaining ordering as much as possible

    experiment <- experiment[order(factor(experiment[[colorby]], levels = unique(experiment[[colorby]]))), , drop = FALSE]
  }

  # Allow for a list of matrices, likely for faceting

  if (!is.list(plotmatrices)) {
    plotmatrices <- list(" " = plotmatrices)
  }

  allplotdata <- do.call(rbind, lapply(names(plotmatrices), function(pm) {
    if (value_type == "density") {
      plotdata <- do.call(rbind, lapply(colnames(plotmatrices[[pm]]), function(s) {
        dens <- density(cond_log2_transform_matrix(plotmatrices[[pm]][, s], rmzeros = TRUE, should_transform = should_transform), n = 100)
        data.frame(name = s, value = dens$x, density = dens$y)
      }))
    } else {
      plotdata <- melt_matrix(as.matrix(plotmatrices[[pm]][, rownames(experiment), drop = FALSE]))
      plotdata <- plotdata[which(plotdata$value > 0), ]
      colnames(plotdata) <- c("gene", "name", "value")
      plotdata$value <- cond_log2_transform_matrix(plotdata$value, should_transform = should_transform)
    }

    if (!is.null(colorby)) {
      plotdata$colorby <- factor(experiment[[colorby]][match(plotdata$name, rownames(experiment))], levels = unique(experiment[[colorby]]))
      if (annotate_samples) {
        plotdata$name <- paste0(plotdata$name, " (", plotdata$colorby, ")")
      }
    }
    plotdata$type <- prettifyVariablename(pm)
    plotdata
  }))

  # Make sure that if we received multiple matrices, they're plotted in the right order

  allplotdata$type <- factor(allplotdata$type, levels = unique(allplotdata$type))

  # Make sure name is a factor to 1) stop ggplot re-ordering the axis and 2) stop it interpreting it as numeric

  allplotdata$name <- factor(allplotdata$name, levels = unique(allplotdata$name))
  allplotdata
}

#' Given a string with spaces, try to split into multiple lines of <
#' \code{linewidth} characters
#'
#' @param string A string with spaces
#' @param linewidth The maximum line length in characters (default: 30)
#'
#' @return A string with newline characters added where appropriate
#'
#' @export
#'
#' @examples
#' splitStringToFixedwidthLines("once upon a time there was a giant and a beanstalk and a pot of gold and some beans")
#'
splitStringToFixedwidthLines <- function(string, linewidth = 20) {
  words <- simpleSplit(string, " ")

  strings <- list()
  string <- words[1]

  for (word in words[-1]) {
    if (nchar(string) >= linewidth) {
      strings[[length(strings) + 1]] <- string
      string <- word
    } else {
      string <- paste(string, word)
    }
  }

  strings[[length(strings) + 1]] <- string
  paste(unlist(strings), collapse = "\n")
}

#' Unpack a list to the environment. Handy when many reactive functions are
#' returned by a call to a module's server function
#'
#' @param object A named list of objects to unpack
#'
#' @export

unpack.list <- function(object) {
  for (.x in names(object)) {
    assign(value = object[[.x]], x = .x, envir = parent.frame())
  }
}

#' Interleave the columns of two matrices of equal dimensions
#'
#' @param mat1 First numeric matrix
#' @param mat2 Second numeric matrix
#'
#' @return output Interleaved matrix
#' @export

interleaveColumns <- function(mat1, mat2) {
  out <- cbind(mat1, mat2)
  out[, unlist(lapply(seq_len(ncol(mat1)), function(n) c(n, n + ncol(mat1))))]
}

#' Return a usable citation string for a package
#'
#' Adds just the tiny bit of wrapping code to make the output of
#' \code{citation()} printable.
#'
#' @param package Character string of package name
#'
#' @return citation A string that can be put in markdown doucments etc.

makePackageCitation <- function(package) {
  paste(capture.output(print(citation(package), style = "HTML")), collapse = " ")
}

#' Choose a valid set of grouping variables from a targets/ experiment data
#' frame.
#'
#' To be useful for grouping a variable needs to be a character or a factor,
#' and to have a number of values greater than one but less than the number of
#' samples.
#'
#' @param df Input data frame
#'
#' @return out A list of valid column names
#' @export
#'
#' @examples
#' # `airway` contains info on the samples it's based on
#'
#' require(airway)
#' data(airway, package = "airway")
#'
#' # However, not all variables are useful for grouping data. Some have a
#' # different value for every sample, one has the same value for all of them.
#'
#' colData(airway)
#'
#' # So just pick the variables that ARE useful
#'
#' chooseGroupingVariables(data.frame(colData(airway)))
#'
chooseGroupingVariables <- function(df) {
  all_vars <- colnames(df)
  vartypes <- unlist(lapply(df, class))
  numunique <- unlist(lapply(df, function(x) length(unique(x[!is.na(x)]))))

  all_vars[vartypes != "integer" & numunique < nrow(df) & numunique > 1]
}

#' Replace NAs with a string for convenience
#'
#' @param vec Character vector or factor containing NAs
#' @param replacement Character replacement (default: 'NA')
#'
#' @return Vector or factor with NAs replaced
#'
#' @export

na.replace <- function(vec, replacement = "NA") {
  isfactor <- is.factor(vec)
  if (isfactor) {
    vec <- as.character(vec)
  }

  vec[is.na(vec)] <- replacement

  # Turn it back into a vector if that's what we were passed

  if (isfactor) {
    vec <- factor(vec)
  }

  vec
}

#' Wrap a Shiny input so its label is displayed inline
#'
#' @param field_def A field definition with NULL set for the label property
#' @param label Field label
#' @param labelwidth With (in units out of 12) for label
#'
#' @return output A UI definition that can be passed to the shinyUI function.
#' @export
#'
#' @examples
#' inlineField(numericInput("foo", label = NULL, min = 0, max = 100, value = 50), "FOO")
#'
inlineField <- function(field_def, label, labelwidth = 6) {
  fluidRow(column(labelwidth, HTML(paste0("<b>", label, ":</b>&nbsp;"))), column(12 - labelwidth, field_def))
}



#' Make a numeric field with selectable associated cardinality (>, < ..).
#'
#' A wrapper around \code{\link[shiny]{numericInput}}, providing an inline
#' label and an associated field specifying how the value should be applied,
#' i.e. 'greater than this value' etc.
#'
#' @param id ID to use for the numeric field
#' @param cardinal_id ID to use for the cardinality field
#' @param label Label
#' @param value Default value
#' @param cardinality Default cardinality
#' @param step Passed to \code{\link[shiny]{numericInput}}
#' @param min Passed to \code{\link[shiny]{numericInput}}
#' @param max Passed to \code{\link[shiny]{numericInput}}
#'
#' @return out An HTML tag object that can be rendered as HTML using
#' as.character()

cardinalNumericField <- function(id, cardinal_id, label, value, cardinality = "<=", step = NA, min = NA, max = NA) {
  tags$div(
    fluidRow(column(4, HTML(paste0("<b>", label, ":</b>&nbsp;"))), column(3, selectInput(cardinal_id, label = NULL, choices = c(
      "<=", ">=", ">= or <= -",
      "<= and >= -"
    ), selected = cardinality), selectize = FALSE), column(5, numericInput(id, label = NULL, value = value, min = min, max = max, step = step))),
    class = "shinyngs-cardinalfield"
  )
}

#' Evaluate a vector of values with respect to a limit and a cardinality, being
#' '>', '<' , '> or <-' (e.g. a fold change above a limit in + or -
#' directions), or '< and >-' (not a above a limit in + or -).
#'
#' @param values Vector of numeric values
#' @param cardinality Cardinality: '>', '<' , '> or <-'
#' @param limit Numeric limit
#'
#' @return out A logical vector

evaluateCardinalFilter <- function(values, cardinality, limit) {
  if (cardinality == "<=") {
    (!is.na(values)) & values <= limit
  } else if (cardinality == ">=") {
    (!is.na(values)) & values >= limit
  } else if (cardinality == ">= or <= -") {
    (!is.na(values)) & abs(values) >= limit
  } else if (cardinality == "<= and >= -") {
    (!is.na(values)) & values <= limit & values >= -limit
  } else {
    stop("invalid cardinality")
  }
}

#' Build an ExploratorySummarisedExperimentList from a YAML description
#'
#' Building ExploratorySummarisedExperimentList objects can be a bit fiddly.
#' This function makes automates object construction based on a descriptor
#' in yaml format.
#'
#' For a simple study with one 'experiement' for Gene-level results, and three
#' 'assays' describing raw, filtered and normalised expression you might make
#' a YAML like:
#' ```
#' title: My RNA seq experiment
#' author: Joe Blogs
#' report: report.md
#' group_vars:
#'   - Group
#'   - Replicate
#' default_groupvar: Group
#' experiments:
#'   Gene:
#'     coldata:
#'       file: my.experiment.csv
#'       id: External
#'     annotation:
#'       file: my.annotation.csv
#'       id: gene_id
#'       entrez: ~
#'       label: gene_id
#'     expression_matrices:
#'       Raw:
#'         file: raw_counts.csv
#'         measure: counts
#'       Filtered:
#'         file: filtered_counts.csv
#'         measure: Counts per million
#'       Normalised:
#'         file: normalised_counts.csv
#'         measure: Counts per million
#'     read_reports:
#'       read_attrition: read_attrition.csv
#' contrasts:
#'   comparisons:
#'   - Variable: Group
#'     Group.1: control
#'     Group.2: TreatmentA
#'   - Variable: Group
#'     Group.1: control
#'     Group.2: TreatmentB
#' contrast_stats:
#'   Gene:
#'     Normalised:
#'       pvals: pvals.csv
#'       qvals: qvals.csv
#' ```
#' @param configfile A YAML-format config file describing the data to be
#'   compiled into an ExploratorySummarizedExperimentList object
#'
#' @import yaml
#' @return out An ExploratorySummarizedExperimentList object suitable for passing to \code{\link{prepareApp}}
#' @export
#'
#' @md
#' @examples
#' eselist <- eselistFromYAML("my.yaml")
#'
eselistFromYAML <- function(configfile) {
  config <- yaml::yaml.load_file(configfile)

  eselistFromList(config)
}

#' Reads gene enrichment files
#' @noRd
#' @param contrast_spec One of:
#' - \code{NULL} (meaning no enrichment was analyzed for that contrast)
#' - a path to a file (e.g. the table output from roast)
#' - a named list with elements "up" and "down" with paths to files (e.g.
#'  corresponding to gsea up-regulated and down-regulated output tables).
#'
#'  The two tables from GSEA output will be combined into a single data frame. A column "Direction" with
#'  values "Up" and "Down" will be added.
#'
#' @returns A data frame with the file contents (or \code{NULL})
#'
read_enrichment_file <- function(contrast_spec) {
  # contrast_spec may be one file name or two file names (up and down), or NULL
  if (is.null(contrast_spec) || length(contrast_spec) == 0) {
    return(NULL)
  }

  read_one <- function(path) {
    read.csv(path,
      sep = getSeparator(path), check.names = FALSE,
      row.names = 1
    )
  }

  if (length(contrast_spec) == 1) {
    return(read_one(contrast_spec))
  }

  if (length(contrast_spec) == 2) {
    # This is useful for GSEA output, that splits up and down in two tsv files.
    # We read both files and set the direction
    up <- read_one(contrast_spec[["up"]])
    if (nrow(up) > 0) {
      up$Direction <- "Up"
    }
    down <- read_one(contrast_spec[["down"]])
    if (nrow(down) > 0) {
      down$Direction <- "Down"
    }
    return(rbind(up, down))
  }

  stop("gene_set_analyses should have zero, one or two contrast files per gene_set_type")
}

#' Build an ExploratorySummarisedExperimentList from a description provided in a list
#'
#' @param config Hierachical named list with input components. See \code{eselistFromYAML} for detail.
#' @param log2_threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param log2_assays A string parameter that can be NULL, empty, or a non-empty string.
#'                     If NULL: log2 transformation will be guessed based on input assays.
#'                     If empty: no log2 transformation will be applied.
#'                     If non-empty: log2 transformation will be applied unconditionally to specified assays.
#'
#' @return out An ExploratorySummarizedExperimentList object suitable for passing to \code{\link{prepareApp}}
#' @export

eselistfromConfig <-
  function(config, log2_assays, log2_threshold = 30) {
    # 'Experiments' are sets of results from a common set of samples

    experiments <- config$experiments

    # Establish ordering. Ordering of YAML file shouldn't be relied upon

    if ("experiment_order" %in% names(config)) {
      experiment_order <- config$experiment_order
    } else {
      experiment_order <- names(experiments)
    }

    experiments <- experiments[experiment_order]

    # Make the basic objects

    print("Constructing ExploratorySummarizedExperiments")

    expsumexps <- lapply(structure(names(experiments), names = names(experiments)), function(expname) {
      exp <- experiments[[expname]]

      # Read feature metadata

      colData <- read_metadata(
        filename = exp$coldata$file,
        id_col = exp$coldata$id,
      )
      annotation <-
        read_metadata(
          exp$annotation$file,
          id_col = exp$annotation$id
        )

      # Read the expression data

      assays <- rev(lapply(exp$expression_matrices, function(mat) {
        print(paste("Reading", mat$file))
        read_matrix(
          mat$file,
          sample_metadata = colData,
          feature_metadata = annotation,
          row.names = 1
        )
      }))

      # If specified, ensure data is unlogged before it's loaded

      assays <- cond_log2_transform_assays(
        assays,
        log2_assays = log2_assays,
        threshold = log2_threshold,
        prettify_names = TRUE,
        reverse = TRUE,
        invert_assays = TRUE
      )

      # Apply ordering if provided

      if ("assay_order" %in% names(exp)) {
        assay_order <- exp$assay_order
      } else {
        assay_order <- names(assays)
      }
      assays <- assays[assay_order]

      # Add contrast_stats where available.

      contrast_stats <- list()
      if (expname %in% names(config$contrasts$stats)) {
        contrast_stats <- lapply(config$contrasts$stats[[expname]], function(assaytests) {
          # 'Uncompiled' means that stats are still stored in separate files for
          # each contrast, as they might come from DESeq etc. We just have to
          # separate them and compile ourselves.

          if ("type" %in% names(assaytests) && assaytests$type == "uncompiled") {
            fold_change_scale <- assaytests$fold_change_scale
            if (is.null(fold_change_scale)) {
              # Back-compat with configs written before fold_change_scale existed
              fold_change_scale <- if (isTRUE(assaytests$unlog_foldchanges)) "log2" else "auto"
            }

            compile_contrast_data(
              differential_stats_files = assaytests$files,
              feature_id_column = assaytests$feature_id_column,
              fc_column = assaytests$fc_column,
              pval_column = assaytests$pval_column, qval_column = assaytests$qval_column,
              fold_change_scale = fold_change_scale
            )
          } else {
            lapply(assaytests, function(at) {
              print(paste("Reading test stats file", at))
              read.csv(at, row.names = 1, header = FALSE)
            })
          }
        })
      }

      # Basic list to pass to object creation

      ese_list <- list(
        assays = assays,
        colData = colData,
        annotation = annotation,
        idfield = exp$annotation$id,
        entrezgenefield = exp$annotation$entrez,
        labelfield = exp$annotation$label,
        assay_measures = lapply(exp$expression_matrices, function(mat) {
          mat$measure
        }),
        contrast_stats = contrast_stats
      )

      if ("read_reports" %in% names(exp)) {
        ese_list$read_reports <- lapply(exp$read_reports, function(rrfile) read.csv(rrfile, row.names = 1, check.names = FALSE))
      }

      if ("gene_set_analyses" %in% names(exp)) {
        ese_list$gene_set_analyses <- lapply(exp$gene_set_analyses, function(assay) {
          lapply(assay, function(gene_set_type) {
            lapply(gene_set_type, read_enrichment_file)
          })
        })

        # Drop gene set types and assays with no results, but keep every
        # contrast position (NULLs included) so that downstream indexing by
        # contrast number stays aligned with the containing contrasts.
        ese_list$gene_set_analyses <- drop_empty_gene_set_analyses(ese_list$gene_set_analyses)
        # The constructor reconciles/validates the tool spec against the tables.
        ese_list$gene_set_analyses_tool <- exp$gene_set_analyses_tool
      }

      do.call(ExploratorySummarizedExperiment, ese_list)
    })

    # Parse contrasts if they weren't provided as a list directly

    if ("comparisons_file" %in% names(config$contrasts)) {
      config$contrasts$comparisons <-
        read_contrasts(config$contrasts$comparisons_file,
          colData(expsumexps[[1]]),
          convert_to_list = TRUE
        )
    }

    # Check that number of differential results sets is equal to number of contrasts

    for (ese in expsumexps) {
      if (ncol(ese@contrast_stats[[1]]$fold_changes) != length(config$contrasts$comparisons)) {
        stop(paste0("Number of supplied contrasts (", length(config$contrasts$comparisons), ") not equal to the number of sets of differential statistics supplied (", ncol(ese@contrast_stats[[1]]$fold_changes), ")"))
      }
    }

    print("Creating ExploratorySummarizedExperimentList")

    eselist_args <- list(
      expsumexps,
      title = config$title,
      author = config$author,
      group_vars = config$group_vars,
      default_groupvar = config$default_groupvar,
      contrasts = lapply(config$contrasts$comparisons, function(x) unlist(x))
    )

    # Optional things

    if ("static_pdf" %in% names(config)) {
      eselist_args$static_pdf <- config$static_pdf
    }

    # If 'report' is specified with assume a mardown document to be parsed. Otherwise just text

    if ("report" %in% names(config)) {
      eselist_args$description <- as.character(includeMarkdown(config$report))
    } else if ("description" %in% names(config)) {
      eselist_args$description <- config$description
    }

    if ("url_roots" %in% names(config)) {
      eselist_args$url_roots <- config$url_roots
    }

    if ("gene_set_id_type" %in% names(config) && "gene_sets" %in% names(config)) {
      eselist_args$gene_set_id_type <- config$gene_set_id_type
      eselist_args$gene_sets <- lapply(config$gene_sets, GSEABase::getGmt)
    }

    eselist <- do.call(ExploratorySummarizedExperimentList, eselist_args)

    eselist
  }

#' Drop empty gene set types and assays from a gene_set_analyses structure
#' @noRd
#' @param gene_set_analyses A three-level nested list keyed by assay, gene set
#' type and contrast. Contrast entries may be \code{NULL} where no enrichment
#' result was supplied.
#'
#' @return The same structure with gene set types that hold no results, and
#' assays left with no gene set types, removed. Contrast positions are kept
#' intact (including \code{NULL}s) so that indexing by contrast number stays
#' aligned with the containing contrasts.
drop_empty_gene_set_analyses <- function(gene_set_analyses) {
  gene_set_analyses <- lapply(gene_set_analyses, function(assay) {
    Filter(function(gene_set_type) any(!vapply(gene_set_type, is.null, logical(1))), assay)
  })
  Filter(function(assay) length(assay) > 0, gene_set_analyses)
}

#' Read an expression matrix file and match to specified samples and features
#'
#' @param matrix_file Matrix file
#' @param sample_metadata Data frame of sample metadata
#' @param feature_metadata Data fraome of feature metadata
#' @param sep Sepaarator in matrix file
#' @param row.names Matrix column number or name containing feature identifiers
#'
#' @return output Numeric matrix
#' @export

read_matrix <- function(matrix_file, sample_metadata, feature_metadata = NULL, sep = NULL, row.names = 1) {
  if (is.null(sep)) {
    sep <- getSeparator(matrix_file)
  }
  matrix_data <-
    read.delim(
      matrix_file,
      check.names = FALSE,
      header = TRUE,
      sep = sep,
      row.names = row.names
    )

  if (any(!rownames(sample_metadata) %in% colnames(matrix_data))) {
    missing <-
      rownames(sample_metadata)[(!rownames(sample_metadata) %in% colnames(matrix_data))]
    stop(
      paste0(
        "Some sample metadata names (",
        paste(missing, collapse = ","),
        ") are absent from the matrix in ",
        matrix_file,
        ", columns are: ",
        paste(colnames(matrix_data), collapse = ",")
      )
    )
  }

  # Allow for feature names appearing in the first column

  if (!is.null(feature_metadata)) {
    if (!any(rownames(feature_metadata) %in% rownames(matrix_data))) {
      rownames(matrix_data) <- matrix_data[, 1]
      if (!any(rownames(feature_metadata) %in% rownames(matrix_data))) {
        stop(paste("All feature metadata names are absent from the matrix in", matrix_file))
      }
    }
  }

  as.matrix(matrix_data[, rownames(sample_metadata), drop = FALSE])
}

#' Read a metadata file
#'
#' @param filename File name
#' @param id_col Identifier column in the file
#' @param sep File separator
#' @param stringsAsFactors Passed to \code{read.delim}
#'
#' @export
#'
#' @return output Data frame

read_metadata <- function(filename, id_col = NULL, sep = NULL, stringsAsFactors = FALSE) {
  if (is.null(sep)) {
    sep <- getSeparator(filename)
  }

  if (!file.exists(filename)) {
    stop(paste("Metadata file", filename, "does not exist."))
  } else {
    metadata <-
      read.delim(
        filename,
        sep = sep,
        check.names = FALSE,
        header = TRUE,
        stringsAsFactors = stringsAsFactors
      )
  }

  if (!is.null(id_col)) {
    if (is.character(id_col) && !id_col %in% colnames(metadata)) {
      stop(
        paste0(
          "Metadata ID column (",
          id_col,
          ") does not exist in metadata ",
          paste(colnames(metadata), collapse = ","),
          " from file ",
          filename
        )
      )
    }

    metadata <- metadata[match(unique(metadata[[id_col]]), metadata[[id_col]]), ]
    rownames(metadata) <- metadata[[id_col]]
  }
  return(metadata)
}


#' Check one list is a subset of another and throw an error if not
#'
#' @param test_list  Test list
#' @param reference_list Reference list
#' @param test_list_name Name of test list for error
#' @param reference_list_name Name of reference list for error
#'
#' @return output Returns TRUE if check passes
#' @export

checkListIsSubset <- function(test_list,
                              reference_list,
                              test_list_name,
                              reference_list_name) {
  if (!all(test_list %in% reference_list)) {
    missing_vars <- test_list[!test_list %in% reference_list]
    stop(
      paste0(
        "Not all ",
        test_list_name,
        " are available in the ",
        reference_list_name,
        ".\n",
        "Missing ", test_list_name, ": ", paste(missing_vars, collapse = ", "), "\n",
        "Available ", reference_list_name, ": ", paste(unique(reference_list), collapse = ", ")
      )
    )
  }
  TRUE
}

#' Remove random effects from a model formula
#'
#' @param formula_string Formula string that may contain random effects
#'
#' @return output Fixed-effects-only formula
fixedEffectsFormula <- function(formula_string) {
  reformulas::nobars(as.formula(formula_string))
}

#' Build a model matrix from the fixed-effects part of a formula
#'
#' @param formula_string Formula string that may contain random effects
#' @param samples Data frame of sample information
#'
#' @return output Model matrix for the fixed-effects formula
fixedEffectsModelMatrix <- function(formula_string, samples) {
  model.matrix(fixedEffectsFormula(formula_string), data = samples)
}

#' Validate a formula-based contrast string against fixed-effect coefficients
#'
#' @param contrast_id Contrast identifier
#' @param contrast_formula Formula string used for the contrast
#' @param contrast_string Contrast string to validate
#' @param model_matrix Optional precomputed model matrix for the fixed-effects formula
#' @param samples Data frame of sample information
#'
#' @return output Returns TRUE if validation passes
validateFormulaBasedContrast <- function(contrast_id,
                                         contrast_formula,
                                         contrast_string,
                                         model_matrix = NULL,
                                         samples) {
  if (is.null(model_matrix)) {
    model_matrix <- fixedEffectsModelMatrix(contrast_formula, samples)
  }
  model_coefficients <- make.names(colnames(model_matrix), unique = TRUE)

  tryCatch(
    limma::makeContrasts(contrasts = contrast_string, levels = model_coefficients),
    error = function(e) {
      stop(
        paste0(
          "Contrast id '", contrast_id, "' has invalid make_contrasts_str '", contrast_string,
          "' for formula '", contrast_formula, "'. ",
          "Available coefficient names for make_contrasts_str: ",
          paste(model_coefficients, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  )
}


#' Read and validate a contrasts file against sample metadata
#'
#' Checks:
#' 1. No duplicate contrast IDs. Ensure that the required columns (variable, reference, target) are present.
#' 2. Values in the contrast variable column exist as column names in the sample metadata.
#' 3. If blocking factors are supplied, checks that they are present in the sample metadata.
#' 4. Design matrix is full rank.
#' 5. Warn about continuous covariates (e.g. numeric patient IDs treated as continuous).
#' 6. Values of specified columns don't contain special characters.
#' 7. Verify that the specified reference and target values exist in the corresponding sample metadata column.
#' 8. Issue a warning if the reference and target levels are identical.
#'
#' @param filename Contrasts file
#' @param samples Data frame of sample information
#' @param variable_column Column in contrasts file referencing sample sheet
#'   column
#' @param reference_column Column in contrast file referencing reference level
#'   of sample sheet variable
#' @param target_column Column in contrast file referencing target level of
#'   sample sheet variable
#' @param blocking_column Colon-separated column in contrast file referencing
#'   sample sheet variables to be used as blocking factors
#' @param convert_to_list Convert output to a list as used internally by
#'   shinyngs?
#' @param validate_design Validate design matrix (check for NAs, full rank,
#'   numeric columns, special characters)? Set to FALSE to skip these checks.
#'
#' @return output Validated contrasts data frame
#' @export

read_contrasts <-
  function(filename,
           samples,
           variable_column = "variable",
           reference_column = "reference",
           target_column = "target",
           blocking_column = "blocking",
           convert_to_list = FALSE,
           validate_design = TRUE) {
    # Read the contrasts depending on the file format (CSV or YAML)
    if (grepl("\\.csv$", filename)) {
      contrasts <- read_metadata(filename)
      # Check for duplicates in constrasts id
      if (any(duplicated(contrasts$id))) {
        stop("Duplicate contrast ids found in CSV contrasts file.")
      }
      contrast_cols <- c(variable_column, reference_column, target_column)
      if (!blocking_column %in% names(contrasts)) {
        contrasts[[blocking_column]] <- NA
      }

      # Check contrast headers are as expected
      if (!all(contrast_cols %in% colnames(contrasts))) {
        stop(paste("Contrasts file must contain all of", paste(contrast_cols, collapse = ", ")))
      }
    } else if (grepl("\\.ya?ml$", filename)) {
      contrasts_yaml <- yaml::read_yaml(filename)

      if (is.null(contrasts_yaml$contrasts)) {
        stop("YAML file must contain a 'contrasts' section.")
      }

      # Parse YAML contrasts into a data frame
      contrasts <- do.call(rbind, lapply(contrasts_yaml$contrasts, function(x) {
        stopifnot(!is.null(x$id)) # Require that each contrast has an 'id'

        row <- data.frame(
          id = x$id,
          variable = NA, reference = NA, target = NA,
          blocking = NA,
          exclude_samples_col = NA,
          exclude_samples_values = NA,
          formula = NA,
          make_contrasts_str = NA
        )

        # A contrast can be defined in two ways:
        #
        # 1 . Specifying a column in the sample sheet, and two values from that column which define two groups of samples to compare. Tools using a contrast defined in this way would need to generate a model and contrast using that information.
        # 2. Specifying the formula and contrast string explicitly. Tools using those contrasts can then pass these directly to function of suites such as DESeq2.

        if (!is.null(x$comparison)) {
          if (!is.null(x$formula) || !is.null(x$make_contrasts_str)) {
            stop(sprintf("Contrast id '%s' with 'comparison' must not have 'formula' or 'make_contrasts_str'.", x$id))
          }

          fields <- setNames(rep(NA, 3), c("variable", "reference", "target"))
          fields[names(fields)[seq_along(x$comparison)]] <- x$comparison

          if (any(is.na(fields)) || any(fields == "")) {
            stop(sprintf("Contrast id '%s' must provide non-empty 'variable', 'reference', and 'target' fields.", x$id))
          }

          row$variable <- fields["variable"]
          row$reference <- fields["reference"]
          row$target <- fields["target"]

          if (!is.null(x$blocking_factors)) {
            row$blocking <- paste(x$blocking_factors, collapse = ";")
          }
        } else if (!is.null(x$formula)) {
          if (is.null(x$make_contrasts_str) || !is.null(x$blocking_factors)) {
            stop(sprintf("Contrast id '%s' with 'formula' must have 'make_contrasts_str' and no 'blocking_factors'.", x$id))
          }

          row$formula <- x$formula
          row$make_contrasts_str <- x$make_contrasts_str
        } else {
          stop(sprintf("Contrast id '%s' must provide either 'comparison' or 'formula' + 'make_contrasts_str'.", x$id))
        }

        if (!is.null(x$exclude_samples_col) || !is.null(x$exclude_samples_values)) {
          if (is.null(x$exclude_samples_col) || is.null(x$exclude_samples_values)) {
            stop(sprintf("Contrast id '%s' must provide both 'exclude_samples_col' and 'exclude_samples_values'.", x$id))
          }

          row$exclude_samples_col <- x$exclude_samples_col
          row$exclude_samples_values <- paste(x$exclude_samples_values, collapse = ";")
        }

        row
      }))
      if (any(duplicated(contrasts$id))) {
        stop("Duplicate contrast ids found in YAML contrasts file.")
      }
    } else {
      stop("Invalid file format. Please provide a CSV or YAML file.")
    }

    # Check contrast content is appropriate to sample sheet
    variables_without_na <- na.omit(contrasts$variable)
    if (length(variables_without_na) > 0) {
      success <- checkListIsSubset(variables_without_na, colnames(samples), "contrast variables", "sample metadata")
    }
    # Check blocking variables, where supplied
    blocking <- unlist(lapply(contrasts[[blocking_column]], function(x) simpleSplit(x, ";")))
    blocking <- blocking[!is.na(blocking)]
    if (length(blocking) > 0) {
      success <- checkListIsSubset(blocking, colnames(samples), "blocking variables", "sample metadata")
    }
    if ("exclude_samples_col" %in% colnames(contrasts)) {
      exclude_cols <- na.omit(contrasts$exclude_samples_col)
      if (length(exclude_cols) > 0) {
        success <- checkListIsSubset(exclude_cols, colnames(samples), "exclude sample columns", "sample metadata")
      }
    }

    # Ensure reference and target are valid for their variable
    for (i in seq_len(nrow(contrasts))) {
      blocking_vars <- simpleSplit(contrasts[[blocking_column]][i], ";")
      design_cols <- character(0)

      # Extract design matrix columns from contrasts: the variable column plus any blocking factors.
      # For formula-based contrasts, extract variables from the formula itself.
      if (validate_design) {
        # Filter samples if exclude columns are specified for this contrast
        contrast_samples <- samples
        if ("exclude_samples_col" %in% colnames(contrasts) && "exclude_samples_values" %in% colnames(contrasts)) {
          if (!is.na(contrasts$exclude_samples_col[i]) && !is.na(contrasts$exclude_samples_values[i])) {
            exclude_col <- contrasts$exclude_samples_col[i]
            exclude_vals <- simpleSplit(contrasts$exclude_samples_values[i], ";")
            contrast_samples <- samples[!samples[[exclude_col]] %in% exclude_vals, , drop = FALSE]
          }
        }

        if ("formula" %in% colnames(contrasts) && !is.na(contrasts$formula[i])) {
          design_cols <- unique(all.vars(as.formula(contrasts$formula[i])))
          success <- checkListIsSubset(design_cols, colnames(samples), "formula variables", "sample metadata")
          mm <- fixedEffectsModelMatrix(contrasts$formula[i], contrast_samples)

          validateFormulaBasedContrast(
            contrast_id = contrasts[i, "id"],
            contrast_formula = contrasts$formula[i],
            contrast_string = contrasts$make_contrasts_str[i],
            model_matrix = mm,
            samples = contrast_samples
          )
        } else {
          design_cols <- unique(na.omit(c(contrasts[[variable_column]][i], blocking_vars)))
          mm <- model.matrix(~ . - 1, data = contrast_samples[, design_cols, drop = FALSE])
        }

        design_matrix <- contrast_samples[, design_cols, drop = FALSE]

        # Ensure there are no NA values in the design matrix.
        if (any(is.na(design_matrix))) {
          stop("NA values found in one or more design matrix columns.")
        }

        # Check that the design matrix is full rank.
        if (qr(mm)$rank < ncol(mm)) {
          stop(paste("Design matrix is not full rank.", "Model matrix columns:", paste(colnames(mm), collapse = ", "), "\n"))
        }

        # Warn about continuous covariates in the design matrix columns.
        for (col in design_cols) {
          if (is.numeric(samples[[col]])) {
            warning(paste("Column", col, "is numeric and may be treated as continuous."))
          }
        }

        # Check that values in design matrix columns do not contain disallowed special characters.
        for (col in design_cols) {
          vals <- as.character(samples[[col]])
          for (sc in c("/", "\\\\")) { # Default special characters: c("/", "\\\\")
            if (any(grepl(sc, vals))) {
              warning(paste(
                "Column", col, "contains special character", sc,
                "which may cause issues downstream."
              ))
            }
          }
        }
      }

      var <- contrasts[i, variable_column]
      ref <- contrasts[i, reference_column]
      tgt <- contrasts[i, target_column]

      # Only check values if both reference and target are present
      if (!is.na(ref) && !is.na(tgt)) {
        success <- checkListIsSubset(ref, samples[[var]], "contrast levels", "sample metadata variable")
        success <- checkListIsSubset(tgt, samples[[var]], "contrast levels", "sample metadata variable")

        if (ref == tgt) {
          warning(sprintf("Contrast id '%s' has identical reference and target levels.", contrasts[i, "id"]))
        }
      }
    }

    # Convert contrasts to a list if requested
    if (convert_to_list) {
      contrasts <- apply(contrasts, 1, function(x) {
        conlist <- split(unname(x), names(x))[names(x)]
        rename <- c("variable" = "Variable", "reference" = "Group.1", "target" = "Group.2")
        rename_ind <- match(names(rename), names(conlist))
        names(conlist)[rename_ind] <- rename
        nonempty <- unlist(lapply(conlist, function(y) !(is.na(y) || is.null(y) || grepl("^\\s*$", y))))
        conlist[nonempty]
      })
    }

    contrasts
  }


#' Guess whether fold change values are on a log2 or linear scale
#'
#' Shinyngs stores fold changes internally as signed linear values (the ratio
#' of the two conditions, with an absolute magnitude of at least 1 and the
#' sign indicating direction - see \code{\link{foldChange}}), so a value
#' strictly between -1 and 1 is only possible on a log2 scale (e.g. a log2
#' fold change of 0.5 is a ~1.4-fold change). log2 fold changes are also
#' typically small and symmetric around zero, while linear fold changes can
#' have very large magnitudes for the same effect size (e.g. a 1000-fold
#' change is a log2 fold change of ~10).
#'
#' @param values Numeric vector of fold change values (NAs and non-finite
#'   values are ignored).
#' @param log2_magnitude_limit Values with an absolute magnitude greater than
#'   this are considered implausible for a log2 fold change (default 15,
#'   corresponding to a ~32,000-fold change).
#'
#' @return One of \code{"log2"}, \code{"linear"} or \code{"ambiguous"}.
#' @export
#' @examples
#' guess_foldchange_scale(c(-2.1, 0.3, 1.8, -0.05))
#' guess_foldchange_scale(c(-8, 2, 45, -120))
#' guess_foldchange_scale(c(-2, 3, 5, -8))
guess_foldchange_scale <- function(values, log2_magnitude_limit = 15) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return("ambiguous")
  }

  abs_values <- abs(values)

  has_sub_unity <- any(abs_values > 0 & abs_values < 1)
  has_large_magnitude <- any(abs_values > log2_magnitude_limit)

  if (has_sub_unity) {
    return("log2")
  }
  if (has_large_magnitude) {
    return("linear")
  }
  "ambiguous"
}

#' Resolve the scale of a fold-change column, cross-checking a user
#' declaration and a column-naming convention against the observed data
#'
#' Mirrors the log2 guessing done for assay matrices
#' (\code{\link{cond_log2_transform_matrix}}) but for differential statistics
#' fold changes, and errors rather than silently proceeding when the
#' available signals disagree - see
#' \url{https://github.com/pinin4fjords/shinyngs/issues/125}.
#'
#' @param values Numeric vector of fold change values.
#' @param fc_column Name of the column the values came from, used for the
#'   \code{log2FoldChange}-style naming heuristic. Can be \code{NULL} if
#'   unknown.
#' @param declared_scale One of \code{"auto"} (default), \code{"log2"} or
#'   \code{"linear"}.
#'
#' @return Either \code{"log2"} or \code{"linear"}.
#' @export
#' @examples
#' resolve_foldchange_scale(c(-2.1, 0.3, 1.8), fc_column = "log2FoldChange")
#' resolve_foldchange_scale(c(-8, 45, -120), fc_column = "FoldChange")
resolve_foldchange_scale <- function(values, fc_column = NULL, declared_scale = "auto") {
  declared_scale <- match.arg(declared_scale, c("auto", "log2", "linear"))
  if (!is.null(fc_column) && is.na(fc_column)) {
    fc_column <- NULL
  }

  column_label <- if (is.null(fc_column)) "the fold change column" else paste0("'", fc_column, "'")
  name_hint <- if (!is.null(fc_column) && grepl("log2", fc_column, ignore.case = TRUE)) "log2" else NULL
  distribution_guess <- guess_foldchange_scale(values)

  contradicts <- function(a, b) !is.null(a) && !is.null(b) && b != "ambiguous" && a != b

  if (declared_scale != "auto") {
    if (contradicts(declared_scale, distribution_guess)) {
      stop(paste0(
        "--fold_change_scale/fold_change_scale was set to '", declared_scale, "', but the values in ",
        column_label, " look like ", distribution_guess, " fold changes (values with an absolute ",
        "magnitude below 1 imply log2; very large excursions imply linear). Check fold_change_scale ",
        "and fc_column, or pass the correct scale explicitly."
      ))
    }
    if (contradicts(declared_scale, name_hint)) {
      stop(paste0(
        "fold_change_scale was set to '", declared_scale, "', but the column name ", column_label,
        " conventionally holds log2 fold changes. Check fold_change_scale and fc_column."
      ))
    }
    return(declared_scale)
  }

  if (contradicts(name_hint, distribution_guess)) {
    stop(paste0(
      "Column name ", column_label, " suggests log2 fold changes, but the observed values look like ",
      distribution_guess, " fold changes. Set fold_change_scale explicitly ('log2' or 'linear') to resolve this."
    ))
  }

  if (distribution_guess != "ambiguous") {
    return(distribution_guess)
  }
  if (!is.null(name_hint)) {
    return(name_hint)
  }

  stop(paste0(
    "Could not determine whether the fold change values in ", column_label, " are log2 or linear: ",
    "the column name gives no hint and the distribution is ambiguous (no values below 1 in magnitude, ",
    "none very large). Set fold_change_scale explicitly ('log2' or 'linear') to resolve this."
  ))
}

#' Map the deprecated \code{unlog_foldchanges}/\code{--unlog_foldchanges}
#' argument onto \code{fold_change_scale}, warning if it was used
#'
#' Shared by \code{read_differential}, \code{compile_contrast_data},
#' \code{validate_inputs} and the \code{make_app_from_files.R},
#' \code{differential_plots.R} and \code{validate_fom_components.R} scripts
#' under \code{exec/}, so the shim only needs to be written once.
#'
#' @param fold_change_scale The caller's current \code{fold_change_scale}
#'   value (used unchanged when \code{unlog_foldchanges} is \code{NULL}).
#' @param unlog_foldchanges The deprecated argument value, or \code{NULL} if
#'   it was not supplied.
#'
#' @return The \code{fold_change_scale} to use.
#' @export
resolve_deprecated_unlog_foldchanges <- function(fold_change_scale, unlog_foldchanges) {
  if (is.null(unlog_foldchanges)) {
    return(fold_change_scale)
  }

  warning(
    "`unlog_foldchanges`/`--unlog_foldchanges` is deprecated and will be removed in a future release; ",
    "use `fold_change_scale`/`--fold_change_scale` = \"log2\" (to unlog) or \"linear\" (to leave values ",
    "as-is) instead.",
    call. = FALSE
  )
  if (isTRUE(unlog_foldchanges)) "log2" else "linear"
}

# Unlog a signed fold change value from log2 to linear scale (see
# resolve_foldchange_scale()).
unlog_fold_change <- function(x) {
  sign(x) * 2^(abs(x))
}

# Read the feature id/p value/q value/fold change columns of a differential
# statistics file, without resolving or applying any fold change scale.
# Shared by read_differential() (which resolves/applies the scale for a
# single file) and compile_contrast_data() (which combines several files'
# raw fold changes before resolving the scale once across all of them).
read_stats_table <- function(filename, feature_id_column = NULL, pval_column = NULL, qval_column = NULL, fc_column = NULL) {
  st <- read_metadata(filename, id_col = feature_id_column)
  stats_cols <- c(feature_id_column, pval_column, qval_column, fc_column)

  success <-
    checkListIsSubset(
      stats_cols,
      colnames(st),
      "stats variables",
      "available stats columns"
    )
  st <- st[, stats_cols]

  for (c in colnames(st)) {
    st[[c]][grep("^ *NA$", st[[c]])] <- NA
    if (c %in% c(pval_column, qval_column, fc_column)) {
      st[[c]] <- as.numeric(st[[c]])
    }
  }
  st
}

#' Read tables of differential statistics
#'
#' @param filename File name of file with table of differential statistics
#' @param feature_id_column Column of stats file with feature identifiers
#' @param pval_column Column of stats file with p values
#' @param qval_column Column of stats file with adjust p values/ q values
#' @param fc_column Column of stats with fold changes
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default, infer and validate from the column name and data
#'   distribution), \code{"log2"} or \code{"linear"}. See
#'   \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Reverse a log on fold changes? Set to TRUE if values are logged.
#'
#' @return output Validated selected columns of differential stats files as a
#'   data frame, with the resolved scale attached as the \code{fold_change_scale}
#'   attribute.
#' @export

read_differential <- function(filename,
                              feature_id_column = NULL,
                              pval_column = NULL,
                              qval_column = NULL,
                              fc_column = NULL,
                              fold_change_scale = "auto",
                              unlog_foldchanges = NULL) {
  fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

  st <- read_stats_table(
    filename = filename,
    feature_id_column = feature_id_column,
    pval_column = pval_column,
    qval_column = qval_column,
    fc_column = fc_column
  )

  resolved_scale <- resolve_foldchange_scale(
    values = st[[fc_column]],
    fc_column = fc_column,
    declared_scale = fold_change_scale
  )

  if (resolved_scale == "log2") {
    st[[fc_column]] <- unlog_fold_change(st[[fc_column]])
  }

  attr(st, "fold_change_scale") <- resolved_scale
  st
}

#' Compile contrast stats for inclusion in shinyngs
#'
#' @param differential_stats_files Tabular files with differential stats
#' @param pval_column P value column in stats files
#' @param qval_column Q value column in stats files
#' @param fc_column Fold change column in stats files
#' @param feature_id_column Feature identifier column in stats files
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default), \code{"log2"} or \code{"linear"}. Resolved once
#'   across the fold changes combined from all \code{differential_stats_files},
#'   rather than per-file, so contrasts from the same experiment are treated
#'   consistently. See \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Should fold change values be unlogged?
#'
#' @return output A named list of data frames by statistic, number of columns equal to input file number

compile_contrast_data <-
  function(differential_stats_files,
           feature_id_column = NULL,
           pval_column = NULL,
           qval_column = NULL,
           fc_column = NULL,
           fold_change_scale = "auto",
           unlog_foldchanges = NULL) {
    fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

    # Read stats and make sure they're numeric. Fold changes are read raw here
    # (no scale applied) so that the scale can be resolved once below, across
    # the fold changes combined from every contrast file.

    contrast_stats <- lapply(differential_stats_files, function(dsf) {
      read_stats_table(
        filename = dsf,
        feature_id_column = feature_id_column,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column
      )
    })

    contrast_stats_rearranged <- list()

    add_to_stats <- function(source) {
      df <- do.call(cbind, lapply(contrast_stats, function(x) {
        x[, source, drop = FALSE]
      }))
      names(df) <- paste0("V", seq_len(ncol(df)))
      rownames(df) <- contrast_stats[[1]][[feature_id_column]]
      df
    }

    if (!is.null(fc_column)) {
      fold_changes <- add_to_stats(source = fc_column)

      resolved_scale <- resolve_foldchange_scale(
        values = unlist(fold_changes),
        fc_column = fc_column,
        declared_scale = fold_change_scale
      )

      if (resolved_scale == "log2") {
        fold_changes[] <- lapply(fold_changes, unlog_fold_change)
      }

      attr(fold_changes, "fold_change_scale") <- resolved_scale
      contrast_stats_rearranged[["fold_changes"]] <- fold_changes
    }
    if (!is.null(pval_column)) {
      contrast_stats_rearranged[["pvals"]] <- add_to_stats(source = pval_column)
    }
    if (!is.null(qval_column)) {
      contrast_stats_rearranged[["qvals"]] <- add_to_stats(source = qval_column)
    }
    contrast_stats_rearranged
  }

#' Call the various read/ validate methods for input data surrounding an experiment
#'
#' @param samples_metadata Sample metadata data frame
#' @param features_metadata Feature metadata data frame
#' @param assay_files List of assay matrices
#' @param contrasts_file Contrasts definition file
#' @param sample_id_col Column of sample metadata used for identifiers
#' @param feature_id_col Column of feature metadata used for identifiers
#' @param assay_names Optional comma-separated list of assay names
#' @param differential_results Optional list of differential stats files
#' @param pval_column P value column if differential stats files specified
#' @param qval_column Q value column if differential stats files specified
#' @param fc_column Fold change column if differential stats files specified
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default, infer and validate from the column name and data
#'   distribution), \code{"log2"} or \code{"linear"}. Each differential
#'   results file is validated independently. See
#'   \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Boolean- should fold changes in stats files be unlogged?
#'
#' @return output A named list with feature/ observation components
#' @export

validate_inputs <- function(samples_metadata,
                            assay_files,
                            contrasts_file = NULL,
                            features_metadata = NULL,
                            sample_id_col = "sample",
                            assay_names = NULL,
                            differential_results = NULL,
                            feature_id_col = "gene_id",
                            pval_column = "pval_column",
                            qval_column = "qval_column",
                            fc_column = "log2FoldChange",
                            fold_change_scale = "auto",
                            unlog_foldchanges = NULL) {
  fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

  validated_parts <- list()

  # Read the sample (observation) - wise metadata

  print(paste(
    "Reading sample sheet at",
    samples_metadata,
    "with ID col",
    sample_id_col
  ))

  samples <- read_metadata(
    filename = samples_metadata,
    id_col = sample_id_col
  )
  validated_parts[[samples_metadata]] <- samples

  # Read feature-wise metadata if provided

  features <- NULL
  if (!is.null(features_metadata)) {
    print(paste(
      "Reading feature metadata at",
      features_metadata,
      "with ID col",
      feature_id_col
    ))

    features <- read_metadata(
      filename = features_metadata,
      id_col = feature_id_col
    )
    validated_parts[[features_metadata]] <- features
  }

  # Read the assay matrices

  assay_files <-
    stringsToNamedVector(
      elements_string = assay_files,
      simplify_files = FALSE,
      prettify_names = FALSE
    )

  # Read the matrices while checking samples and features match columns and rows

  validated_parts[["assays"]] <- lapply(assay_files, function(x) {
    print(paste("Reading assay matrix", x, "and validating against samples and features (if supplied)"))

    mat <- read_matrix(
      matrix_file = x,
      sample_metadata = samples,
      feature_metadata = features
    )
    print(paste("... ", x, "matrix good"))
    mat
  })

  # Read contrasts and check against sample info
  if (!is.null(contrasts_file)) {
    print("Reading contrast definitions and validating against sample sheet")
    validated_parts[[contrasts_file]] <- read_contrasts(contrasts_file, samples)
    print("... contrasts good")
  }

  if (!is.null(differential_results)) {
    contrast_stats_files <-
      stringsToNamedVector(differential_results,
        simplify_files = FALSE,
        prettify_names = FALSE
      )

    validated_parts[["differential_stats"]] <- lapply(contrast_stats_files, function(dsf) {
      read_differential(
        filename = dsf,
        feature_id_column = feature_id_col,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column,
        fold_change_scale = fold_change_scale
      )
    })
  }

  validated_parts
}

#' Check if a comma-separated string can be parsed to an integer vector
#'
#' @param string Input string
#'
#' @return output Boolean indicating whether the check passed
#' @export

# Function to check if a comma-separated string can be parsed to a positive integer vector
is_valid_positive_integer_vector <- function(string) {
  # as.integer() will truncate floats without throwing an error and also
  # accept negatives, so check if string contains NOT 0-9 or comma
  if (grepl("[^0-9,]", string)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Apply log2 transformation on a matrix.
#'
#' @param matrix_data A matrix containing data.
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#' @param threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param rmzeros A boolean indicating whether to remove zeros from the matrix.
#'                If TRUE, zeros are removed. Default is FALSE.
#' @param small_value A small value to add to zero entries before log transformation.
#' @param reverse Boolean, should we unlog rather than log?
#'
#' @return A modified matrix.
#' @export
#'
#' @examples
#' # Create a sample matrix
#' mat <- matrix(c(10, 0, 30, 0, 50, 60), ncol = 2)
#'
#' # Use the function with different parameters
#' transformed_mat1 <- cond_log2_transform_matrix(mat, should_transform = TRUE)
#' transformed_mat2 <- cond_log2_transform_matrix(mat, should_transform = NULL, threshold = 40)
#' transformed_mat3 <- cond_log2_transform_matrix(mat, rmzeros = TRUE)
cond_log2_transform_matrix <- function(matrix_data, should_transform = NULL, threshold = 30, rmzeros = FALSE, small_value = 1, reverse = FALSE) {
  # Determine if transformation is needed
  if (is.null(should_transform)) {
    if (reverse) {
      should_transform <- max(matrix_data, na.rm = TRUE) <= threshold
    } else {
      should_transform <- max(matrix_data, na.rm = TRUE) > threshold
    }
  }

  # Apply transformation based on conditions
  if (should_transform) {
    if (reverse) {
      return(2^(matrix_data))
    } else {
      # Handle zeros before any log transform - either removing them or applying a small minimum
      matrix_data[matrix_data == 0] <- if (rmzeros) NA else small_value

      return(log2(matrix_data))
    }
  }

  return(matrix_data)
}

#' Validate assay indices based on a given string.
#'
#' This function checks if the provided index string represents valid assays in the given assay data.
#' The function can handle index strings that are comma-separated integers or assay names.
#'
#' @param assay_data A list containing matrices as assay data.
#' @param index_string A string that can be a comma-separated list of integers or assay names.
#' @param invert_assays Boolean, return the indices NOT specified.
#' @param prettify_names Boolean. Prettify element names?
#'
#' @return A vector of valid indices (either as integers or assay names).
#'
#' @examples
#' assay_data_example <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))
#' valid_assays1 <- validate_indices(assay_data_example, "1,2")
#' valid_assays2 <- validate_indices(assay_data_example, "a,b")
#'
#' @export

validate_indices <- function(assay_data, index_string, invert_assays = FALSE, prettify_names = TRUE) {
  indices_are_names <- TRUE

  if (is_valid_positive_integer_vector(index_string)) {
    indices_are_names <- FALSE
    indices <- as.integer(simpleSplit(index_string))
  } else {
    indices <- simpleSplit(index_string)
    if (prettify_names) {
      indices <- unlist(lapply(indices, prettifyVariablename))
    }
  }

  valid_indices <- c(seq_along(assay_data), names(assay_data))
  invalid_indices <- indices[!indices %in% valid_indices]
  if (length(invalid_indices) > 0) {
    stop(
      "Invalid assays: ",
      paste(invalid_indices, collapse = ", "),
      ", valid indices are:",
      paste(valid_indices, collapse = ", ")
    )
  }

  if (invert_assays) {
    if (indices_are_names) {
      indices <- names(assay_data)[!names(assay_data) %in% indices]
    } else {
      indices <- setdiff(seq_along(assay_data), indices)
    }
  }

  return(indices)
}

#' Conditionally apply log2 transformation on assay data based on log2_assays parameter.
#'
#' @param assay_data A list containing matrices as assay data.
#' @param log2_assays A string parameter that can be NULL, empty, or a non-empty string.
#'                     If NULL: log2 transformation will be guessed based on input assays.
#'                     If empty: no log2 transformation will be applied.
#'                     If non-empty: log2 transformation will be applied unconditionally to specified assays.
#' @param threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param reverse Boolean, should we unlog rather than log?
#' @param invert_assays Boolean, apply transform to assays NOT specified in log2_assays.
#' @param prettify_names Boolean. Prettify element names? Passed to validate_indices().
#'
#' @return A modified assay_data list.
#' @export

cond_log2_transform_assays <- function(assay_data, log2_assays, threshold = 30, reverse = FALSE, prettify_names = TRUE, invert_assays = FALSE) {
  indices_to_transform <- c()
  should_transform <- FALSE

  # Check if log2_assays is null
  if (is.null(log2_assays)) {
    indices_to_transform <- names(assay_data)
    should_transform <- NULL
  } else if (log2_assays != "") {
    # Determine which assays to log based on log2_assays
    indices_to_transform <- validate_indices(assay_data = assay_data, index_string = log2_assays, prettify_names = prettify_names, invert_assays = invert_assays)

    should_transform <- TRUE
  }

  # Apply log2 transformation to any specified assays
  for (index in indices_to_transform) {
    assay_data[[index]] <- cond_log2_transform_matrix(matrix_data = assay_data[[index]], should_transform = should_transform, threshold = threshold, reverse = reverse)
  }

  return(assay_data)
}


#' Build path to the enrichment results
#'
#' @details
#'
#' The template accepts the following:
#'
#' \describe{
#'   \item{\code{\{contrast_name\}}}{Will be replaced by \code{contrast_info$id} argument}
#'   \item{\code{\{geneset_type\}}}{Will be replaced by the \code{geneset_type} argument}
#'   \item{\code{\{target|reference\}}}{If the \code{direction} argument is \code{"up"}, will be replaced
#'   with \code{contrast_info$target}, if it is \code{"down"}, \code{contrast_info$reference} will be used instead.}
#' }
#'
#' @param template A string, such as \code{"/path/to/folder/{contrast_name}-{geneset_type}.csv"} or
#' \code{"./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv"}
#' @param contrast_info  A list with contrast details: `id`, `reference`, and `target`,
#'   to be replaced in template.
#' @param geneset_type The name of the geneset type, to be replaced in the template
#' @param direction Either `"up"`, `"down"` or `NULL`, used to determine how the replacement will happen.
#'
#' @returns A string similar to template, but with the templates replaced
#' @export
#' @examples
#' build_enrichment_path(
#'   template = "./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv",
#'   contrast_info = list(id = "disease_vs_ctrl", reference = "control", target = "disease"),
#'   geneset_type = "m2.cp.v2024.1.Mm.entrez",
#'   direction = "up"
#' )
#'
build_enrichment_path <- function(template, contrast_info, geneset_type, direction = NULL) {
  path <- template
  path <- gsub("{contrast_name}", contrast_info$id, path, fixed = TRUE)
  path <- gsub("{geneset_type}", geneset_type, path, fixed = TRUE)
  if (!is.null(direction)) {
    target_val <- if (direction == "up") contrast_info$target else contrast_info$reference
    path <- gsub("{target|reference}", target_val, path, fixed = TRUE)
  }
  path
}

#' Detects the enrichment tool used
#'
#' @noRd
#' @param gst The enrichment table
#'
#' @returns The enrichment tool as a string, based on whether "NOM p-val" is a column ("gsea") or
#' either "p value" or "PValue" are found ("roast")
detect_enrichment_tool <- function(gst) {
  if ("NOM p-val" %in% colnames(gst)) {
    return("gsea")
  }
  if (any(c("p value", "PValue") %in% colnames(gst))) {
    return("roast")
  }
  stop("Could not detect enrichment tool from column names")
}



# Column names an enrichment mapping must provide.
enrichment_mapping_fields <- c("pvalue", "fdr", "direction")

# Is gs_tool an explicit column mapping (named pvalue/fdr/direction) rather than
# a built-in tool name? This is what lets tools other than roast/gsea be used:
# the caller names the columns directly.
is_enrichment_mapping <- function(gs_tool) {
  !is.null(names(gs_tool)) && all(enrichment_mapping_fields %in% names(gs_tool))
}

#' Get the expected column names for the gene set enrichment tool
#'
#' @noRd
#' @param gst The enrichment table.
#' @param gs_tool Either `"roast"`, `"gsea"`, or a named vector/list giving the
#' `pvalue`, `fdr` and `direction` column names directly (for other tools).
#'
#' @returns A list with elements `"pvalue"`, `"fdr"` and `"direction"`, each the
#' column name to use for that quantity.
get_enrichment_mapping <- function(gst, gs_tool) {
  if (is_enrichment_mapping(gs_tool)) {
    return(lapply(as.list(gs_tool)[enrichment_mapping_fields], as.character))
  }
  mappings <- list(
    roast = list(pvalue = "p value", fdr = "FDR", direction = "Direction"),
    gsea = list(pvalue = "NOM p-val", fdr = "FDR q-val", direction = "Direction")
  )
  if (identical(gs_tool, "roast") && "PValue" %in% colnames(gst)) {
    mappings[["roast"]][["pvalue"]] <- "PValue"
  }
  if (!is.character(gs_tool) || length(gs_tool) != 1 || !gs_tool %in% names(mappings)) {
    stop(
      "Invalid enrichment tool: ", paste(gs_tool, collapse = ", "),
      ". Use 'gsea', 'roast', or a mapping naming ", paste(enrichment_mapping_fields, collapse = "/"), "."
    )
  }
  mappings[[gs_tool]]
}

# Errors if the table is missing expected columns; returns the column mapping.
validate_enrichment_table <- function(gst, gs_tool) {
  col_map <- get_enrichment_mapping(gst, gs_tool)
  for (col in col_map) {
    if (!col %in% colnames(gst)) {
      stop(paste0(col, " column not found in gst. Found: ", paste0(colnames(gst), collapse = ", ")))
    }
  }
  invisible(col_map)
}

clean_enrichment_table <- function(gst, gs_tool) {
  if (identical(gs_tool, "gsea")) {
    # gsea tsv files have two useless columns that can be removed:
    cols_to_remove <- c("GS<br> follow link to MSigDB", "GS DETAILS")
    gst <- gst[, !(colnames(gst) %in% cols_to_remove), drop = FALSE]
  }
  gst
}

#' Resolve which gene_set_analyses entry corresponds to a selected contrast
#' @noRd
#' @param analyses The gene set analysis entries for a given assay and gene set
#' type (i.e. \code{gene_set_analyses[[assay]][[type]]}), a named list keyed by
#' contrast.
#' @param contrast_number Position of the selected contrast in the containing
#' \code{contrasts} slot.
#' @param contrast The selected contrast record from the \code{contrasts} slot,
#' either a named vector with an \code{id} (and/or \code{Variable}, \code{Group.1},
#' \code{Group.2}) or a bare \code{c(variable, reference, target)} vector.
#'
#' @return The name (character) or position (integer) to index \code{analyses}
#' with, or \code{NULL} when nothing matches. Matching by the contrast identifier
#' is preferred so a stored order that differs from the contrasts order still
#' resolves to the correct entry; otherwise it falls back to the positional index.
#' The same key is valid for the parallel \code{gene_set_analyses_tool} entries,
#' which mirror this structure.
resolve_contrast_key <- function(analyses, contrast_number, contrast) {
  entry_names <- names(analyses)
  candidates <- character(0)
  if (!is.null(names(contrast)) && "id" %in% names(contrast)) {
    candidates <- c(candidates, unname(contrast[["id"]]))
  }
  if (!is.null(names(contrast)) && all(c("Variable", "Group.1", "Group.2") %in% names(contrast))) {
    candidates <- c(candidates, paste(contrast[c("Variable", "Group.1", "Group.2")], collapse = "-"))
  } else if (is.null(names(contrast))) {
    candidates <- c(candidates, paste(contrast, collapse = "-"))
  }
  hit <- candidates[candidates %in% entry_names]
  if (length(hit) >= 1) {
    return(hit[1])
  }
  if (!is.na(contrast_number) && contrast_number >= 1 && contrast_number <= length(analyses)) {
    return(contrast_number)
  }
  NULL
}

#' Resolve the cleaned enrichment table and column mapping for a contrast
#' @noRd
#' @param ese An ExploratorySummarizedExperiment.
#' @param assay,gene_set_type Keys into \code{ese@gene_set_analyses}.
#' @param contrast_number Position of the selected contrast in \code{contrasts}.
#' @param contrast The selected contrast record (see \code{resolve_contrast_key}).
#'
#' @return \code{NULL} when there is no usable result for the selection,
#' otherwise a list with \code{gst} (the cleaned enrichment table) and
#' \code{col_map} (its pvalue/fdr/direction column names). The enrichment tool is
#' taken from the \code{gene_set_analyses_tool} slot when present (older objects
#' predating the slot fall back to auto-detection).
resolve_enrichment <- function(ese, assay, gene_set_type, contrast_number, contrast) {
  analyses <- ese@gene_set_analyses[[assay]][[gene_set_type]]
  contrast_key <- resolve_contrast_key(analyses, contrast_number, contrast)
  if (is.null(contrast_key)) {
    return(NULL)
  }
  gst <- analyses[[contrast_key]]
  if (is.null(gst) || nrow(gst) == 0) {
    return(NULL)
  }

  gs_tool <- if (.hasSlot(ese, "gene_set_analyses_tool")) {
    ese@gene_set_analyses_tool[[assay]][[gene_set_type]][[contrast_key]]
  } else {
    "auto"
  }
  if (is.null(gs_tool) || identical(gs_tool, "auto")) {
    gs_tool <- detect_enrichment_tool(gst)
  }

  col_map <- validate_enrichment_table(gst, gs_tool)
  list(gst = clean_enrichment_table(gst, gs_tool), col_map = col_map)
}

#' Group levels for a colouring variable, in first-appearance order
#'
#' Normalises the values of a colouring column into the set of group levels used
#' to build coloured plotly traces. NA becomes "N/A"; an absent colouring
#' variable yields no groups. Shared by the plot builders and their Shiny
#' modules so trace order and the server's trace-index-to-group mapping cannot
#' drift apart.
#'
#' @param experiment Sample annotation data frame
#' @param colorby Column name in \code{experiment} used for grouping, or NULL
#'
#' @return A character vector of group levels
#'
#' @noRd
groupLevels <- function(experiment, colorby = NULL) {
  if (is.null(colorby)) {
    return(character(0))
  }
  unique(na.replace(as.character(experiment[[colorby]]), "N/A"))
}

#' Resolve a colour palette to a vector named by group level
#'
#' Regenerates the palette when none was supplied or it is too short to cover
#' the levels, then names it by level so traces can look up their colour by
#' group.
#'
#' @param palette Palette of colours, or NULL to generate one
#' @param levels Character vector of group levels to colour
#' @param palette_name Valid R colour palette name, used when generating
#'
#' @return A character vector of colours named by \code{levels}
#'
#' @noRd
resolvePalette <- function(palette, levels, palette_name = "Set1") {
  if (is.null(palette) || any(is.na(palette[seq_along(levels)]))) {
    palette <- makeColorScale(max(length(levels), 1), palette = palette_name)
  }
  stats::setNames(palette[seq_along(levels)], levels)
}

#' Track groups toggled off via a plotly legend
#'
#' Wires up the reactive plumbing shared by plot modules whose legend entries
#' correspond to groups: clicking an entry drops that group so the plot is
#' redrawn on the remainder. Sets up a reactive value of hidden groups, resets
#' it when the grouping variable changes, and translates \code{plotly_restyle}
#' events into additions/removals.
#'
#' @param plot_source The plotly source id the plot was built with
#' @param getLevels Reactive returning the group levels, in the order legend
#'   traces are built
#' @param resetOn Reactive whose change clears the hidden set (typically the
#'   grouping variable)
#' @param trace_offset Number of leading non-group traces before the per-group
#'   traces, so a restyled trace index maps onto \code{getLevels()}
#'
#' @return A reactive value holding the character vector of hidden groups
#'
#' @noRd
legendHiddenGroups <- function(plot_source, getLevels, resetOn, trace_offset = 0L) {
  hiddenGroups <- reactiveVal(character(0))

  observeEvent(resetOn(),
    {
      hiddenGroups(character(0))
    },
    ignoreNULL = FALSE
  )

  observeEvent(event_data("plotly_restyle", source = plot_source), {
    ed <- event_data("plotly_restyle", source = plot_source)
    visible <- ed[[1]][["visible"]]
    if (is.null(visible)) {
      return()
    }
    traces <- unlist(ed[[2]])
    levels_all <- getLevels()
    current <- hiddenGroups()
    for (j in seq_along(traces)) {
      idx <- traces[j] - trace_offset
      if (idx < 0 || idx >= length(levels_all)) next
      level <- levels_all[idx + 1]
      # A restyle event can carry a single visibility value for several trace
      # indices, so fall back to the first when the vector is shorter.
      state <- if (length(visible) >= j) visible[[j]] else visible[[1]]
      current <- if (identical(state, "legendonly")) union(current, level) else setdiff(current, level)
    }
    hiddenGroups(current)
  })

  hiddenGroups
}
