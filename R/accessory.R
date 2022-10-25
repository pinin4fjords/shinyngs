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
#' fields in either a \code{bsCollapse} from \code{shinyBS} (if installed) or
#' a simple div element with a title and class 'shinyngsFieldset' (which can
#' then be used for styling)
#'
#' @param id ID field to apply to the overall container
#' @param fieldset_list A named list, each element containing one or more
#' fields.
#' @param open Only applicable for output with shinyBS, controls which panels
#' are open by default. In most cases all should be left open (the default),
#' since shiny doesn't receive the inputs of fields in collapsed elements.
#' @param use_shinybs Use collapsible panels from shinyBS if installed
#'
#' @return list

fieldSets <- function(id, fieldset_list, open = NULL, use_shinybs = TRUE) {
  if (is.null(open)) {
    open <- names(fieldset_list)
  }

  if (requireNamespace("shinyBS", quietly = TRUE) && use_shinybs) {
    collapse_panels <- lapply(names(fieldset_list), function(listname) {
      shinyBS::bsCollapsePanel(prettifyVariablename(listname), value = listname, fieldset_list[[listname]])
    })

    collapse_panels$id <- id
    collapse_panels$multiple <- TRUE
    collapse_panels$open <- open

    do.call(shinyBS::bsCollapse, collapse_panels)
  } else {
    lapply(names(fieldset_list), function(listname) {
      div(id = id, class = "shinyngsFieldset", h4(prettifyVariablename(listname)), fieldset_list[[listname]])
    })
  }
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
#'
#' @return A reshaped data frame
#'
#' @export
#'
#' @examples
#' plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)
#'
ggplotify <- function(plotmatrices, experiment, colorby = NULL, value_type = "expression", annotate_samples = FALSE) {
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

  ensureLog <- function(vals, condition, rmzeros = FALSE) {
    if (rmzeros) {
      vals <- vals[vals > 0]
    }

    if (condition) {
      log2(vals)
    } else {
      vals
    }
  }

  allplotdata <- do.call(rbind, lapply(names(plotmatrices), function(pm) {
    if (value_type == "density") {
      plotdata <- do.call(rbind, lapply(colnames(plotmatrices[[pm]]), function(s) {
        dens <- density(ensureLog(plotmatrices[[pm]][, s], condition = max(plotmatrices[[pm]]) > 20, rmzeros = TRUE))
        data.frame(name = s, value = dens$x, density = dens$y)
      }))
    } else {
      plotdata <- reshape2::melt(as.matrix(plotmatrices[[pm]][, rownames(experiment)]))
      plotdata <- plotdata[which(plotdata$value > 0), ]
      colnames(plotdata) <- c("gene", "name", "value")
      plotdata$value <- ensureLog(plotdata$value, max(plotdata$value) > 20)
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
#' @param linewidth The maximum line length in characters (default: 20)
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
  out[, unlist(lapply(1:ncol(mat1), function(n) c(n, n + ncol(mat1))))]
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

#' Build an ExploratorySummarisedExperimentList from a description provided in a list
#'
#' @param config Hierachical named list with input components. See \code{eselistFromYAML} for detail.
#'
#' @return out An ExploratorySummarizedExperimentList object suitable for passing to \code{\link{prepareApp}}
#' @export

eselistfromConfig <-
  function(config) {
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
        sep = exp$coldata$sep
      )
      annotation <-
        read_metadata(
          exp$annotation$file,
          id_col = exp$annotation$id,
          sep = exp$annotation$sep,
          stringsAsFactors = FALSE
        )

      # Read the expression data

      assays <- rev(lapply(exp$expression_matrices, function(mat) {
        print(paste("Reading", mat$file))
        read_matrix(
          mat$file,
          sample_metadata = colData,
          feature_metadata = annotation,
          sep = mat$sep,
          row.names = 1
        )
      }))

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
            compile_contrast_data(
              differential_stats_files = assaytests$files,
              sep = assaytests$sep,
              feature_id_column = assaytests$feature_id_column,
              fc_column = assaytests$fc_column,
              pval_column = assaytests$pval_column, qval_column = assaytests$qval_column,
              unlog_foldchanges = assaytests$unlog_foldchanges
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
        ese_list$read_reports <- lapply(exp$read_reports, function(rrfile) read.csv(rrfile, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
      }

      if ("gene_set_analyses" %in% names(exp)) {
        ese_list$gene_set_analyses <- lapply(exp$gene_set_analyses, function(assay) {
          lapply(assay, function(gene_set_type) {
            lapply(gene_set_type, function(contrast) {
              read.csv(contrast, check.names = FALSE, stringsAsFactors = FALSE, row.names = 1)
            })
          })
        })
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
      if (length(ese@contrast_stats) != length(config$contrasts$comparisons)) {
        stop(paste0("Number of supplied contrasts (", length(config$contrasts$comparisons), ") not equal to number of contrast stats files (", length(ese@contrast_stats), ")"))
      }
    }

    print("Creating ExploratorySummarizedExperimentList")

    eselist_args <- list(
      expsumexps,
      title = config$title,
      author = config$author,
      group_vars = config$group_vars,
      default_groupvar = config$default_groupvar,
      contrasts = lapply(config$contrasts$comparisons, function(x) as.character(x[1:3]))
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

  as.matrix(matrix_data[, rownames(sample_metadata)])
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
    stop(
      paste0(
        "Not all ",
        test_list_name,
        " (",
        paste(test_list, collapse = ","),
        ") are available in the ",
        reference_list_name,
        " (",
        paste(reference_list, collapse = ","),
        ")"
      )
    )
  }
  TRUE
}


#' Read and validate a contrasts file against sample metadata
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
           convert_to_list = FALSE) {
    # Read the contrasts

    contrasts <- read_metadata(filename)
    contrast_cols <- c(variable_column, reference_column, target_column, blocking_column)

    # Check contrast headers are as expected

    if (!all(contrast_cols %in% colnames(contrasts))) {
      stop(paste("Contrasts file must contain all of", paste(contrast_cols, collapse = ", ")))
    }

    # Check contrast content is appropriate to sample sheet

    ## 'variable' values should be sample sheet columns

    success <- checkListIsSubset(contrasts$variable, colnames(samples), "contrast variables", "sample metadata")

    blocking <- unlist(lapply(contrasts$blocking, function(x) simpleSplit(x, ";")))
    blocking <- blocking[!is.na(blocking)]
    if (length(blocking > 0)) {
      success <- checkListIsSubset(blocking, colnames(samples), "blocking variables", "sample metadata")
    }

    ## 'reference', 'target', and 'blocking' should be values of their variable
    ## columns

    for (i in 1:nrow(contrasts)) {
      var <- contrasts[i, variable_column]
      for (col in c(reference_column, target_column)) {
        val <- contrasts[i, col]
        if (is.na(val) || val == "") {
          stop(paste("Missing value for", col, "in sample sheet"))
        } else {
          success <- checkListIsSubset(val, samples[[var]], "contrast levels", "sample metadata variable")
        }
      }
    }

    if (convert_to_list) {
      contrasts <- apply(contrasts, 1, function(x) {
        list(
          "Variable" = x["variable"],
          "Group.1" = x["reference"],
          "Group.2" = x["target"]
        )
      })
    }

    contrasts
  }

#' Read tables of differential statistics
#'
#' @param filename File name of file with table of differential statistics
#' @param feature_id_column Column of stats file with feature identifiers
#' @param pval_column Column of stats file with p values
#' @param qval_column Column of stats file with adjust p values/ q values
#' @param fc_column Column of stats with fold changes
#' @param unlog_foldchanges Reverse a log on fold changes? Set to TRUE if values
#'   are logged.
#'
#' @return output Validated selected columns of differential stats files as a
#'   data frame
#' @export

read_differential <- function(filename,
                              feature_id_column = NULL,
                              pval_column = NULL,
                              qval_column = NULL,
                              fc_column = NULL,
                              unlog_foldchanges = FALSE) {
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

  if (unlog_foldchanges) {
    st[[fc_column]] <- 2^st[[fc_column]]
  }
  st
}

#' Compile contrast stats for inclusion in shinyngs
#'
#' @param differential_stats_files Tabular files with differential stats
#' @param sep Separator in stats files
#' @param pval_column P value column in stats files
#' @param qval_column Q value column in stats files
#' @param fc_column Fold change column in stats files
#' @param feature_id_column Feature identifier column in stats files
#' @param unlog_foldchanges Should fold change values be unlogged?
#'
#' @return output A named list of data frames by statistic, number of columns equal to input file number

compile_contrast_data <-
  function(differential_stats_files,
           sep = "\t",
           feature_id_column = NULL,
           pval_column = NULL,
           qval_column = NULL,
           fc_column = NULL,
           unlog_foldchanges = FALSE) {
    # Read stats and make sure they're numeric

    contrast_stats <- lapply(differential_stats_files, function(dsf) {
      read_differential(
        filename = dsf,
        feature_id_column = feature_id_column,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column,
        unlog_foldchanges = unlog_foldchanges
      )
    })

    contrast_stats_rearranged <- list()

    add_to_stats <- function(source) {
      df <- do.call(cbind, lapply(contrast_stats, function(x) {
        x[, source, drop = FALSE]
      }))
      names(df) <- paste0("V", 1:ncol(df))
      rownames(df) <- contrast_stats[[1]][[feature_id_column]]
      df
    }

    if (!is.null(fc_column)) {
      contrast_stats_rearranged[["fold_changes"]] <- add_to_stats(source = fc_column)
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
#' @param unlog_foldchanges Boolean- should fold changes in stats files be
#'   unlogged?
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
                            unlog_foldchanges = FALSE) {
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
        feature_id_column = feature_id_column,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column,
        unlog_foldchanges = unlog_foldchanges
      )
    })
  }

  validated_parts
}
