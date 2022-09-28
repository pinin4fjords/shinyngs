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
#' vn <- 'ugly_name_of_thing'
#' prettyifyVariablename(vn)

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
#' ucfirst('Example')

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
#' nlines('foo\nbar')

nlines <- function(string) {
    length(unlist(strsplit(string, "\n")))
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
#' hiddenInput('myid', 'iamavalue')

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
#' mylist <- pushToList(mylist, 'new element')

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
        open = names(fieldset_list)
    }
    
    if (requireNamespace("shinyBS", quietly = TRUE) && use_shinybs) {
        
        collapse_panels <- lapply(names(fieldset_list), function(listname) {
            shinyBS::bsCollapsePanel(prettifyVariablename(listname), value = listname, fieldset_list[[listname]])
        })
        
        collapse_panels$id = id
        collapse_panels$multiple = TRUE
        collapse_panels$open = open
        
        do.call(shinyBS::bsCollapse, collapse_panels)
        
    } else {
        lapply(names(fieldset_list), function(listname) {
            div(id = id, class = "shinyngsFieldset", h4(prettifyVariablename(listname)), fieldset_list[[listname]])
        })
    }
}

#' Reshape data to the way \code{ggplot2} likes it 
#'
#' @param matrix A matrix of values, e.g. expression data
#' @param experiment A data frame with rows matching the columns of 
#' \code{matrix}
#' @param colorby An optional string specifying a column from \code{experiment}
#' that will be used to set a color column in the reshaped output.
#'
#' @return A reshaped data frame
#'
#' @export
#' 
#' @examples
#' plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)

ggplotify <- function(matrix, experiment, colorby = NULL) {
    
    plotdata <- reshape2::melt(matrix)
    plotdata <- plotdata[which(plotdata$value > 0), ]
    if (max(plotdata$value) > 20) {
        plotdata$value <- log2(plotdata$value)
    }
    
    colnames(plotdata) <- c("gene", "name", "log2_count")
    
    if (!is.null(colorby)) {
        plotdata$colorby <- factor(experiment[[colorby]][match(plotdata$name, rownames(experiment))], levels = unique(experiment[[colorby]]))
    }
    plotdata
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
#' splitStringToFixedwidthLines('once upon a time there was a giant and a beanstalk and a pot of gold and some beans')

splitStringToFixedwidthLines <- function(string, linewidth = 20) {
    words <- unlist(strsplit(string, " "))
    
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
#' data(airway, package = 'airway')
#' 
#' # However, not all variables are useful for grouping data. Some have a 
#' # different value for every sample, one has the same value for all of them.
#' 
#' colData(airway)
#' 
#' # So just pick the variables that ARE useful
#' 
#' chooseGroupingVariables(data.frame(colData(airway)))

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
#' inlineField(numericInput('foo', label = NULL, min = 0, max = 100, value = 50), 'FOO') 

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
    tags$div(fluidRow(column(4, HTML(paste0("<b>", label, ":</b>&nbsp;"))), column(3, selectInput(cardinal_id, label = NULL, choices = c("<=", ">=", ">= or <= -", 
        "<= and >= -"), selected = cardinality), selectize = FALSE), column(5, numericInput(id, label = NULL, value = value, min = min, max = max, step = step))), 
        class = "shinyngs-cardinalfield")
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
        values <= limit
    } else if (cardinality == ">=") {
        values >= limit
    } else if (cardinality == ">= or <= -") {
        abs(values) >= limit
    } else if (cardinality == "<= and >= -") {
        values <= limit & values >= -limit
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
#' eselist <- eselistFromYAML('my.yaml')

eselistFromYAML <- function(configfile){
  
  config <- yaml::yaml.load_file(configfile)
  
  # 'Experiments' are sets of results from a common set of samples
  
  experiments <- config$experiments
  
  # Establish ordering. Ordering of YAML file shouldn't be relied upon
  
  if ('experiment_order' %in% names(config)){
    experiment_order <- config$experiment_order
  }else{
    experiment_order <- names(experiments)
  }

  experiments <- experiments[experiment_order]
  
  # Make the basic objects
  
  print("Constructing ExploratorySummarizedExperiments")
  
  expsumexps <- lapply(structure(names(experiments), names = names(experiments)), function(expname){
    
    exp <- experiments[[expname]]
    
    # Read the expression data
    
    assays <- rev(lapply(exp$expression_matrices, function(mat){
      print(paste("Reading", mat$file))
      as.matrix(read.csv(mat$file, row.names=1, check.names = FALSE))
    }))
    
    # Apply ordering if provided
    
    if ('assay_order' %in% names(exp)){
      assay_order <- exp$assay_order
    }else{
      assay_order <- names(assays)
    }
    assays <- assays[assay_order]
    
    # Add contrast_stats where available. 
    
    contrast_stats <- list()
    if (expname %in% names(config$contrasts$stats )){
      contrast_stats <- lapply(config$contrasts$stats[[expname]], function(assaytests){
        lapply(assaytests, function(at){
          print(paste("Reading test stats file", at))
          read.csv(at, row.names = 1, header = FALSE)
        })
      })
    }
    
    # Make the object
    
    print(paste('coldata:', exp$coldata$file))
    print(paste('annotation:', exp$annotation$file))
    
    # Basic list to pass to object creation
    
    ese_list <- list(
      assays = assays,
      colData = read.csv(exp$coldata$file, row.names = 1),
      annotation = read.csv(exp$annotation$file, row.names = 1, stringsAsFactors = FALSE),
      idfield = exp$annotation$id,
      entrezgenefield = exp$annotation$entrez,
      labelfield = exp$annotation$label,
      assay_measures = lapply(exp$expression_matrices, function(mat){mat$measure}),
      contrast_stats = contrast_stats[assay_order]
    )
    
    if ('read_reports' %in% names(exp)){
      ese_list$read_reports = lapply(exp$read_reports, function(rrfile) read.csv(rrfile, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
    }
    
    if ('gene_set_analyses' %in% names(exp)){
      ese_list$gene_set_analyses <- lapply(exp$gene_set_analyses, function(assay){
        lapply(assay, function(gene_set_type){
          lapply(gene_set_type, function(contrast){
              read.csv(contrast, check.names = FALSE, stringsAsFactors = FALSE, row.names = 1)
          })
        })
      })
    }
    
    do.call(ExploratorySummarizedExperiment, ese_list)
  })
  
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
  
  if ('static_pdf' %in% names(config)){
      eselist_args$static_pdf <- config$static_pdf
  }
  
  # If 'report' is specified with assume a mardown document to be parsed. Otherwise just text
  
  if ('report' %in% names(config)){
    eselist_args$description = as.character(includeMarkdown(config$report))
  }else if ('description' %in% names(config)){
    eselist_args$description <- config$description
  }
  
  if ('url_roots' %in% names(config)){
    eselist_args$url_roots <- config$url_roots
  }
  
  if ('gene_set_id_type' %in% names(config) && 'gene_sets' %in% names(config)){
    eselist_args$gene_set_id_type <- config$gene_set_id_type
    eselist_args$gene_sets <- lapply(config$gene_sets, GSEABase::getGmt)
  }
  
  eselist <- do.call(ExploratorySummarizedExperimentList, eselist_args)
  
  eselist
  
}
