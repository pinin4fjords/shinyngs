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

#' Check if a comma-separated string can be parsed to an integer vector
#'
#' @param string Input string
#'
#' @return Boolean indicating whether the check passed
#' @export

is_valid_positive_integer_vector <- function(string) {
  # as.integer() will truncate floats without throwing an error and also
  # accept negatives, so check if string contains NOT 0-9 or comma
  if (grepl("[^0-9,]", string)) {
    return(FALSE)
  }
  return(TRUE)
}
