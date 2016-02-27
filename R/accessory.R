#' Make machine variable names pretty for display
#' 
#' Just split out words using '_' and capitalise first letter
#'
#' @param vn A string to be prettified
#'
#' @return output Prettified string
#'
#' @export
#' 
#' @examples
#' vn <- 'ugly_name_of_thing'
#' > prettyifyVariablename(vn)
#' [1] 'Ugly name of thing'

prettifyVariablename <- function(vn) {
    gsub("_", " ", ucfirst(tolower(vn)))
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
#' > ucfirst('Example')
#' [1] 'Example'

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
#' > nlines('foo\nbar')
#' [1] 2

nlines <- function(string) {
    length(unlist(strsplit(string, "\n")))
} 
