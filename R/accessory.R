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

#' Make a hidden input field. Handy for replacing superfluous single-value 
#' selects etc
#'
#' @param string An HTML id
#' @param value The value the input should return
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
#' @param list A list to push to
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
#' @param width The maximum line length in characters (default: 20)
#'
#' @return A string with newline characters added where appropriate
#'
#' @export
#' 
#' @examples
#' > splitStringToFixedwidthLines('once upon a time there was a giant and a beanstalk and a pot of gold and some beans')
#' [1] 'once upon a time there\nwas a giant and a beanstalk\nand a pot of gold and\nsome beans'

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
#' @param x A named list of objects to unpack
#'
#' @export

unpack.list <- function(object) {
    for (.x in names(object)) {
        assign(value = object[[.x]], x = .x, envir = parent.frame())
    }
} 
