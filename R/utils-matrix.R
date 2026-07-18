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
