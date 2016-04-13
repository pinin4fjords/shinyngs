#' The ExploratorySummaizedExperimentList class
#'
#' @slot title character. 
#' @slot author character. 
#' @slot description character. 
#' @slot group_vars character. 
#' @slot default_groupvar character. 
#' @slot contrasts list. 
#' @slot url_roots list. 
#' @slot gene_sets list. 
#'
#' @export

setClass("ExploratorySummarizedExperimentList", contains = "list", representation = representation(title = "character", author = "character", description = "character", group_vars = "character", 
    default_groupvar = "character", contrasts = "list", url_roots = "list", gene_sets = "list"))

# Subset operator for integer type like 1:2

setMethod("[", c("ExploratorySummarizedExperimentList", "ANY", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets)
})

# Subset operator for numeric type like 1

setMethod("[", c("ExploratorySummarizedExperimentList", "numeric", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets)
})

# And one for logicals. Do I really need to repeat these? Must look into it....

setMethod("[", c("ExploratorySummarizedExperimentList", "logical", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets)
})

#' ExploratorySummarizedExperimentLists, containers for
#' ExploratorySummarizedExperiments
#' 
#' ExploratorySummarizedExperiment lists are intented to contain one or more 
#' ExploratorysummarizedExperiments with the same sets of samples/columns 
#' but different feature sets. The motivating use case was the desire to 
#' examine expression at both transcript and gene levels in RNA-seq experiments
#' explorted via \code{Shinyngs}
#' 
#' As a the containing object for experiments, this class is intented to 
#' contain various variables relevant across a whole study, which will be 
#' displayed in an exploratory interface generated in \code{shinyngs}. This 
#' includes the study title, author etc as well as definitions of the contrasts 
#' used in differential analysis and the gene sets relevant to all experiments
#'
#' @param eses List of ExploratorySummarizedExperiments
#' @param title Study title 
#' @param author Study authors
#' @param description Study summary to displayed on front page
#' @param group_vars Variables by which a user will be allowed to group the 
#' samples of individual experiments, must correspond to their \code{colData}
#' @param default_groupvar Default \code{group_var}
#' @param contrasts List of length-3 vectors containing 1) the
#' \code{group_var}, 2) the \code{group_var} value corresponding to the
#' 'control' side and 3) the value corresponding to the 'treatment' side
#' @param url_roots A list of URL roots, with list names corresponding to 
#' metadata column names of the experiments. Exploratory tools displayed via
#' \code{shinyngs} can use these roots to construct URLs to 'link out'.
#' @param gene_sets A named list of GeneSetCollections as might be produced by
#' reading .gmt format gene sets (for example from MSigDB) using 
#' GSEABase::getGmt(). \code{shinyngs} modules currently assume genes
#' represented by Entrez ID (not symbol). 
#'
#' @return output An ExploratorySummarizedExperimentList
#' @export

ExploratorySummarizedExperimentList <- function(eses, title = "", author = "", description = "", group_vars = character(), default_groupvar = character(), contrasts = list(), url_roots = list(), 
    gene_sets = list()) {
    
    new("ExploratorySummarizedExperimentList", eses, title = title, author = author, description = description, group_vars = group_vars, default_groupvar = default_groupvar, contrasts = contrasts, 
        url_roots = url_roots, gene_sets = gene_sets)
} 
