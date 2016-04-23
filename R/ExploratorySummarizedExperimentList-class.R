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
#' @slot read_distribution
#'
#' @export

setClass("ExploratorySummarizedExperimentList", contains = "list", representation = representation(title = "character", author = "character", description = "character", 
    group_vars = "character", default_groupvar = "character", contrasts = "list", url_roots = "list", gene_sets = "list", read_distribution = "matrix"))

# Subset operator for integer type like 1:2

setMethod("[", c("ExploratorySummarizedExperimentList", "ANY", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets, read_distribution = x@read_distribution)
})

# Subset operator for numeric type like 1

setMethod("[", c("ExploratorySummarizedExperimentList", "numeric", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets, read_distribution = x@read_distribution)
})

# And one for logicals. Do I really need to repeat these? Must look into it....

setMethod("[", c("ExploratorySummarizedExperimentList", "logical", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
    initialize(x, x@.Data[i], title = x@title, author = x@author, description = x@description, group_vars = x@group_vars, default_groupvar = x@default_groupvar, contrasts = x@contrasts, 
        url_roots = x@url_roots, gene_sets = x@gene_sets, read_distribution = x@read_distribution)
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
#' @param read_distribution A matrix of values showing groups (exons, introns
#' etc) in rows and samples in columns, illustrating how reads fell in the 
#' genome. RSeqQC can produce such statistics.
#'
#' @return output An ExploratorySummarizedExperimentList
#' @export

ExploratorySummarizedExperimentList <- function(eses, title = "", author = "", description = "", group_vars = character(), default_groupvar = character(), contrasts = list(), 
    url_roots = list(), gene_sets = list(), read_distribution = matrix()) {
    
    # Key the gene sets by gene name for easier access
    
    if (length(gene_sets) > 0) {
        
        gene_sets_by_name <- list()
        
        for (ese in eses) {
            annotation <- data.frame(SummarizedExperiment::mcols(ese))
            entrezgenefield <- ese@entrezgenefield
            labelfield <- ese@labelfield
            
            if (labelfield %in% names(gene_sets_by_name)) {
                next
            }
            
            gene_sets_by_name[[labelfield]] <- lapply(gene_sets, function(gene_set_collection) {
                
                # gene_set_collection doesn't behave exactly like a list (it's a GSEABase object), so we have to make sure the result gets named properly
                
                gsc <- lapply(gene_set_collection, function(gene_set) {
                  set_gene_ids <- as.integer(GSEABase::geneIds(gene_set))
                  gs <- structure(annotation[match(set_gene_ids, annotation[[entrezgenefield]]), labelfield], names = set_gene_ids)
                  gs[!is.na(gs)]
                })
                names(gsc) <- names(gene_set_collection)
                gsc
            })
        }
        
        gene_sets <- gene_sets_by_name
    }
    
    new("ExploratorySummarizedExperimentList", eses, title = title, author = author, description = description, group_vars = group_vars, default_groupvar = default_groupvar, 
        contrasts = contrasts, url_roots = url_roots, gene_sets = gene_sets, read_distribution = read_distribution)
} 
