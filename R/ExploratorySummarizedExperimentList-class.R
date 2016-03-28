#' Title
#'
#' @slot ese list. 
#' @slot title character. 
#' @slot author character. 
#' @slot description character. 
#' @slot group_vars character. 
#' @slot default_groupvar character. 
#' @slot contrasts list. 
#' @slot url_roots list. 
#' @slot geneset_files list. 
#'
#' @export

setClass(
  "ExploratorySummarizedExperimentList",
  contains = "list",
  representation = representation(
    ese = "list",
    title = "character",
    author = "character",
    description = "character",
    group_vars = "character",
    default_groupvar = "character",
    contrasts = "list",
    url_roots = "list",
    geneset_files = "list"
  )
)


#' Title
#'
#' @param eses 
#' @param title 
#' @param author 
#' @param description 
#' @param group_vars 
#' @param default_groupvar 
#' @param contrasts 
#' @param url_roots 
#' @param geneset_files 
#'
#' @return output An ExploratorySummarizedExperimentList
#' @export

ExploratorySummarizedExperimentList <- function(eses, title = '', author = '', description = '', group_vars = character(), default_groupvar = character(), contrasts = list(), url_roots = list(), geneset_files = list()){ 
    
    new(
      "ExploratorySummarizedExperimentList",
      eses,
      title = title,
      author = author,
      description = description,
      group_vars = group_vars,
      default_groupvar = default_groupvar,
      contrasts = contrasts,
      url_roots = url_roots,
      geneset_files = geneset_files
    )
  } 