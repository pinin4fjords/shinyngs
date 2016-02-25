library(shiny)
library(shinyjs)

source("apps.R")

configfile <- "config.txt"
config <- data.frame(t(read.delim(configfile, sep = "\t", header = FALSE, row.names = 1)),
    stringsAsFactors = FALSE)

experiment <- read.delim(config$experiment, stringsAsFactors = FALSE)
expression <- read.delim(config$expression)[, rownames(experiment)]
annotation <- read.delim(config$annotation, stringsAsFactors = FALSE)

gene_set_type_names <- do.call(rbind, unlist(lapply(strsplit(config$gene_set_types,
    ","), function(x) strsplit(x, "=")), recursive = F))
gene_set_type_names <- structure(gene_set_type_names[, 2], names = gene_set_type_names[,
    1])

params <- list(experiment = experiment, expression = expression, annotation = annotation,
    transcriptfield = config$transcriptfield, entrezgenefield = config$entrezgenefield,
    genefield = config$genefield, group_vars = unlist(strsplit(config$groupvars,
        ",")), default_groupvar = config$default_groupvar, gene_set_dir = config$gene_set_dir,
    gene_set_type_names = gene_set_type_names)

app <- prepareApp("heatmap", params)
shinyApp(app$ui, app$server)
