## ---- eval=FALSE---------------------------------------------------------
#  library(devtools)
#  install_github('pinin4fjords/shinyngs')

## ----eval = FALSE--------------------------------------------------------
#  library(shinyngs)
#  
#  data(airway, package = 'airway')
#  ese <- as(airway, 'ExploratorySummarizedExperiment')
#  eselist <- ExploratorySummarizedExperimentList(ese)

## ----eval = FALSE--------------------------------------------------------
#  app <- prepareApp('heatmap', eselist)
#  shiny::shinyApp(ui = app$ui, server = app$server)

## ----eval = FALSE--------------------------------------------------------
#  app <- prepareApp('rnaseq', eselist)
#  shiny::shinyApp(ui = app$ui, server = app$server)

## ----eval = FALSE--------------------------------------------------------
#  data(airway, package = 'airway')
#  expinfo <- metadata(airway)[[1]]
#  
#  eselist <- ExploratorySummarizedExperimentList(
#    ese,
#    title = expinfo@title,
#    author = expinfo@name,
#    description = abstract(expinfo)
#  )
#  app <- prepareApp('rnaseq', eselist)
#  shiny::shinyApp(ui = app$ui, server = app$server)

## ----eval = FALSE--------------------------------------------------------
#  # Use Biomart to retrieve some annotation, and add it to the object
#  
#  library(biomaRt)
#  attributes <- c(
#    'ensembl_gene_id', # The sort of ID your results are keyed by
#    'entrezgene', # Will be used mostly for gene set based stuff
#    'external_gene_name' # Used to annotate gene names on the plot
#  )
#  
#  mart <- useMart(biomart = 'ENSEMBL_MART_ENSEMBL', dataset = 'hsapiens_gene_ensembl', host='www.ensembl.org')
#  annotation <- getBM(attributes = attributes, mart = mart)
#  annotation <- annotation[order(annotation$entrezgene),]
#  
#  mcols(ese) <- annotation[match(rownames(ese), annotation$ensembl_gene_id),]
#  
#  # Tell shinyngs what the ids are, and what field to use as a label
#  
#  ese@idfield <- 'ensembl_gene_id'
#  ese@labelfield <- 'external_gene_name'
#  
#  # Re-build the app
#  
#  eselist <- ExploratorySummarizedExperimentList(
#    ese,
#    title = expinfo$Title,
#    author = expinfo$Author,
#    description = expinfo$Description
#  )
#  app <- prepareApp('rnaseq', eselist)
#  shiny::shinyApp(ui = app$ui, server = app$server)

## ----eval=FALSE----------------------------------------------------------
#  library(shinyngs)
#  data("zhangneurons")

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", zhangneurons)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("heatmap", zhangneurons)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=TRUE-----------------------------------------------------------
# Assays is a list of matrices
data(zhangneurons, envir = environment())
myassays <- as.list(SummarizedExperiment::assays(zhangneurons[[1]]))
head(myassays[[1]])

## ----eval=TRUE-----------------------------------------------------------
mycoldata <- data.frame(SummarizedExperiment::colData(zhangneurons[[1]]))
head(mycoldata)

## ----eval=TRUE-----------------------------------------------------------
myannotation <- SummarizedExperiment::mcols(zhangneurons[[1]])
head(myannotation)

## ----eval=TRUE-----------------------------------------------------------
myese <- ExploratorySummarizedExperiment(
    assays = SimpleList(
      myassays
    ),
    colData = DataFrame(mycoldata),
    annotation <- myannotation,
    idfield = 'ensembl_gene_id',
    entrezgenefield = "entrezgene",
    labelfield = "external_gene_name"
  )
print(myese)

## ----eval=TRUE-----------------------------------------------------------
myesel <- ExploratorySummarizedExperimentList(
  eses = list(expression = myese),
  title = "My title",
  author = "My Authors",
  description = 'Look what I gone done'
)

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", myesel)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=TRUE-----------------------------------------------------------
myesel@group_vars <- c('cell_type', 'source_name')

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", myesel)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=TRUE-----------------------------------------------------------
zhangneurons@contrasts
myesel@contrasts <- zhangneurons@contrasts

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", myesel)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=TRUE-----------------------------------------------------------
head(zhangneurons[[1]]@tests[[1]]$pvals, n = 10)

## ----eval=TRUE-----------------------------------------------------------
myesel[[1]]@tests <- zhangneurons[[1]]@tests

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", myesel)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=FALSE----------------------------------------------------------
#  genesets_files = list(
#    'KEGG' =  "/path/to/MSigDB/c2.cp.kegg.v5.0.entrez.gmt",
#    'MSigDB canonical pathway' = "/path/to/MSigDB/c2.cp.v5.0.entrez.gmt",
#    'GO biological process' = "/path/to/MSigDB/c5.bp.v5.0.entrez.gmt",
#    'GO cellular component' = "/path/to/MSigDB/c5.cc.v5.0.entrez.gmt",
#    'GO molecular function' = "/path/to/MSigDB/c5.mf.v5.0.entrez.gmt",
#    'MSigDB hallmark'= "/path/to/MSigDB/h.all.v5.0.entrez.gmt"
#  )
#  
#  gene_sets <- lapply(genesets_files, GSEABase::getGmt)

## ----eval = FALSE--------------------------------------------------------
#  myesel <- ExploratorySummarizedExperimentList(
#    eses = list(expression = myese),
#    title = "My title",
#    author = "My Authors",
#    description = 'Look what I gone done',
#    gene_sets = gene_sets
#  )

## ----eval = TRUE---------------------------------------------------------
names(zhangneurons@gene_sets)

## ----eval = TRUE---------------------------------------------------------
names(zhangneurons@gene_sets$external_gene_name$KEGG)[1:10]

## ----eval = TRUE---------------------------------------------------------
zhangneurons@gene_sets$external_gene_name$KEGG$KEGG_GLYCOLYSIS_GLUCONEOGENESIS

## ----eval = TRUE---------------------------------------------------------
names(zhangneurons$gene@gene_set_analyses)
names(zhangneurons$gene@gene_set_analyses$`normalised-filtered`)
names(zhangneurons$gene@gene_set_analyses$`normalised-filtered`$KEGG)
head(zhangneurons$gene@gene_set_analyses$`normalised-filtered`$KEGG$`myelinating_oligodendrocytes:0-1`)

## ----eval = FALSE--------------------------------------------------------
#  app <- prepareApp('dendro', eselist)
#  shiny::shinyApp(ui = app$ui, server = app$server)

## ----eval = FALSE--------------------------------------------------------
#  ?shinyngs

## ----eval=FALSE----------------------------------------------------------
#  library(shinyngs)
#  
#  mydata <- readRDS("data.rds")
#  
#  app <- prepareApp("rnaseq", mydata)
#  shiny::shinyApp(app$ui, app$server)

