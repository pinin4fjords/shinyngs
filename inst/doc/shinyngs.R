## ---- eval=FALSE---------------------------------------------------------
#  library(devtools)
#  install_github('pinin4fjords/shinyngs')

## ----eval=FALSE----------------------------------------------------------
#  library(shinyngs)
#  data("zhangneurons")

## ----eval=TRUE, message=FALSE, warning=FALSE-----------------------------
library(SummarizedExperiment)
library(shinyngs)
data("zhangneurons")

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("rnaseq", zhangneurons)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=FALSE----------------------------------------------------------
#  app <- prepareApp("heatmap", zhangneurons)
#  shiny::shinyApp(app$ui, app$server)

## ----eval=TRUE-----------------------------------------------------------
# Assays is a list of matrices
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
#  myesel@gene_sets <- lapply(genesets_files, GSEABase::getGmt)

## ----eval=FALSE----------------------------------------------------------
#  library(shinyngs)
#  
#  mydata <- readRDS("data.rds")
#  
#  app <- prepareApp("rnaseq", mydata)
#  shiny::shinyApp(app$ui, app$server)

