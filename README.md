## Synopsis

This package will construct Shiny dashboards for a variety of next-generation sequencing and other applications. But I'm currently porting a large script for RNA-seq type downstream analyses, so for now all it does is produce a heatmap builder or a 3D PCA plot as toy examples. 

The app uses Shiny modules (http://shiny.rstudio.com/articles/modules.html), the learning of which was one of the motivations of producing this package. The provision of modules as reusable components should enable the simple production of a variety of applications in future.  

For the heatmap and future applications, data must be in the SummarisedExperiment structure of the GenomicRanges package. This allows multiple expression matrices to be stored, alongside experimental variables and annotation.

## Code Example

### A basic heatmap builder

To produce a simple heat map using some example data you'd do the following:

```{r, eval=FALSE}
require(airway)
library(shinyngs)
library(shiny)
library(GenomicRanges)

# Get some example data in the form of a StructuredExperiment object

data(airway, package="airway")
se <- airway

# Use Biomart to retrieve some annotation, and add it to the object

library(biomaRt)
attributes <- c(
  "ensembl_gene_id", # The sort of ID your results are keyed by
  "entrezgene", # Will be used mostly for gene set based stuff
  "external_gene_name" # Used to annotate gene names on the plot
)
mart <- useMart(biomart = "ENSEMBL_MART_ENSEMBL", dataset = 'hsapiens_gene_ensembl', host='www.ensembl.org')
annotation <- getBM(attributes = attributes, mart = mart)
annotation <- annotation[order(annotation$entrezgene),]

mcols(se) <- annotation[match(rownames(se), annotation$ensembl_gene_id),]

# Specify some display parameters

params <- list(
  transcriptfield = "ensembl_gene_id", 
  entrezgenefield = "entrezgene",
  genefield = "external_gene_name", 
  group_vars = c('cell', 'dex', 'albut'), 
  default_groupvar = 'albut'
)

# Prepare the UI and server parts of the Shiny app

app <- prepareApp("heatmap", se, params)

# Run the Shiny app

shinyApp(app$ui, app$server)
```

### An interactive 3D PCA plot

Using the same input data a PCA plot can be produced as follows:

```{r, eval=FALSE}

# Prepare the UI and server parts of the Shiny app

app <- prepareApp("pca", se, params)

# Run the Shiny app

shinyApp(app$ui, app$server)
```

#### Adding a gene set filter

It's quite handy to see heat maps based on known gene sets. Assuming you have a bunch of .gmt format gene set files from MSigDB keyed by Entrez ID, you can add a gene set filter to the heatmap controls like:

```{r, eval=FALSE}
gene_sets = list(
  'KEGG' =  "/path/to/MSigDB/c2.cp.kegg.v5.0.entrez.gmt",
  'MSigDB canonical pathway' = "/path/to/MSigDB/c2.cp.v5.0.entrez.gmt",
  'GO biological process' = "/path/to/MSigDB/c5.bp.v5.0.entrez.gmt",
  'GO cellular component' = "/path/to/MSigDB/c5.cc.v5.0.entrez.gmt",
  'GO molecular function' = "/path/to/MSigDB/c5.mf.v5.0.entrez.gmt",
  'MSigDB hallmark'= "/path/to/MSigDB/h.all.v5.0.entrez.gmt"
)

params <- list(
  transcriptfield = "ensembl_gene_id", 
  entrezgenefield = "entrezgene",
  genefield = "external_gene_name", 
  group_vars = c('cell', 'dex', 'albut'), 
  default_groupvar = 'albut',
  geneset_files = gene_sets
)

app <- prepareApp("heatmap", se, params)
shinyApp(app$ui, app$server)
```

This will read in the gene sets (which will take a while first time), and use them to add a filter which will allow users to make heat maps based on known sets of genes. Of course you could make your own .gmt files with custom gene sets.

### To make your own Summarized experiment objects

Assuming you have: 

* an expression matrix
* a data frame of experimental variables with rows matching the columns of the expression matrix 
* a data frame containing annotation, one row for each of the expression matrix

... you can make a StructuredExperiment like:

```{r, eval=FALSE}
se <- SummarizedExperiment(
  assays=SimpleList(expression=expression),
  colData=DataFrame(experiment)
)
mcols(se) <- annotation
```

### Running on a shiny server

Just use the commands sets above with `shinyApp()` in a file called app.R in a directory of its own on your Shiny server.

## Motivation

Shiny apps are great for NGS and bioinformatics applications in general. But apps can get monstrous when their complexity increases, and it's not always easy to re-use components. This is an effort to create modularised components (e.g. a heatmap with controls), re-used to produce multiple shiny apps.

For example this package currently contains five Shiny modules: 

* `heatmap` - provides controls and a display for making heat maps based on user criteria.
* `pca` - provides controls and display for an interactive PCA plot.
* `selectmatrix` - provides controls and output for subsetting the profided assay data prior to plotting. Called by the `heatmap` and `pca` modules.
* `sampleselect` - provides a UI element for selecting the columns of the matrix based on sample name or group. Called by the `selectmatrix` module.
* `geneselect` - provides a UI element for selecing the rows of a matrix based on criteria such as variance. Called by the `selectmatrix` module.
* `genesets` - provides UI element for selecting gene sets. Called by the `geneselect` module when a user chooses to filter by gene set. 

So `heatmap` and `pca` both use `selectmatrix` to provide the UI controls to subselect the supplied matrices as well as the code which reads the output of those controls to actually derive the subsetted matrix. Shiny modules make this recycling of code much, much simpler than it would be otherwise. 

I intend to provide modules for a number of things I currently use (boxplots, PCA, scatterplots), which can then be simply plugged into many different applications.

## Installation

```{r, eval=FALSE}
library(devtools)
install_github('pinin4fjords/shinyngs')
```

## Contributors

This is an experimental embryonic project, but I can be reached on @pinin4fjords with any queries. Other contributors welcome.

## License

MIT