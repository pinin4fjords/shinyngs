## Synopsis

Shiny layouts for next-generation sequencing applications. Will provide Shiny applications for various array and NGS applications. Currently very RNA-seq centric, in fact since I'm currently migrating from a monster RNA-seq Shiny script for demonstration purposes this just makes heat maps. But I have plans....

## Code Example

To produce a simple heat map using some example data you'd do the following:

```{r, eval=FALSE}
library(shinyngs)
library(shiny)

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

## Motivation

Shiny apps are great for NGS and bioinformatics applications in general. But apps can get monstrous when their complexity increases, and it's not always easy to re-use components. This is an effort to create modularised components (e.g. a heatmap with controls), re-used to produce multiple shiny apps.

## Installation

```{r, eval=FALSE}
install_github('pinin4fjords/shinyngs')
```

## Contributors

This is an experimental embryonic project, but I can be reached on @pinin4fjords with any queries.

## License

MIT