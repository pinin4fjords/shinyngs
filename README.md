## Synopsis

Shiny layouts for next-generation sequencing applications. Provides Shiny applications for various array and NGS applications. Currently very RNA-seq centric, with plans for expansion.

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

TODO: Provide code examples and explanations of how to get the project.

## API Reference

TODO: Depending on the size of the project, if it is small and simple enough the reference docs can be added to the README. For medium size to larger projects it is important to at least provide a link to where the API reference docs live.

## Tests

TODO: Describe and show how to run the tests with code examples.

## Contributors

TODO: Let people know how they can dive into the project, include important links to things like issue trackers, irc, twitter accounts if applicable.

## License

MIT