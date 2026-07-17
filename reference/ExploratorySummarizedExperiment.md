# ExploratorySummarizedExperiments

This function creates objects of the ExploratorySummarizedExperiment
class, an extension of SummarizedExperiment designed to hold additional
information about the features present - for example differential
expression values and the type of identifiers used in rows.

## Usage

``` r
ExploratorySummarizedExperiment(
  assays,
  colData,
  annotation,
  idfield,
  labelfield = character(),
  entrezgenefield = character(),
  contrast_stats = list(),
  assay_measures = list(),
  gene_set_analyses = list(),
  dexseq_results = list(),
  read_reports = list(),
  gene_set_analyses_tool = list()
)
```

## Arguments

- assays:

  An object of class SimpleList as would be supplied to the
  SummarizedExperiment constructor

- colData:

  An object of class DataFrame as would be supplied to the
  SummarizedExperimentConstructor. Row names could correspond to column
  names in the matrices in `assays`

- annotation:

  A data frame with annotation for the features (rows) of the `assays`
  matrices. Rows must correspond to to those matrices.

- idfield:

  To which of the `annotation` columns do row names correspond?

- labelfield:

  Which column from `annotation` should be used to label features (e.g.
  a gene name field)?

- entrezgenefield:

  Which column from `annotation` is the Entrez gene ID?

- contrast_stats:

  List of matrices containing contrast-related statistics. Only 'pvals',
  'qvals' and 'fold_changes' are currently used. Fold changes are
  calculated on the fly where not supplied. Matrix columns correspond to
  'contrasts' set in the containing SummarizedExperimentList.

- assay_measures:

  Optional List of measures to display related to each assay.

- gene_set_analyses:

  Three-level nested lists of gene set tables keyed first by assay, then
  by gene set type and then by contrast.

- dexseq_results:

  An optional list of `DEXSeqResults` objects corresponding to the
  contrasts listed in the `contrasts` slot..

- read_reports:

  A named list of matrices with read counts in columns and sample names
  in rows. Useful for providing mapped read counts, counts per gene type
  etc

- gene_set_analyses_tool:

  Three-level nested lists of a string, nested as `gene_set_analyses`.
  Each string may be `"auto"` (the default), `"gsea"` or `"roast"`. It
  defines the format of the corresponding `gene_set_analyses` table.

## Value

output An ExploratoryRangedSummarizedExperient object

## Details

It is intended that one or more ExploratorySummarizedExperiments with
the same samples (columns) are contained within an
ExploratorySumarizedExperimentList, which will contain information
relevant to all experiments such as gene sets and contrasts.

It's clear that the structure of this class and that of
SummarizedExperimentList will need to be refined in future.
