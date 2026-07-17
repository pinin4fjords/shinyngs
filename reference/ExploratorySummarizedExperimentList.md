# ExploratorySummarizedExperimentLists, containers for ExploratorySummarizedExperiments

ExploratorySummarizedExperiment lists are intented to contain one or
more ExploratorysummarizedExperiments with the same sets of
samples/columns but different feature sets. The motivating use case was
the desire to examine expression at both transcript and gene levels in
RNA-seq experiments explorted via `Shinyngs`

## Usage

``` r
ExploratorySummarizedExperimentList(
  eses,
  title = "",
  author = "",
  description = "",
  static_pdf = character(),
  group_vars = character(),
  default_groupvar = character(),
  contrasts = list(),
  url_roots = list(),
  gene_sets = list(),
  gene_set_id_type = character(),
  ensembl_species = character()
)
```

## Arguments

- eses:

  List of ExploratorySummarizedExperiments

- title:

  Study title

- author:

  Study authors

- description:

  Study summary to displayed on front page

- static_pdf:

  A URL to a static PDF document to be displayed on the front page.

- group_vars:

  Variables by which a user will be allowed to group the samples of
  individual experiments, must correspond to their `colData`

- default_groupvar:

  Default `group_var`

- contrasts:

  List of length-3 vectors containing 1) the `group_var`, 2) the
  `group_var` value corresponding to the 'control' side and 3) the value
  corresponding to the 'treatment' side

- url_roots:

  A list of URL roots, with list names corresponding to metadata column
  names of the experiments. Exploratory tools displayed via `shinyngs`
  can use these roots to construct URLs to 'link out'.

- gene_sets:

  A named list of GeneSetCollections as might be produced by reading
  .gmt format gene sets (for example from MSigDB) using
  GSEABase::getGmt(). These must contain identifiers of the type
  specified in `gene_set_id_type`.

- gene_set_id_type:

  A column found in the metadata of the component
  ExploratorySummarizedExperiment objects via `mcols()`. Used to relate
  the rows of assays to gene sets.

- ensembl_species:

  Ensembl species definition like 'mmusculus'. Used to interface with
  BiomaRt e.g. to make gene model plots in the `gene` module.

## Value

output An ExploratorySummarizedExperimentList

## Details

As a the containing object for experiments, this class is intented to
contain various variables relevant across a whole study, which will be
displayed in an exploratory interface generated in `shinyngs`. This
includes the study title, author etc as well as definitions of the
contrasts used in differential analysis and the gene sets relevant to
all experiments
