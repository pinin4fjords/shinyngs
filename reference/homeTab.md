# Build the landing ("Home") tab for a full shinyngs application

Produces a full-width Home `tabPanel` with a study header, a "Jump to
analysis" card, the interactive summary tiles (`summarytilesOutput`),
the study description, and an unobtrusive footer carrying the interface
note and bug-report link. Shared by the rnaseq, chipseq and
illuminaarray applications.

## Usage

``` r
homeTab(ns, eselist, platform = "RNA-seq")
```

## Arguments

- ns:

  Namespace function for the calling application module

- eselist:

  ExploratorySummarizedExperimentList object

- platform:

  Human-readable platform name used in the interface note (e.g.
  "RNA-seq")

## Value

A `tabPanel`
