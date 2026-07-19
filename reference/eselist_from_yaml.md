# Build an ExploratorySummarisedExperimentList from a YAML description

Building ExploratorySummarisedExperimentList objects can be a bit
fiddly. This function makes automates object construction based on a
descriptor in yaml format.

## Usage

``` r
eselist_from_yaml(configfile)
```

## Arguments

- configfile:

  A YAML-format config file describing the data to be compiled into an
  ExploratorySummarizedExperimentList object

## Value

out An ExploratorySummarizedExperimentList object suitable for passing
to
[`prepare_app`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md)

## Details

For a simple study with one 'experiement' for Gene-level results, and
three 'assays' describing raw, filtered and normalised expression you
might make a YAML like:

    title: My RNA seq experiment
    author: Joe Blogs
    report: report.md
    group_vars:
      - Group
      - Replicate
    default_groupvar: Group
    experiments:
      Gene:
        coldata:
          file: my.experiment.csv
          id: External
        annotation:
          file: my.annotation.csv
          id: gene_id
          entrez: ~
          label: gene_id
        expression_matrices:
          Raw:
            file: raw_counts.csv
            measure: counts
          Filtered:
            file: filtered_counts.csv
            measure: Counts per million
          Normalised:
            file: normalised_counts.csv
            measure: Counts per million
        read_reports:
          read_attrition: read_attrition.csv
    contrasts:
      comparisons:
      - Variable: Group
        Group.1: control
        Group.2: TreatmentA
      - Variable: Group
        Group.1: control
        Group.2: TreatmentB
    contrast_stats:
      Gene:
        Normalised:
          pvals: pvals.csv
          qvals: qvals.csv

## Examples

``` r
eselist <- eselist_from_yaml("my.yaml")
#> Warning: cannot open file 'my.yaml': No such file or directory
#> Error in readLines(con, warn = readLines.warn): cannot open the connection
```
