# Build path to the enrichment results

Build path to the enrichment results

## Usage

``` r
build_enrichment_path(template, contrast_info, geneset_type, direction = NULL)
```

## Arguments

- template:

  A string, such as
  `"/path/to/folder/{contrast_name}-{geneset_type}.csv"` or
  `"./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv"`

- contrast_info:

  A list with contrast details: \`id\`, \`reference\`, and \`target\`,
  to be replaced in template.

- geneset_type:

  The name of the geneset type, to be replaced in the template

- direction:

  Either \`"up"\`, \`"down"\` or \`NULL\`, used to determine how the
  replacement will happen.

## Value

A string similar to template, but with the templates replaced

## Details

The template accepts the following:

- `{contrast_name}`:

  Will be replaced by `contrast_info$id` argument

- `{geneset_type}`:

  Will be replaced by the `geneset_type` argument

- `{target|reference}`:

  If the `direction` argument is `"up"`, will be replaced with
  `contrast_info$target`, if it is `"down"`, `contrast_info$reference`
  will be used instead.

## Examples

``` r
build_enrichment_path(
  template = "./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv",
  contrast_info = list(id = "disease_vs_ctrl", reference = "control", target = "disease"),
  geneset_type = "m2.cp.v2024.1.Mm.entrez",
  direction = "up"
)
#> [1] "./disease_vs_ctrl/m2.cp.v2024.1.Mm.entrez/report_for_disease.csv"
```
