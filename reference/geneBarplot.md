# Main function for drawing the bar plot with plotly

The gene module picks specified rows out the assay data, either simply
by id or label. This is used to create a gene-centric info page.

## Usage

``` r
geneBarplot(
  expression,
  experiment,
  colorby,
  expressionmeasure = "Expression",
  palette = NULL
)
```

## Arguments

- expression:

  Matrix of values

- experiment:

  Data frame containing metadata to use for coloring etc

- colorby:

  Column name in `experiment` specifying how points should be colored

- expressionmeasure:

  String to use for labelling y axis in plots

- palette:

  Palette of colors, one for each unique value derived from `colorby`.

## Details

This function does the actual plotting with plotly. It will produce a
plot for every row of the input matrix.

## Examples

``` r
gene("gene", ses)
#> Error in gene("gene", ses): could not find function "gene"
```
