# Build one shared color palette across every value in an annotation data frame

heatmaply's `col_side_colors`/`row_side_colors` mechanism encodes the
values of all annotation variables onto a single combined color scale,
so a single palette covering every unique value across every column is
needed, rather than one palette per column.

## Usage

``` r
combinedAnnotationColors(sample_annotation)
```

## Arguments

- sample_annotation:

  A data frame with sample metadata

## Value

output A named vector of colors, one per unique value across all columns
of `sample_annotation`
