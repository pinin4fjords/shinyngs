# Validate a formula-based contrast string against fixed-effect coefficients

Validate a formula-based contrast string against fixed-effect
coefficients

## Usage

``` r
validateFormulaBasedContrast(
  contrast_id,
  contrast_formula,
  contrast_string,
  model_matrix = NULL,
  samples
)
```

## Arguments

- contrast_id:

  Contrast identifier

- contrast_formula:

  Formula string used for the contrast

- contrast_string:

  Contrast string to validate

- model_matrix:

  Optional precomputed model matrix for the fixed-effects formula

- samples:

  Data frame of sample information

## Value

output Returns TRUE if validation passes
