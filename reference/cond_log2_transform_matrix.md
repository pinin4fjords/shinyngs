# Apply log2 transformation on a matrix.

Apply log2 transformation on a matrix.

## Usage

``` r
cond_log2_transform_matrix(
  matrix_data,
  should_transform = NULL,
  threshold = 30,
  rmzeros = FALSE,
  small_value = 1,
  reverse = FALSE
)
```

## Arguments

- matrix_data:

  A matrix containing data.

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

- threshold:

  A numeric threshold to determine if the matrix should be
  log-transformed. This is only checked if should_transform is NULL.

- rmzeros:

  A boolean indicating whether to remove zeros from the matrix. If TRUE,
  zeros are removed. Default is FALSE.

- small_value:

  A small value to add to zero entries before log transformation.

- reverse:

  Boolean, should we unlog rather than log?

## Value

A modified matrix.

## Examples

``` r
# Create a sample matrix
mat <- matrix(c(10, 0, 30, 0, 50, 60), ncol = 2)

# Use the function with different parameters
transformed_mat1 <- cond_log2_transform_matrix(mat, should_transform = TRUE)
transformed_mat2 <- cond_log2_transform_matrix(mat, should_transform = NULL, threshold = 40)
transformed_mat3 <- cond_log2_transform_matrix(mat, rmzeros = TRUE)
```
