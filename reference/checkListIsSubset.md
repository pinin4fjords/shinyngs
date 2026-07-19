# Check one list is a subset of another and throw an error if not

Check one list is a subset of another and throw an error if not

## Usage

``` r
checkListIsSubset(
  test_list,
  reference_list,
  test_list_name,
  reference_list_name
)
```

## Arguments

- test_list:

  Test list

- reference_list:

  Reference list

- test_list_name:

  Name of test list for error

- reference_list_name:

  Name of reference list for error

## Value

output Returns TRUE if check passes

## Examples

``` r
checkListIsSubset(c("treatment"), c("treatment", "batch"), "contrast variables", "sample metadata")
#> [1] TRUE
```
