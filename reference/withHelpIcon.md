# Append a help icon carrying a tooltip to a label

Used to attach non-obvious control-level guidance at the point of use,
rather than requiring a user to open a panel-level help modal to
understand an individual field.

## Usage

``` r
withHelpIcon(label, tooltip = NULL, placement = "right")
```

## Arguments

- label:

  Label text or tag content

- tooltip:

  Tooltip text shown on hover/focus of the icon. If `NULL`, `label` is
  returned unchanged and no icon is added.

- placement:

  Tooltip placement, passed to
  [`tooltip`](https://rstudio.github.io/bslib/reference/tooltip.html)

## Value

A `tagList` combining the label and, if `tooltip` is supplied, a
tooltip-triggering help icon.

## Examples

``` r
withHelpIcon("Whisker distance", "How far outliers may sit from the box")
#> Whisker distance
#>  
#> <bslib-tooltip placement="right" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>   <template>How far outliers may sit from the box</template>
#>   <span style="cursor: help; color: var(--bs-secondary-color);">
#>     <i class="fas fa-circle-info" role="presentation" aria-label="circle-info icon" verify_fa="FALSE"></i>
#>   </span>
#> </bslib-tooltip>
```
