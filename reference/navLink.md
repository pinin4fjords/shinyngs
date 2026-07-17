# Build a link that activates a navbar tab client-side

Produces an anchor whose click shows the `tabPanel` carrying the given
`value`, using the Bootstrap tab plugin. This keeps landing-page
navigation on the client, avoiding cross-namespace `updateNavbarPage()`
wiring. Tab values targeted this way must be unique across the app.

## Usage

``` r
navLink(label, value, class = NULL, icon = NULL)
```

## Arguments

- label:

  Link label (a string or tag)

- value:

  The `value` of the target `tabPanel`

- class:

  Optional CSS class for the anchor

- icon:

  Optional icon tag placed before the label

## Value

An anchor tag
