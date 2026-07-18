# Read a bookmarked input value from a restore state

Server-side selectize inputs hold no options client-side, so their
bookmarked value can't restore itself and must be re-applied when the
choices repopulate. This reads that value from an `onRestore` state,
trying the module's namespaced id first and falling back to the bare id.

## Usage

``` r
bookmarkedInputValue(state, session, id)
```

## Arguments

- state:

  The restore state passed to an `onRestore` callback

- session:

  The module session

- id:

  The unqualified input id

## Value

The bookmarked value, or `NULL` if absent
