# Configure URL bookmarking for the top-level session

URL bookmarking serialises every input into the address bar. Some inputs
are either transient (event streams, click counters) or would bloat the
URL beyond usefulness (DataTable row/state inputs can enumerate every
row). An observer keeps the exclude list in sync with those inputs as
they appear (tables load and contrast filter sets are inserted after the
initial page).

## Usage

``` r
configureBookmarking(input, session, nav_input = NULL)
```

## Arguments

- input:

  The top-level `input` object, which carries all inputs across module
  namespaces under their fully-qualified names

- session:

  The top-level session

- nav_input:

  Fully-qualified id of the page navbar's tab input, captured and
  restored explicitly because bslib navsets don't participate in
  bookmarking

## Details

On bookmark, the state URL is written to the address bar so it can be
copied and shared directly, with a notification pointing the user there.
