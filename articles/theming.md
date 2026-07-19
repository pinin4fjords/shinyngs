# Theming, palettes and shareable views

shinyngs ships three presentation features that are easy to miss but
change how an app looks and how its views are shared: a
colour-blind-safe categorical palette (with an in-app picker), a
light/dark theme built on Bootstrap 5, and URL bookmarking that turns
the current view into a shareable link. The code chunks below are
illustrative and are not evaluated when this article is built.

## Colour-blind-safe palette

Every categorical colour in shinyngs comes, by default, from a single
fixed palette: `COLORBLIND_PALETTE`, an Okabe & Ito (2008)
colour-blind-safe set of eight hues, ordered so that
colour-vision-deficiency separation stays highest between neighbouring
entries. The original palette’s black swatch is replaced with a mid grey
(`#595959`) so it remains visible against both the light and dark app
backgrounds.

``` r

COLORBLIND_PALETTE
#> "#E69F00" "#56B4E9" "#009E73" "#F0E442"
#> "#0072B2" "#D55E00" "#CC79A7" "#595959"
```

Colours are handed out by the exported
[`make_color_scale()`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md)
helper. Because it draws from the fixed palette in order, the same
position always gets the same colour, so a group keeps its colour across
every plot in the app:

``` r

library(shinyngs)

# The colour-blind-safe palette is the default (palette = "colorblind")
make_color_scale(4)
#> "#E69F00" "#56B4E9" "#009E73" "#F0E442"

# Ask for more colours than the base palette provides and shades are
# interpolated between them, with a message noting that colour-blind
# separation can no longer be guaranteed for every pair.
make_color_scale(12)
```

Passing any other value treats it as an RColorBrewer qualitative palette
name (for example `"Dark2"` or `"Set1"`), expanded with interpolation
when more colours are requested than the palette holds:

``` r

make_color_scale(6, palette = "Dark2")
```

### The in-app palette picker

Modules that colour by an experimental variable expose a palette
drop-down through the `colormaker` module
([`colormakerInput()`](https://pinin4fjords.github.io/shinyngs/reference/colormakerInput.md)
/
[`colormaker()`](https://pinin4fjords.github.io/shinyngs/reference/colormaker.md)).
The first choice, labelled “Colour-blind safe”, selects
`COLORBLIND_PALETTE`; the remaining choices are the RColorBrewer
qualitative palettes, offered for users who prefer them. The
colour-blind-safe palette is always the pre-selected default.

``` r

# UI: the palette drop-down
colormakerInput(ns("groupby"))

# Server: a reactive returning the chosen palette, sized to the number of
# categories the caller reports
getPalette <- colormaker("groupby", getNumberCategories = getNumberCategories)
```

In practice you rarely wire `colormaker` up yourself: the `groupby`
module already embeds the picker via
[`colormakerInput()`](https://pinin4fjords.github.io/shinyngs/reference/colormakerInput.md)
and returns `getPalette` (a reactive) alongside `getGroupby`. A plotting
module then passes that palette straight into its standalone plot
function (for example
`interactive_boxplot(..., palette = groupby_reactives$getPalette())`).
Those plot functions also accept a `palette_name` argument (defaulting
to the colour-blind-safe palette) so they produce consistent colours
when called directly, outside an app.

## Theming: Bootstrap 5 and light/dark mode

The app shell is a
[`bslib::page_navbar()`](https://rstudio.github.io/bslib/reference/page_navbar.html)
built in
[`shinyngsPageNavbar()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsPageNavbar.md).
It applies a Bootstrap 5 theme via
[`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
using the `cosmo` bootswatch, with the package’s accent colour
(`SHINYNGS_ACCENT`, `#2780e3`) set as the Bootstrap `primary`. That
accent is defined once, on the theme, and everything else (CSS, spinner
colours, plot styling) derives from the resulting Bootstrap variables.

``` r

bslib::bs_theme(version = 5, bootswatch = "cosmo", primary = "#2780e3")
```

### Light/dark mode toggle

The navbar carries a light/dark switch built from
[`bslib::input_dark_mode()`](https://rstudio.github.io/bslib/reference/input_dark_mode.html),
with the fixed id `shinyngs_dark_mode`. Toggling it flips Bootstrap’s
`data-bs-theme` attribute, which restyles the whole page.

``` r

bslib::input_dark_mode(id = "shinyngs_dark_mode")
```

One subtlety worth knowing: `input_dark_mode` only sets `data-bs-theme`
once its web component hydrates on the client. To avoid a flash of the
light theme on a dark-resolved load,
[`shinyngsPageNavbar()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsPageNavbar.md)
injects a tiny inline `<head>` script that resolves the OS colour scheme
and sets `data-bs-theme` before the first paint.

The dark-mode toggle is deliberately *not* bookmarked (see below), so a
shared link does not force your colour-scheme preference on whoever
opens it.

### Configuring the theme

Theming is centralised in
[`shinyngsPageNavbar()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsPageNavbar.md)
rather than exposed as a per-app option, so all app types (`rnaseq`,
`chipseq`, `illuminaarray`, and the single-module `simpleApp`) get the
same look. To re-skin the app, the change is made in one place: swap the
`bootswatch` name or the `primary` colour on the `bs_theme()` call, and
the CSS and plots follow from the Bootstrap variables it produces.

## Shareable views and URL bookmarking

shinyngs uses Shiny’s URL bookmarking (`enableBookmarking("url")`) so
that the current view, that is, which tab is open, which experiment and
assay are selected, the row/column filters, plot options and so on, is
encoded into the address bar. Copying that URL and re-opening it
restores the same view.

Bookmarking is enabled in two places:
[`prepare_app()`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md)
sets it so the option is present wherever the app object ends up, and
[`configureBookmarking()`](https://pinin4fjords.github.io/shinyngs/reference/configureBookmarking.md)
sets it again in the running session (harnesses can transport the app
object to a fresh process where the earlier option would be lost).

### The “Share view” button

The navbar’s “Share view” control is an `actionButton` with the id
`shinyngs_share_view`. Clicking it calls `session$doBookmark()`, which
writes the state URL to the address bar, copies it to the clipboard (via
a custom `shinyngs_copy_link` message handled in the package JS), and
shows a confirmation notification.

``` r

observeEvent(input$shinyngs_share_view, {
  session$doBookmark()
})

onBookmarked(function(url) {
  updateQueryString(url)
  session$sendCustomMessage("shinyngs_copy_link", url)
  showNotification("Shareable link copied to your clipboard ...")
})
```

### What is and isn’t captured

[`configureBookmarking()`](https://pinin4fjords.github.io/shinyngs/reference/configureBookmarking.md)
keeps a live exclude list so the URL stays useful. Transient or bloating
inputs are excluded as they appear: DataTable internals
(`_rows_current`, `_state`, `_search`, …), plotly event streams
(`plotly_`), help-modal triggers, and the presentation controls that
shouldn’t travel with a shared link (`shinyngs_dark_mode`,
`shinyngs_plot_format`, `shinyngs_share_view`). An observer watches for
newly-appeared inputs (tables and inserted contrast filters stream in
after the initial page) and pushes an updated exclude set via
`setBookmarkExclude()`.

Two pieces of state need explicit handling because bslib navsets and
server-side selectize inputs don’t bookmark themselves:

- The active navbar tab is saved in `onBookmark()` and re-applied in
  `onRestored()` via `updateTabsetPanel()`.
- Server-side selectize inputs hold no options client-side, so their
  saved value is re-applied when their choices repopulate.
  [`bookmarkedInputValue()`](https://pinin4fjords.github.io/shinyngs/reference/bookmarkedInputValue.md)
  reads that value back out of the restore state, trying the module’s
  namespaced id first and falling back to the bare id.

``` r

bookmarkedInputValue(state, session, "myField")
```

The net effect: a colleague opening your shared link lands on the same
tab, looking at the same experiment, assay, filters and plot settings
you had, while their own light/dark preference is left untouched.
