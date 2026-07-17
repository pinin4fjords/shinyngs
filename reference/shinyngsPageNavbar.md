# Build the top-level bslib page shell shared by the app modules

Applies the package's Bootstrap 5 theme and dark navbar styling, injects
the package CSS/JS and shinyjs, adds a light/dark mode toggle to the
navbar, and constructs the resulting
[`bslib::page_navbar()`](https://rstudio.github.io/bslib/reference/page_navbar.html).
The accent colour is defined once here, on the theme, and everything
else (CSS, plots) derives from the resulting Bootstrap variables.

## Usage

``` r
shinyngsPageNavbar(navbar_menus)
```

## Arguments

- navbar_menus:

  A named list of arguments accepted by
  [`bslib::page_navbar()`](https://rstudio.github.io/bslib/reference/page_navbar.html)
  (`id`, `title`, `window_title`, and one or more
  [`bslib::nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)/[`bslib::nav_menu()`](https://rstudio.github.io/bslib/reference/nav-items.html)
  elements).

## Value

A
[`bslib::page_navbar()`](https://rstudio.github.io/bslib/reference/page_navbar.html)
