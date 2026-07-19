# The brand accent colour. bslib's bs_theme() and shinycssloaders both need a
# literal hex here (neither resolves a var(--bs-primary) reference), so this
# constant is the single source both derive from.
SHINYNGS_ACCENT <- "#2780e3"

#' Make a hidden input field. Handy for replacing superfluous single-value
#' selects etc
#'
#' @param id An HTML id
#' @param values The value the input should return
#'
#' @return output HTML as output by Shiny's \code{HTML()}
#'
#' @export
#'
#' @examples
#' hidden_input("myid", "iamavalue")
#'
hidden_input <- function(id, values) {
  HTML(paste0(unlist(lapply(values, function(value) paste0("<input type='text' id='", id, "' value='", value, "' style='display: none;'>")))))

  # HTML(paste0('<input type='text' id='', id, '' value='', value, '' style='display: none;'>'))
}

#' Simple list push
#'
#' @param input_list A list to push to
#' @param element The element to push
#'
#' @return list with element pushed
#'
#' @export
#'
#' @examples
#' mylist <- push_to_list(mylist, "new element")
#'
push_to_list <- function(input_list, element) {
  input_list[[length(input_list) + 1]] <- element
  input_list
}

#' Build the top-level bslib page shell shared by the app modules
#'
#' Applies the package's Bootstrap 5 theme and dark navbar styling, injects
#' the package CSS/JS and shinyjs, adds a light/dark mode toggle and a
#' "Credits" link (software acknowledgements, via the \code{shinyngs_credits}
#' modal wired up in \code{configureBookmarking()}) to the navbar, and
#' constructs the resulting \code{bslib::page_navbar()}. The accent colour is
#' defined once here, on the theme, and everything else (CSS, plots) derives
#' from the resulting Bootstrap variables.
#'
#' @param navbar_menus A named list of arguments accepted by
#' \code{bslib::page_navbar()} (\code{id}, \code{title}, \code{window_title},
#' and one or more \code{bslib::nav_panel()}/\code{bslib::nav_menu()}
#' elements).
#'
#' @return A \code{bslib::page_navbar()}
#'
#' @keywords internal
#'
shinyngsPageNavbar <- function(navbar_menus) {
  cssfile <- system.file("www", paste0(packageName(), ".css"), package = packageName())
  jsfile <- system.file("www", paste0(packageName(), ".js"), package = packageName())
  shiny::addResourcePath("shinyngs-www", system.file("www", package = packageName()))
  navbar_menus$theme <- bslib::bs_theme(version = 5, bootswatch = "cosmo", primary = SHINYNGS_ACCENT)
  navbar_menus$navbar_options <- bslib::navbar_options(bg = "dark", theme = "dark")

  # Resolve the light/dark theme in <head> before first paint. input_dark_mode
  # only sets data-bs-theme once its web component hydrates, so a dark-resolved
  # load (dark OS scheme) would otherwise paint the default light theme for a
  # frame or two first, flashing white (most visibly across large plots).
  no_flash <- tags$head(tags$script(HTML(
    "(function(){try{var d=document.documentElement;if(!d.lang){d.lang='en';}if(!d.getAttribute('data-bs-theme')){var m=window.matchMedia&&window.matchMedia('(prefers-color-scheme: dark)').matches;d.setAttribute('data-bs-theme',m?'dark':'light');}}catch(e){}})();"
  )))
  navbar_menus$header <- tagList(no_flash, includeCSS(cssfile), includeScript(jsfile), shinyjs::useShinyjs())
  navbar_menus <- c(navbar_menus, list(
    bslib::nav_spacer(),
    bslib::nav_item(actionButton(
      "shinyngs_share_view",
      label = "Share view",
      icon = icon("share-nodes"),
      class = "btn-sm",
      title = "Copy a shareable link that restores the current view"
    )),
    bslib::nav_item(a11yControl(
      bslib::input_dark_mode(id = "shinyngs_dark_mode"),
      label = "Toggle light or dark mode",
      placement = "bottom"
    )),
    bslib::nav_item(a11yControl(
      tags$button(
        id = "shinyngs_plot_format_toggle",
        type = "button",
        class = "btn btn-link shinyngs-navbar-icon-btn",
        `data-format` = "png",
        icon("file-image")
      ),
      label = "Plot download format: PNG. Click to switch to SVG.",
      tooltip = "Format used by each plot's camera/download button - click to toggle PNG/SVG",
      placement = "bottom"
    )),
    bslib::nav_item(modalInput(
      "shinyngs_credits", "Credits",
      class = "btn btn-sm",
      tooltip = "Software this app is built on"
    ))
  ))
  do.call(bslib::page_navbar, navbar_menus)
}

#' Read a bookmarked input value from a restore state
#'
#' Server-side selectize inputs hold no options client-side, so their bookmarked
#' value can't restore itself and must be re-applied when the choices repopulate.
#' This reads that value from an \code{onRestore} state, trying the module's
#' namespaced id first and falling back to the bare id.
#'
#' @param state The restore state passed to an \code{onRestore} callback
#' @param session The module session
#' @param id The unqualified input id
#'
#' @return The bookmarked value, or \code{NULL} if absent
#'
#' @keywords internal
#'
bookmarkedInputValue <- function(state, session, id) {
  val <- state$input[[session$ns(id)]]
  if (is.null(val)) {
    val <- state$input[[id]]
  }
  val
}

#' Configure URL bookmarking for the top-level session
#'
#' URL bookmarking serialises every input into the address bar. Some inputs are
#' either transient (event streams, click counters) or would bloat the URL
#' beyond usefulness (DataTable row/state inputs can enumerate every row). An
#' observer keeps the exclude list in sync with those inputs as they appear
#' (tables load and contrast filter sets are inserted after the initial page).
#'
#' On bookmark, the state URL is written to the address bar so it can be copied
#' and shared directly, with a notification pointing the user there.
#'
#' Also wires up the \code{shinyngs_credits} modal server, since this is the
#' one place called once per top-level session across every app type
#' (rnaseq, chipseq, illuminaarray, simpleApp).
#'
#' @param input The top-level \code{input} object, which carries all inputs
#'   across module namespaces under their fully-qualified names
#' @param session The top-level session
#' @param nav_input Fully-qualified id of the page navbar's tab input, captured
#'   and restored explicitly because bslib navsets don't participate in
#'   bookmarking
#'
#' @keywords internal
#'
configureBookmarking <- function(input, session, nav_input = NULL) {
  # Also set here (not just in prepare_app) so bookmarking is active in whichever
  # process actually runs the app, including harnesses that transport the app
  # object to a fresh process where prepare_app's option would be lost.
  enableBookmarking("url")

  modalServer("shinyngs_credits", "Credits")

  # session$userData is shared with every nested module session, so plot
  # modules can read the chosen download format without each one taking it
  # as an extra constructor argument.
  session$userData$plotFormat <- reactive({
    fmt <- input$shinyngs_plot_format
    if (is.null(fmt)) "png" else fmt
  })

  patterns <- c(
    "_rows_current", "_rows_all", "_rows_selected", "_state", "_search",
    "_cell_clicked", "_cells_selected", "_columns", # DT internals
    "plotly_", ".clientValue-", # plotly event streams
    "-link", # help-modal triggers
    "shinyngs_dark_mode", "shinyngs_plot_format", "shinyngs_share_view", "insertBtn", "removeBtn"
  )

  # Grep only names that are new since the last fire (inputs stream in as tables
  # load and filter sets are inserted) and only push the exclude list when it
  # actually grows, since setBookmarkExclude replaces the whole set.
  seen <- character(0)
  excluded <- character(0)
  observe({
    nms <- names(input)
    fresh <- setdiff(nms, seen)
    seen <<- nms
    if (length(fresh) == 0) {
      return()
    }
    hits <- unique(unlist(lapply(patterns, grep, x = fresh, fixed = TRUE, value = TRUE)))
    if (length(hits) > 0) {
      excluded <<- union(excluded, hits)
      setBookmarkExclude(excluded, session = session)
    }
  })

  observeEvent(input$shinyngs_share_view, {
    session$doBookmark()
  })

  if (!is.null(nav_input)) {
    onBookmark(function(state) {
      state$values$active_tab <- isolate(input[[nav_input]])
    })
    onRestored(function(state) {
      tab <- state$values$active_tab
      if (!is.null(tab)) {
        updateTabsetPanel(session, nav_input, selected = tab)
        # Selecting a panel that lives in a nav_menu leaves its dropdown
        # visually open; close any open navbar dropdown once the selection has
        # applied on the client.
        shinyjs::runjs(
          "setTimeout(function(){document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function(e){e.classList.remove('show');});}, 300);"
        )
      }
    })
  }

  onBookmarked(function(url) {
    updateQueryString(url)
    session$sendCustomMessage("shinyngs_copy_link", url)
    showNotification(
      "Shareable link copied to your clipboard (and shown in the address bar).",
      type = "message", duration = 6, session = session
    )
  }, session = session)
}

#' Lay out a module's controls beside its output
#'
#' Wraps the standard "controls on the left, output on the right" arrangement
#' used by every analysis tab in a \code{bslib::layout_sidebar()}. The sidebar
#' collapses on narrow screens; the main area holds the output (typically a
#' \code{moduleMain()}).
#'
#' @param controls Sidebar content, usually a module's input UI
#' @param main Main content, usually a \code{moduleMain()}
#' @param width Sidebar width in pixels
#'
#' @return A \code{bslib::layout_sidebar()}
#'
#' @keywords internal
#'
moduleLayout <- function(controls, main, width = 320) {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(controls, width = width, open = "desktop"),
    main,
    fillable = FALSE,
    border = FALSE,
    class = "shinyngs-panel"
  )
}

#' Assemble a module's main-panel content
#'
#' Lays out a module's output: an optional help-modal trigger floated to the top
#' right, the section title, and the output body (plots, tables). The content
#' sits directly in the panel with no surrounding card, keeping the plot area
#' uncluttered. Pass \code{title = NULL} for modules that render their own
#' (often dynamic) heading, to avoid a duplicate title.
#'
#' @param title Section title (a string or tag), or \code{NULL} to omit it
#' @param ... Body content (plots, tables, sub-headings)
#' @param help Optional help-modal trigger, floated to the top right
#'
#' @return A \code{tagList}
#'
#' @keywords internal
#'
moduleMain <- function(title, ..., help = NULL) {
  tagList(
    help,
    if (!is.null(title)) h3(class = "shinyngs-section-title", title),
    ...
  )
}

#' Accent colour for loading spinners
#'
#' \code{shinycssloaders::withSpinner()} bakes its colour into a literal CSS
#' value rather than accepting a \code{var(--bs-primary)} reference, so it
#' can't pick up the Bootstrap theme variable directly. This returns the
#' brand accent (\code{SHINYNGS_ACCENT}), the same value the theme's
#' \code{primary} colour derives from.
#'
#' @return A hex colour string
#'
#' @keywords internal
#'
shinyngsSpinnerColor <- function() {
  SHINYNGS_ACCENT
}

#' Apply shinyngs' shared plotly toolbar configuration
#'
#' Gives every interactive plot the same modebar: the plotly logo is dropped,
#' the noisier selection buttons are removed, and the "download plot" button
#' produces an image named after the plot (rather than the generic
#' \code{newplot.png}) in the user's chosen format. Call once on the assembled
#' plotly object before returning it from a \code{renderPlotly()}.
#'
#' @param p A plotly object
#' @param filename Base name (no extension) for the downloaded image
#' @param format Image format for the download button, e.g. \code{"png"} or
#'   \code{"svg"}
#'
#' @return The plotly object with a shared \code{config()} applied
#'
#' @keywords internal
#'
shinyngsPlotlyConfig <- function(p, filename = "shinyngs_plot", format = "png") {
  plotly::config(
    p,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("sendDataToCloud", "lasso2d", "select2d"),
    toImageButtonOptions = list(format = format, filename = filename)
  )
}

#' Create sets of fields for display
#'
#' Shiny apps can get cluttered with many inputs. This method wraps sets of
#' fields in a \code{bslib::accordion()}, one panel per named element, with
#' every panel able to be open at once (there is no "close others on open"
#' behaviour).
#'
#' @param id ID field to apply to the overall container
#' @param fieldset_list A named list, each element containing one or more
#' fields.
#' @param open Controls which panels are open by default, as a character vector
#' of panel names. In most cases all should be left open (the default), since
#' fields in collapsed panels may be less discoverable.
#'
#' @return A \code{bslib::accordion()}

fieldSets <- function(id, fieldset_list, open = NULL) {
  if (is.null(open)) {
    open <- TRUE
  }

  panels <- lapply(names(fieldset_list), function(listname) {
    bslib::accordion_panel(
      title = prettify_variable_name(listname),
      value = listname,
      fieldset_list[[listname]]
    )
  })

  do.call(bslib::accordion, c(list(id = id, open = open, multiple = TRUE), panels))
}
