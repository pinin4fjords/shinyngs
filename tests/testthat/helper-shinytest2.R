# shinytest2_eselist()
#
# A small synthetic ExploratorySummarizedExperimentList with contrasts and
# contrast_stats populated, so PCA/heatmap/differential-table paths all have
# something to render. Built once per test session and cached.

shinytest2_eselist <- local({
  eselist <- NULL
  function() {
    if (is.null(eselist)) {
      set.seed(42)

      n_genes <- 60
      n_samples <- 12

      counts <- matrix(
        stats::rnbinom(n_genes * n_samples, mu = 200, size = 5),
        nrow = n_genes, ncol = n_samples
      )
      rownames(counts) <- paste0("gene", seq_len(n_genes))
      colnames(counts) <- paste0("sample", seq_len(n_samples))

      coldata <- S4Vectors::DataFrame(
        row.names = colnames(counts),
        condition = rep(c("control", "treated"), each = n_samples / 2),
        batch = rep(c("batch1", "batch2"), times = n_samples / 2)
      )

      annotation <- data.frame(
        gene_id = rownames(counts),
        gene_name = paste0("Gene", seq_len(n_genes)),
        row.names = rownames(counts)
      )

      pvals <- matrix(stats::runif(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
      qvals <- matrix(stats::p.adjust(pvals[, 1], method = "BH"), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
      fold_changes <- matrix(stats::rnorm(n_genes, sd = 2), nrow = n_genes, dimnames = list(rownames(counts), "V1"))

      ese <- ExploratorySummarizedExperiment(
        assays = S4Vectors::SimpleList(counts = counts),
        colData = coldata,
        annotation = annotation,
        idfield = "gene_id",
        labelfield = "gene_name",
        contrast_stats = list(counts = list(pvals = pvals, qvals = qvals, fold_changes = fold_changes))
      )

      new_eselist <- ExploratorySummarizedExperimentList(
        eses = list(counts = ese),
        title = "shinytest2 smoke test study",
        author = "shinytest2",
        description = "Synthetic dataset for shinytest2 smoke tests",
        group_vars = c("condition", "batch"),
        default_groupvar = "condition"
      )
      new_eselist@contrasts <- list(
        list(id = "condition_control_treated", Variable = "condition", Group.1 = "control", Group.2 = "treated")
      )

      eselist <<- new_eselist
    }
    eselist
  }
})

# shinytest2_app_driver()
#
# Launch a shinytest2::AppDriver for a prepare_app() type, skipping the test if
# no headless Chrome is available.

shinytest2_app_driver <- function(type, name, eselist = NULL, ...) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  if (is.null(eselist)) {
    eselist <- shinytest2_eselist()
  }
  app <- prepare_app(type, eselist)
  driver <- shinytest2::AppDriver$new(
    shiny::shinyApp(ui = app$ui, server = app$server),
    name = name,
    height = 900, width = 1400, seed = 42, load_timeout = 60000,
    wait = FALSE,
    ...
  )
  DIAG_wait_for_idle(driver, name)
  driver
}

DIAG_wait_for_idle <- function(driver, name) {
  js <- "new Promise((resolve, reject) => {
    let idleId = null;
    const timeoutId = setTimeout(() => reject('timeout'), 60000);
    const done = () => { clearTimeout(timeoutId); resolve('idle'); };
    $(document).on('shiny:busy', () => clearTimeout(idleId));
    $(document).on('shiny:idle', () => { idleId = setTimeout(done, 200); });
    if (window.shinytest2.busy !== true) idleId = setTimeout(done, 200);
  })"
  ret <- driver$get_chromote_session()$Runtime$evaluate(
    js, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 120
  )
  if (identical(ret$result$subtype, "error") || length(ret$exceptionDetails) > 0) {
    message("DIAG[", name, "] idle wait failed")
    message("DIAG result: ", paste(utils::capture.output(str(ret$result)), collapse = "\n"))
    message("DIAG exceptionDetails: ", paste(utils::capture.output(str(ret$exceptionDetails)), collapse = "\n"))
    probe <- driver$get_chromote_session()$Runtime$evaluate(
      "JSON.stringify({href: location.href, readyState: document.readyState, jq: typeof window.jQuery, dollar: typeof window.$, dollarIsJq: window.$ === window.jQuery, st2: typeof window.shinytest2, shiny: typeof window.Shiny, connected: !!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected())})",
      returnByValue = TRUE
    )
    message("DIAG probe: ", probe$result$value)
    message("DIAG logs:\n", paste(utils::capture.output(print(driver$get_logs())), collapse = "\n"))
    stop("DIAG idle wait failed for ", name)
  }
  invisible(driver)
}

# shinytest2_bookmark_app_driver()
#
# Launch an AppDriver against an on-disk app.R rather than a shinyApp object.
# URL bookmarking must be enabled when the session is constructed (that is when
# module onBookmark hooks register and Shiny decides whether to keep the share
# button), which prepare_app() does. Passing a shinyApp object to AppDriver runs
# it in a fresh process that never called prepare_app, so bookmarking would be
# off; running from an app.R that calls prepare_app in that process mirrors real
# use. Requires shinyngs to be installed (it is under R CMD check).

shinytest2_bookmark_app_driver <- function(type, name, ...) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("shinyngs")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  dir <- withr::local_tempdir(.local_envir = parent.frame())
  saveRDS(shinytest2_eselist(), file.path(dir, "eselist.rds"))
  writeLines(
    c(
      "library(shinyngs)",
      "eselist <- readRDS('eselist.rds')",
      sprintf("app <- prepare_app('%s', eselist)", type),
      "shiny::shinyApp(app$ui, app$server)"
    ),
    file.path(dir, "app.R")
  )

  shinytest2::AppDriver$new(
    dir,
    name = name,
    height = 900, width = 1400, seed = 42, load_timeout = 90000,
    ...
  )
}
