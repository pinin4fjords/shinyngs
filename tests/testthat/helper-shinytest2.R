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
# Launch a shinytest2::AppDriver for a prepareApp() type against the shared
# synthetic dataset, skipping the test if no headless Chrome is available.

shinytest2_app_driver <- function(type, name, ...) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  app <- prepareApp(type, shinytest2_eselist())
  shinytest2::AppDriver$new(
    shiny::shinyApp(ui = app$ui, server = app$server),
    name = name,
    height = 900, width = 1400, seed = 42, load_timeout = 60000,
    ...
  )
}
