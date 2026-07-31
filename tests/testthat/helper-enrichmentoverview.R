make_enrichmentoverview_eselist <- function() {
  n_genes <- 60
  counts <- matrix(stats::rpois(n_genes * 4, lambda = 50) + 1, nrow = n_genes)
  rownames(counts) <- paste0("g", seq_len(n_genes))
  colnames(counts) <- paste0("s", 1:4)
  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    condition = c("control", "control", "treated", "treated"),
    batch = c("a", "a", "b", "b")
  )
  gene_set_analyses <- list(counts = list(KEGG = list(
    condition_control_treated = data.frame(
      `p value` = c(0.001, 0.04), FDR = c(0.01, 0.08), Direction = c("Up", "Down"),
      row.names = c("SET_A", "SET_B"), check.names = FALSE
    ),
    batch_a_b = data.frame(
      `p value` = c(0.02, 0.03), FDR = c(0.04, 0.06), Direction = c("Up", "Down"),
      row.names = c("SET_A", "SET_C"), check.names = FALSE
    )
  )))
  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata,
    annotation = data.frame(gene_id = rownames(counts), row.names = rownames(counts)),
    idfield = "gene_id", labelfield = "gene_id", gene_set_analyses = gene_set_analyses
  )
  eselist <- ExploratorySummarizedExperimentList(
    eses = list(genes = ese), group_vars = c("condition", "batch"), default_groupvar = "condition"
  )
  eselist@contrasts <- list(
    list(id = "condition_control_treated", Variable = "condition", Group.1 = "control", Group.2 = "treated"),
    list(id = "batch_a_b", Variable = "batch", Group.1 = "a", Group.2 = "b")
  )
  eselist
}
