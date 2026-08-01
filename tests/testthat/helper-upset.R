make_upset_eselist <- function() {
  n_genes <- 12
  gene_ids <- paste0("gene", seq_len(n_genes))

  set.seed(1)
  counts <- matrix(stats::rpois(n_genes * 4, lambda = 50) + 1, nrow = n_genes)
  rownames(counts) <- gene_ids
  colnames(counts) <- paste0("s", 1:4)

  coldata <- S4Vectors::DataFrame(
    row.names = colnames(counts),
    grpA = c("ctrl", "ctrl", "treatA", "treatA"),
    grpB = c("ctrl", "ctrl", "treatB", "treatB")
  )
  annotation <- data.frame(gene_id = gene_ids, row.names = gene_ids)

  fc1 <- c(3, 3, 3, 2, 2, -2, -2, -2, -2, -3, -3, -3)
  fc2 <- c(4, 4, 4, -4, -4, 5, 5, 5, 5, -5, -5, -5)
  fold_changes <- matrix(c(fc1, fc2), nrow = n_genes, dimnames = list(gene_ids, c("1", "2")))

  ese <- ExploratorySummarizedExperiment(
    assays = S4Vectors::SimpleList(counts = counts), colData = coldata, annotation = annotation,
    idfield = "gene_id", contrast_stats = list(counts = list(fold_changes = fold_changes))
  )

  eselist <- ExploratorySummarizedExperimentList(eses = list(counts = ese), group_vars = c("grpA", "grpB"), default_groupvar = "grpA")
  eselist@contrasts <- list(
    list(id = "c1", Variable = "grpA", Group.1 = "ctrl", Group.2 = "treatA"),
    list(id = "c2", Variable = "grpB", Group.1 = "ctrl", Group.2 = "treatB")
  )
  eselist
}
