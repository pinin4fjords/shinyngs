# make_medium_module_eselist()
#
# A small ExploratorySummarizedExperimentList shared by the assaydatatable,
# rowmetatable, experimenttable and readreports module tests. Row metadata
# includes a low-cardinality "biotype" column (alongside the near-unique
# gene_id/gene_name columns) so categorycountplot has a valid field to tally,
# and colData's "condition" column is similarly low-cardinality for the same
# reason on the sample side.

make_medium_module_eselist <- function(n_genes = 8, n_samples = 4, extra_assay = FALSE, read_reports = list()) {
  gene_ids <- paste0("gene", seq_len(n_genes))
  sample_ids <- paste0("s", seq_len(n_samples))

  counts <- matrix(seq_len(n_genes * n_samples), nrow = n_genes, dimnames = list(gene_ids, sample_ids))

  coldata <- S4Vectors::DataFrame(
    row.names = sample_ids,
    condition = rep(c("ctrl", "treated"), each = n_samples / 2)
  )

  annotation <- data.frame(
    gene_id = gene_ids,
    gene_name = paste0("Gene", seq_len(n_genes)),
    biotype = rep(c("protein_coding", "lncRNA"), length.out = n_genes),
    row.names = gene_ids
  )

  assays_list <- list(counts = counts)
  if (extra_assay) {
    assays_list$norm <- counts / 2
  }

  ese <- ExploratorySummarizedExperiment(
    assays = assays_list, colData = coldata, annotation = annotation,
    idfield = "gene_id", labelfield = "gene_name", read_reports = read_reports
  )

  ExploratorySummarizedExperimentList(list(counts = ese), group_vars = "condition", default_groupvar = "condition")
}
