make_test_ese_inputs <- function() {
  assay <- matrix(
    c(1.2345, 2.3456, 3.4567, 4.5678),
    nrow = 2,
    dimnames = list(c("g1", "g2"), c("s1", "s2"))
  )
  list(
    assays = list(expression = assay),
    colData = data.frame(group = c("a", "b"), row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), label = c("G1", "G2"), row.names = c("g1", "g2"))
  )
}
