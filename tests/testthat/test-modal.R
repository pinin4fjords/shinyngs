test_that("modalServer re-evaluates function title and content on each open", {
  titles <- character()
  bodies <- character()
  gene <- "gene A"
  title_fn <- function() {
    titles <<- c(titles, paste("info for", gene))
    tail(titles, 1)
  }
  content_fn <- function() {
    bodies <<- c(bodies, paste("body for", gene))
    tail(bodies, 1)
  }

  shiny::testServer(
    modalServer,
    args = list(id = "geneInfo", title = title_fn, content = content_fn),
    {
      session$setInputs(link = 1)
      gene <<- "gene B"
      session$setInputs(link = 2)
    }
  )

  expect_identical(titles, c("info for gene A", "info for gene B"))
  expect_identical(bodies, c("body for gene A", "body for gene B"))
})

test_that("modalServer accepts a static title and loads default markdown help", {
  expect_silent(
    shiny::testServer(
      modalServer,
      args = list(id = "dendro", title = "Sample clustering dendrogram"),
      {
        session$setInputs(link = 1)
      }
    )
  )
})
