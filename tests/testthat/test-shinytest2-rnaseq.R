# The full rnaseq app

test_that("the rnaseq app boots and shows its home tab", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-boot")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)

  expect_match(app$get_text("h3.shinyngs-eyebrow"), "Jump to analysis")
})

test_that("the page loader spans one navigation cycle", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-page-loader")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    "document.getElementById('shinyngs-page-loader').classList.contains('shinyngs-page-loader--hidden')",
    timeout = 20000
  )
  app$run_js("(function(){
    var loader = document.getElementById('shinyngs-page-loader');
    window.__shinyngsLoaderTransitions = [];
    window.__shinyngsVisibleProgressSeen = false;
    var loaderVisible = !loader.classList.contains('shinyngs-page-loader--hidden');
    new MutationObserver(function(){
      var next = !loader.classList.contains('shinyngs-page-loader--hidden');
      if (next !== loaderVisible) {
        window.__shinyngsLoaderTransitions.push(next ? 'show' : 'hide');
        loaderVisible = next;
      }
    }).observe(loader, {attributes: true, attributeFilter: ['class']});
    new MutationObserver(function(){
      var notifications = Array.from(document.querySelectorAll('.shiny-progress-notification'));
      if (notifications.some(function(progress){
        var notification = progress.closest('.shiny-notification');
        return notification && getComputedStyle(notification).display !== 'none';
      })) window.__shinyngsVisibleProgressSeen = true;
    }).observe(document.body, {childList: true, subtree: true});
    document.querySelector('.navbar a[data-value=\"Annotation\"]').click();
  })();")
  app$wait_for_js(
    "!!document.querySelector('#rnaseq-rowmetatable-rowmetatable-datatable table') && document.getElementById('shinyngs-page-loader').classList.contains('shinyngs-page-loader--hidden')",
    timeout = 20000
  )

  Sys.sleep(1)
  expect_equal(unlist(app$get_js("window.__shinyngsLoaderTransitions")), c("show", "hide"))
  expect_false(app$get_js("window.__shinyngsVisibleProgressSeen"))
})

test_that("top-level panels deliver each expensive output once", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-panel-render-counts")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$run_js("(function(){
    window.__shinyngsPanelOutputCounts = {};
    window.__shinyngsPanelLoaderTransitions = [];
    $(document).off('.shinyngsPanelRenderCounts');
    $(document).on('shiny:value.shinyngsPanelRenderCounts', function(event){
      var output = event.target;
      if (!output || !output.id) return;
      var classes = output.className || '';
      if (!/plotly|datatables|shiny-plot-output|shiny-image-output|html-widget-output/.test(classes)) return;
      var counts = window.__shinyngsPanelOutputCounts;
      counts[output.id] = (counts[output.id] || 0) + 1;
    });
    var loader = document.getElementById('shinyngs-page-loader');
    var loaderVisible = !loader.classList.contains('shinyngs-page-loader--hidden');
    new MutationObserver(function(){
      var next = !loader.classList.contains('shinyngs-page-loader--hidden');
      if (next !== loaderVisible) {
        window.__shinyngsPanelLoaderTransitions.push(next ? 'show' : 'hide');
        loaderVisible = next;
      }
    }).observe(loader, {attributes: true, attributeFilter: ['class']});
  })();")

  targets <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('.navbar a[data-value]'))
      .filter(function(link){
        return link.dataset.value && link.dataset.value !== 'Home' &&
          !link.matches('[data-bs-toggle=\"dropdown\"], [data-toggle=\"dropdown\"]');
      })
      .map(function(link){ return link.dataset.value; })"
  ))

  anomalies <- character()
  loader_anomalies <- character()
  rendered_outputs <- character()
  for (target in targets) {
    app$run_js("Array.from(document.querySelectorAll('.navbar a[data-value]'))
      .find(function(link){ return link.dataset.value === 'Home'; }).click();")
    app$wait_for_idle(timeout = 20000)
    app$wait_for_js(
      "document.getElementById('shinyngs-page-loader').classList.contains('shinyngs-page-loader--hidden')",
      timeout = 20000
    )
    app$run_js("window.__shinyngsPanelOutputCounts = {};
      window.__shinyngsPanelLoaderTransitions = [];")
    app$run_js(sprintf(
      "(function(){
        var target = %s;
        Array.from(document.querySelectorAll('.navbar a[data-value]'))
          .find(function(link){ return link.dataset.value === target; }).click();
      })();",
      jsonlite::toJSON(target, auto_unbox = TRUE)
    ))
    app$wait_for_idle(timeout = 20000)
    Sys.sleep(1)

    counts <- unlist(app$get_js("window.__shinyngsPanelOutputCounts"))
    rendered_outputs <- union(rendered_outputs, names(counts))
    if (any(counts != 1)) {
      duplicated <- names(counts)[counts != 1]
      anomalies <- c(anomalies, paste(target, duplicated, counts[duplicated], sep = ": "))
    }
    loader_transitions <- unlist(app$get_js("window.__shinyngsPanelLoaderTransitions"))
    if (!identical(loader_transitions, c("show", "hide"))) {
      loader_anomalies <- c(loader_anomalies, paste(target, paste(loader_transitions, collapse = ", "), sep = ": "))
    }
  }

  expect_equal(anomalies, character())
  expect_equal(loader_anomalies, character())
  expect_gte(length(rendered_outputs), 10)
})

# pca module (exercises selectmatrix internally)

test_that("the PCA tab renders a scatterplot and its selectmatrix controls", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-pca")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "pca")
  app$wait_for_idle(timeout = 20000)

  expect_equal(app$get_value(input = "rnaseq-pca-pca-threedee"), "TRUE")

  outputs <- names(app$get_values()$output)
  expect_true("rnaseq-pca-pca-scatter" %in% outputs)
  expect_true("rnaseq-pca-components-datatable" %in% outputs)
  expect_true("rnaseq-pca-pca-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); assert the scatter actually plots one
  # point per sample rather than just checking the output registered.
  scatter <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-pca-pca-scatter"),
    simplifyVector = FALSE
  )
  traces <- scatter$x$data
  expect_gt(length(traces), 0)

  point_traces <- Filter(function(tr) identical(tr$mode, "markers") && length(tr$text) > 0, traces)
  expect_gt(length(point_traces), 0)
  sample_labels <- unlist(lapply(point_traces, function(tr) unlist(tr$text, use.names = FALSE)), use.names = FALSE)
  n_points <- sum(vapply(point_traces, function(tr) length(tr$x), integer(1)))
  expect_setequal(sample_labels, paste0("sample", seq_len(12)))
  expect_length(sample_labels, 12)
  expect_equal(n_points, 12)
  expect_equal(sum(vapply(point_traces, function(tr) length(tr$y), integer(1))), 12)

  # The components datatable lists one row per sample.
  components <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-pca-components-datatable"),
    simplifyVector = FALSE
  )
  expect_equal(length(components$x$data[[1]]), 12)
})

test_that("PCA dynamic controls deliver one final render without restarting the page loader", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-pca-single-render")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "pca")
  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    "document.getElementById('shinyngs-page-loader').classList.contains('shinyngs-page-loader--hidden')",
    timeout = 20000
  )
  app$run_js("(function(){
    var outputName = 'rnaseq-pca-pca-scatter';
    var loader = document.getElementById('shinyngs-page-loader');
    window.__shinyngsRenderCount = 0;
    window.__shinyngsRenderErrors = [];
    window.__shinyngsInteractionLoaderTransitions = [];
    window.__resetShinyngsRenderTrace = function(){
      window.__shinyngsRenderCount = 0;
      window.__shinyngsRenderErrors = [];
    };
    $(document).off('.shinyngsSingleRender');
    $(document).on('shiny:value.shinyngsSingleRender', function(event){
      if (event.name === outputName) window.__shinyngsRenderCount += 1;
    });
    $(document).on('shiny:error.shinyngsSingleRender', function(event){
      var message = event.error && event.error.message;
      if (message) window.__shinyngsRenderErrors.push(event.name + ': ' + message);
    });
    var loaderVisible = !loader.classList.contains('shinyngs-page-loader--hidden');
    new MutationObserver(function(){
      var next = !loader.classList.contains('shinyngs-page-loader--hidden');
      if (next !== loaderVisible) {
        window.__shinyngsInteractionLoaderTransitions.push(next ? 'show' : 'hide');
        loaderVisible = next;
      }
    }).observe(loader, {attributes: true, attributeFilter: ['class']});
  })();")

  app$run_js("(function(){
    window.__resetShinyngsRenderTrace();
    var slider = document.getElementById('rnaseq-pca-pca-pointSize');
    slider.dispatchEvent(new PointerEvent('pointerdown', {bubbles: true}));
    Shiny.setInputValue('rnaseq-pca-pca-pointSize', 8, {priority: 'event'});
  })();")
  app$wait_for_js("window.__shinyngsRenderCount >= 1", timeout = 20000)
  app$wait_for_idle(timeout = 20000)
  Sys.sleep(1)

  expect_equal(app$get_js("window.__shinyngsRenderCount"), 1)
  expect_length(unlist(app$get_js("window.__shinyngsRenderErrors")), 0)
  expect_length(unlist(app$get_js("window.__shinyngsInteractionLoaderTransitions")), 0)

  app$set_inputs(`rnaseq-pca-pca-zAxis` = "4")
  app$set_inputs(`rnaseq-pca-pca-threedee` = "FALSE")
  app$wait_for_idle(timeout = 20000)
  app$run_js("window.__resetShinyngsRenderTrace();")
  app$set_inputs(`rnaseq-pca-pca-threedee` = "TRUE", wait_ = FALSE)
  app$wait_for_js("window.__shinyngsRenderCount >= 1", timeout = 20000)
  app$wait_for_idle(timeout = 20000)
  Sys.sleep(1)

  expect_equal(app$get_js("window.__shinyngsRenderCount"), 1)
  expect_length(unlist(app$get_js("window.__shinyngsRenderErrors")), 0)
  expect_equal(app$get_value(input = "rnaseq-pca-pca-zAxis"), "3")

  app$run_js("window.__resetShinyngsRenderTrace();")
  app$set_inputs(`rnaseq-pca-pca-selectmatrix-sampleGroupVar` = "batch", wait_ = FALSE)
  app$wait_for_js("window.__shinyngsRenderCount >= 1", timeout = 20000)
  app$wait_for_idle(timeout = 20000)
  Sys.sleep(1)

  expect_equal(app$get_js("window.__shinyngsRenderCount"), 1)
  expect_length(unlist(app$get_js("window.__shinyngsRenderErrors")), 0)
  expect_setequal(
    app$get_value(input = "rnaseq-pca-pca-selectmatrix-sampleGroupVal"),
    c("batch1", "batch2")
  )

  app$run_js("(function(){
    window.__shinyngsInteractionLoaderTransitions = [];
    var activeNestedTab = Array.from(document.querySelectorAll('.nav-tabs a.active')).find(function(tab){
      return !tab.closest('.navbar');
    });
    activeNestedTab.click();
  })();")
  Sys.sleep(1)
  expect_length(unlist(app$get_js("window.__shinyngsInteractionLoaderTransitions")), 0)
})

# heatmap module

test_that("the Clustering Heatmap tab renders an interactive heatmap", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-heatmap")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "Clustering Heatmap")
  app$wait_for_idle(timeout = 20000)

  outputs <- names(app$get_values()$output)
  expect_true("rnaseq-heatmap-clustering-interactive_heatmap" %in% outputs)
  expect_true("rnaseq-heatmap-clustering-heatmap-selectmatrix-geneSelect_ui" %in% outputs)

  # 12 samples in shinytest2_eselist(); the clustering heatmap's main trace is
  # a 12x12 sample-by-sample matrix, so assert that shape rather than just
  # checking the output registered.
  widget <- jsonlite::fromJSON(
    app$get_value(output = "rnaseq-heatmap-clustering-interactive_heatmap"),
    simplifyVector = FALSE
  )
  heatmap_traces <- Filter(function(tr) identical(tr$type, "heatmap"), widget$x$data)
  expect_gt(length(heatmap_traces), 0)

  # The main sample-by-sample trace is picked by row count, same as
  # splitAnnotationLegend() (R/heatmap.R) does to tell it apart from any
  # annotation-strip trace of the same type.
  row_counts <- vapply(heatmap_traces, function(tr) length(tr$z), integer(1))
  expect_equal(sum(row_counts == 12), 1)
  main_trace <- heatmap_traces[[which.max(row_counts)]]
  expect_equal(length(main_trace$x), 12)
  expect_equal(length(main_trace$y), 12)
})

test_that("the Gene info tab defaults single-contrast differential effects to the table", {
  skip_on_cran()

  app <- shinytest2_app_driver("rnaseq", "rnaseq-gene-contrast-profile")
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-rnaseq` = "geneinfo")
  app$wait_for_idle(timeout = 20000)
  app$set_inputs(`rnaseq-gene-gene_label-label` = "Gene1", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  tab_labels <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('#rnaseq-gene-differentialEffects_ui .nav-link')).map(e => e.textContent.trim())"
  ))

  expect_equal(tab_labels, "Table")
  expect_true(app$get_js("!!document.querySelector('#rnaseq-gene-geneContrastsTable-datatable table')"))
  expect_false(app$get_js("!!document.querySelector('#rnaseq-gene-geneContrastProfile')"))
})

# URL bookmarking round-trip is covered separately in
# test-shinytest2-bookmark.R (its own file, so its 40s timeouts and separate
# on-disk-app process don't make this file the parallel-worker bottleneck).
