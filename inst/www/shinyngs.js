// Keep plotly charts in step with the active Bootstrap light/dark theme.
//
// The charts are created by many independent modules with their own layouts,
// so rather than thread the current theme through every renderPlotly() this
// restyles them from the client: backgrounds are made transparent (the card
// or page background shows through) and text/axis/grid colours are read from
// the live Bootstrap CSS variables, which flip when the navbar's light/dark
// toggle changes data-bs-theme on <html>.

(function () {
  function themeColors() {
    var s = getComputedStyle(document.body);
    return {
      fg: s.getPropertyValue("--bs-body-color").trim() || "#333333",
      grid: s.getPropertyValue("--bs-border-color").trim() || "#dee2e6"
    };
  }

  function themeOnePlot(gd) {
    if (typeof Plotly === "undefined" || !gd || !gd.classList) return;
    if (!gd.classList.contains("js-plotly-plot")) return;
    var c = themeColors();
    try {
      Plotly.relayout(gd, {
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        "font.color": c.fg,
        "xaxis.color": c.fg,
        "yaxis.color": c.fg,
        "xaxis.gridcolor": c.grid,
        "yaxis.gridcolor": c.grid,
        "legend.font.color": c.fg
      });
    } catch (e) {
      /* a plot mid-render can reject relayout; the next trigger will catch it */
    }
  }

  function themeAllPlots() {
    document.querySelectorAll(".js-plotly-plot").forEach(themeOnePlot);
  }

  // Re-theme when the user toggles light/dark (bslib sets data-bs-theme on <html>).
  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      if (muts[i].attributeName === "data-bs-theme") {
        themeAllPlots();
        return;
      }
    }
  }).observe(document.documentElement, { attributes: true });

  // Re-theme each plot as Shiny (re)renders it. The output element id is
  // e.name; the plotly widget is that element or a descendant of it.
  $(document).on("shiny:value", function (e) {
    setTimeout(function () {
      var el = document.getElementById(e.name);
      if (!el) {
        themeAllPlots();
        return;
      }
      var plot = el.classList.contains("js-plotly-plot") ? el : el.querySelector(".js-plotly-plot");
      if (plot) {
        themeOnePlot(plot);
      }
    }, 50);
  });

  $(document).on("shiny:connected", function () {
    setTimeout(themeAllPlots, 300);
  });
})();
