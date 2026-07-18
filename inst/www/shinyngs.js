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

  // Structural marks (dendrogram branches, box outlines, upset grid lines and
  // bars) are drawn in a literal black by the plotting modules; they vanish on
  // the dark theme. The data palette is colour-blind-safe and contains no pure
  // black, so flipping only exactly-black line/marker/text colours to the body
  // colour re-themes the chrome-like marks without touching any data colour.
  function isBlack(c) {
    if (typeof c !== "string") return false;
    var v = c.trim().toLowerCase().replace(/\s+/g, "");
    return v === "black" || v === "#000" || v === "#000000" || v === "rgb(0,0,0)";
  }

  // Flipping a structural mark to the body colour makes it no longer black, so
  // its indices are remembered per plot: a later theme toggle re-themes those
  // traces even though isBlack() no longer matches. `rebuild` (a fresh Shiny
  // render, which redraws the marks black again) discards the remembered set so
  // stale indices can't outlive a re-render. Restyles are batched per property
  // so a plot with many black traces (e.g. a per-sample boxplot) redraws once.
  function themeMarks(gd, fg, rebuild) {
    if (!gd.data) return;
    if (rebuild || !gd._shinyngsMarks) {
      gd._shinyngsMarks = { line: [], marker: [], text: [] };
    }
    var rec = gd._shinyngsMarks;
    gd.data.forEach(function (tr, i) {
      if (tr.line && isBlack(tr.line.color) && rec.line.indexOf(i) < 0) rec.line.push(i);
      if (tr.marker && isBlack(tr.marker.color) && rec.marker.indexOf(i) < 0) rec.marker.push(i);
      if (tr.textfont && isBlack(tr.textfont.color) && rec.text.indexOf(i) < 0) rec.text.push(i);
    });
    try {
      if (rec.line.length) Plotly.restyle(gd, { "line.color": fg }, rec.line);
      if (rec.marker.length) Plotly.restyle(gd, { "marker.color": fg }, rec.marker);
      if (rec.text.length) Plotly.restyle(gd, { "textfont.color": fg }, rec.text);
    } catch (e) {
      /* mid-render restyle can reject; the next trigger will catch it */
    }
  }

  function themeOnePlot(gd, rebuild) {
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
    themeMarks(gd, c.fg, rebuild);
  }

  function themeAllPlots(rebuild) {
    document.querySelectorAll(".js-plotly-plot").forEach(function (gd) {
      themeOnePlot(gd, rebuild);
    });
  }

  // Re-theme when the user toggles light/dark (bslib sets data-bs-theme on <html>).
  new MutationObserver(function () {
    themeAllPlots(false);
  }).observe(document.documentElement, { attributes: true, attributeFilter: ["data-bs-theme"] });

  // Re-theme each plot as Shiny (re)renders it. The output element id is
  // e.name; the plotly widget is that element or a descendant of it.
  $(document).on("shiny:value", function (e) {
    setTimeout(function () {
      var el = document.getElementById(e.name);
      if (!el) {
        themeAllPlots(false);
        return;
      }
      var plot = el.classList.contains("js-plotly-plot") ? el : el.querySelector(".js-plotly-plot");
      if (plot) {
        themeOnePlot(plot, true);
      }
    }, 50);
  });

  $(document).on("shiny:connected", function () {
    setTimeout(function () {
      themeAllPlots(false);
    }, 300);
  });

  // The URL is generated server-side, so it arrives as a message; the "Share
  // view" click is recent enough to still count as a user gesture for the
  // clipboard write.
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("shinyngs_copy_link", function (url) {
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(url).catch(function () {});
      }
    });
  }

  // Plot download format toggle: a plain icon button (styled like the
  // light/dark toggle) that flips between PNG and SVG. R never binds to this
  // element directly; it just reads the resulting Shiny input value once, via
  // session$userData in configureBookmarking(), so every renderPlotly() picks
  // it up without taking it as an extra module argument.
  var PLOT_FORMAT_ICON_CLASS = { png: "far fa-file-image", svg: "fas fa-bezier-curve" };

  function setPlotFormatButtonState(btn, format) {
    btn.setAttribute("data-format", format);
    var i = btn.querySelector("i");
    if (i) i.className = PLOT_FORMAT_ICON_CLASS[format];
    var next = format === "png" ? "SVG" : "PNG";
    btn.setAttribute("aria-label", "Plot download format: " + format.toUpperCase() + ". Click to switch to " + next + ".");
  }

  $(document).on("click", "#shinyngs_plot_format_toggle", function () {
    var btn = this;
    var next = btn.getAttribute("data-format") === "png" ? "svg" : "png";
    setPlotFormatButtonState(btn, next);
    if (window.Shiny) Shiny.setInputValue("shinyngs_plot_format", next, { priority: "event" });
  });
})();
