# Dashboard extension guide

`create_dashboard()` separates figure production in R from a shared JavaScript
viewer. No per-figure JavaScript renderer is needed. The viewer intentionally
displays the original scientific figure, including facets and reference lines.
Its layout draws on Stock SMART's sidebar and grouped exploration, without using
NOAA branding or presenting model estimates as official stock status.

## Add a figure

1. Implement or reuse a `plot_*()` function that supports the stockplotr RDA
   export convention: `list(figure = ggplot, caption = ..., alt_text = ...)`.
2. Add one entry to `dashboard_specs()` in `R/create_dashboard.R`: function suffix,
   readable title, category, preferred modules, a label-matching expression, and
   any default arguments. Use explicit labels and module choices; never mix
   weight and abundance quantities or invent a reference point.
3. Run `create_dashboard()` against example data containing the figure. Check its
   axes, facets, captions, alternative text and downloads. Add relevant test data
   for new schemas. `dashboard_figures()` and the viewer use the same registry.

The source CSV contains the selected module and reference quantities retained by
the exporter. Do not describe it as the exact plotted-layer data.

## Build on the framework

- Add saved collections, bookmarks, and section descriptions in the viewer.
  Keep reading files through ordinary script and image elements so `file://`
  continues to work. Do not introduce `fetch`, remote fonts or CDN libraries.
- Add a validated multi-model input contract before showing model comparisons.
  Record model identity, units, years and provenance; shared axes need explicit
  compatible-unit checks.
- For point-level tooltips or brushing, introduce one shared chart-data schema
  and renderer. Describe x/y values, series, facets, units, uncertainty and
  reference points explicitly. Preserve the stockplotr image for export. This is
  a separate scientific-data feature, not necessary for offline navigation.
- Version changes to `manifest.json` with `schema_version`. Migrate the viewer
  deliberately and test older manifests or reject them with clear guidance.
- Large ensembles may need precomputed summaries and selective asset generation;
  avoid requiring a server for the standard portable report.

## Verification

Run `devtools::test(filter = "dashboard")` (or `testthat::test_local()`), then build
a report using `example_data`. Extract its ZIP to a path containing spaces and
open **that copy** of `index.html` directly. Verify search and category filters,
comparison selection/removal, previous/next, image enlargement, keyboard focus,
SVG/PNG/CSV downloads, narrow screens, and captions. Check the console and ensure
all image/script/style references are local. No external runtime dependency is
permitted in the report.
