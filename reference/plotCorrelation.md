# Plot Correlation Matrix as Heatmap

Creates a heatmap visualization of a correlation or similarity matrix
with optional seriation (reordering) to reveal structure.

## Usage

``` r
plotCorrelation(
  mat,
  use_seriation = TRUE,
  seriation_method = "OLO",
  show_row_names = TRUE,
  show_column_names = TRUE,
  show_legend = TRUE,
  legend_title = "Correlation",
  color_palette = NULL,
  row_fontsize = 10,
  column_fontsize = 10,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_names_side = "right",
  column_names_side = "bottom",
  name = "value",
  ...
)
```

## Arguments

- mat:

  A numeric matrix or data.frame containing correlation values. Can be a
  square correlation matrix or any matrix of numeric values.

- use_seriation:

  Logical indicating whether to reorder rows and columns using seriation
  to reveal structure. Default is TRUE.

- seriation_method:

  Character string specifying the seriation method. Options include
  "OLO" (Optimal Leaf Ordering), "GW" (Gruvaeus-Wainer), "Spectral", or
  any method supported by the seriation package. Default is "OLO".
  Ignored if `use_seriation = FALSE`.

- show_row_names:

  Logical indicating whether to show row names. Default is TRUE.

- show_column_names:

  Logical indicating whether to show column names. Default is TRUE.

- show_legend:

  Logical indicating whether to show the color legend. Default is TRUE.

- legend_title:

  Character string for legend title. Default is "Correlation".

- color_palette:

  Character vector of colors for the heatmap, or a colorRamp2 function.
  Default is blue-white-red diverging palette.

- row_fontsize:

  Numeric value for row label font size. Default is 10.

- column_fontsize:

  Numeric value for column label font size. Default is 10.

- cluster_rows:

  Logical indicating whether to cluster rows using hierarchical
  clustering. Overridden by `use_seriation`. Default is FALSE.

- cluster_columns:

  Logical indicating whether to cluster columns using hierarchical
  clustering. Overridden by `use_seriation`. Default is FALSE.

- row_names_side:

  Character string specifying side for row names: "left" or "right".
  Default is "right".

- column_names_side:

  Character string specifying side for column names: "top" or "bottom".
  Default is "bottom".

- name:

  Character string for heatmap name (used internally by ComplexHeatmap).
  Default is "value".

- ...:

  Additional arguments passed to ComplexHeatmap::Heatmap().

## Value

A ComplexHeatmap object

## Details

This function creates a heatmap visualization using ComplexHeatmap. When
`use_seriation = TRUE`, the function uses the seriation package to
reorder rows and columns to reveal structure in the matrix. This is
particularly useful for correlation matrices where related features
should appear together.

The seriation algorithm tries to place similar rows/columns near each
other, making patterns and clusters more visible.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic correlation heatmap
cor_mat <- cor(t(assay(spe, "logcounts")))
plotCorrelation(cor_mat)

# Without seriation
plotCorrelation(cor_mat, use_seriation = FALSE)

# Custom colors
plotCorrelation(cor_mat,
                color_palette = c("blue", "white", "red"))

# Hide column names for clarity
plotCorrelation(cor_mat,
                show_column_names = FALSE,
                row_fontsize = 8)

# Cell type co-occurrence matrix
cooccur_mat <- calculateCooccurrence(spe)
plotCorrelation(cooccur_mat,
                legend_title = "Co-occurrence",
                use_seriation = TRUE)
} # }
```
