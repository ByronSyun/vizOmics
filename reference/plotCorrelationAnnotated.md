# Plot Correlation Matrix with Annotations

Enhanced version of plotCorrelation that adds row and column
annotations.

## Usage

``` r
plotCorrelationAnnotated(
  mat,
  row_annotation = NULL,
  column_annotation = NULL,
  annotation_colors = NULL,
  ...
)
```

## Arguments

- mat:

  A numeric matrix or data.frame containing correlation values. Can be a
  square correlation matrix or any matrix of numeric values.

- row_annotation:

  Data.frame with row annotations. Row names should match matrix row
  names. Can include categorical or numeric variables.

- column_annotation:

  Data.frame with column annotations. Row names should match matrix
  column names.

- annotation_colors:

  Named list of color mappings for annotations.

- ...:

  Additional arguments passed to ComplexHeatmap::Heatmap().

## Value

A ComplexHeatmap object with annotations

## Examples

``` r
if (FALSE) { # \dontrun{
# With annotations
row_annot <- data.frame(
  celltype = spe$celltype,
  cluster = spe$cluster,
  row.names = colnames(spe)
)
plotCorrelationAnnotated(cor_mat, row_annotation = row_annot)
} # }
```
