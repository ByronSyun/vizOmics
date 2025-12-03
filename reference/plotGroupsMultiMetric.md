# Plot Groups with Multiple Metrics

Create a multi-panel plot showing group differences using different
summary metrics (median, mean, max) for comparison.

## Usage

``` r
plotGroupsMultiMetric(
  object,
  group_var,
  features = NULL,
  source = c("reducedDim", "assay", "colData"),
  assay_name = "logcounts",
  reducedDim_name = "PhiSpace",
  top_n = 5,
  metrics = c("median", "mean", "max"),
  ...
)
```

## Arguments

- object:

  A SpatialExperiment, SingleCellExperiment, or list of such objects.
  Can also be a data.frame with numeric features. When using experiment
  objects, data can be extracted from assays or reduced dimensions.

- group_var:

  Character string specifying the grouping variable (e.g., "cluster",
  "condition"). If `object` is a data.frame, this should be a column
  name. If `object` is an experiment object, this can be a column in
  colData.

- features:

  Character vector specifying which features (genes/variables) to plot.
  If NULL (default), uses all features from the specified source.
  Ignored when using reduced dimensions.

- source:

  Character string specifying the data source. Options are "assay" to
  use expression data from assays, "reducedDim" to use dimension
  reduction results (default), or "colData" to use column metadata. When
  source is "assay", you must specify `assay_name`. When source is
  "reducedDim", you must specify `reducedDim_name`. Ignored if `object`
  is a data.frame.

- assay_name:

  Character string specifying which assay to use when
  `source = "assay"`. Common values are "counts", "logcounts", or
  "normcounts". Default is "logcounts".

- reducedDim_name:

  Character string specifying the name of the reduced dimension when
  `source = "reducedDim"`. Default is "PhiSpace".

- top_n:

  Integer specifying how many top features to show per group. Default
  is 5. Set to NULL to show all features.

- metrics:

  Character vector of metrics to display. Default is c("median", "mean",
  "max").

- ...:

  Other input from plotGroups.

## Value

A combined ggplot2 object (requires patchwork package)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare different ranking metrics for cell type scores
plotGroupsMultiMetric(spe, 
                      group_var = "cluster", 
                      top_n = 5)

# Compare metrics for gene expression
plotGroupsMultiMetric(spe,
                      group_var = "cluster",
                      source = "assay",
                      assay_name = "logcounts",
                      features = c("CD3E", "CD8A", "CD4"),
                      top_n = NULL)
} # }
```
