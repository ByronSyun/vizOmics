# Plot Group Differences in Numeric Features

Creates faceted boxplots showing the distribution of numeric features
(e.g., gene expression, cell type scores, pathway scores) across
different groups (e.g., clusters, conditions, spatial regions). The
function automatically selects the top N features per group based on
summary statistics and orders them within each facet.

## Usage

``` r
plotGroups(
  object,
  group_var,
  features = NULL,
  source = c("reducedDim", "assay", "colData"),
  assay_name = "logcounts",
  reducedDim_name = "PhiSpace",
  top_n = 5,
  metric = c("median", "mean", "max"),
  color_scale = c("blue", "white", "red"),
  facet_ncol = 3,
  facet_scales = c("free", "fixed", "free_x", "free_y"),
  title = NULL,
  subtitle = NULL,
  outlier_size = 0.5,
  box_alpha = 0.7,
  show_legend = FALSE,
  group_prefix = "Cluster",
  remove_na = TRUE
)

.extract_phispace_data(object, reducedDim_name, group_var)
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

- metric:

  Character string specifying the metric for ranking features. Options
  are "median" (default), "mean", or "max".

- color_scale:

  Character vector of length 3 specifying colors for the gradient (low,
  mid, high). Default is c("blue", "white", "red").

- facet_ncol:

  Integer specifying number of columns in facet layout. Default is 3.

- facet_scales:

  Character string specifying facet scales. Options are "free"
  (default), "fixed", "free_x", or "free_y".

- title:

  Character string for plot title. Default is auto-generated.

- subtitle:

  Character string for plot subtitle. Default is auto-generated.

- outlier_size:

  Numeric value for outlier point size. Default is 0.5.

- box_alpha:

  Numeric value for boxplot transparency (0-1). Default is 0.7.

- show_legend:

  Logical indicating whether to show the color legend. Default is FALSE.

- group_prefix:

  Character string to prefix group labels in facets. Default is
  "Cluster". Set to "" for no prefix.

- remove_na:

  Logical indicating whether to remove NA values. Default is TRUE.

## Value

A ggplot2 object

## Details

The function performs the following steps:

1.  Extracts numeric features from the specified source (assay,
    reducedDim, or colData)

2.  Assigns observations to groups based on the grouping variable

3.  Calculates summary statistics (median by default) per feature per
    group

4.  Selects top N features per group based on the summary statistic

5.  Creates faceted boxplots with features ordered by their summary
    statistic

6.  Colors boxplots by summary statistic value

## Functions

- `.extract_phispace_data()`: Legacy function name (deprecated)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with reduced dimensions (e.g., PhiSpace scores)
plotGroups(spe, group_var = "cluster")

# Plot gene expression from assays
plotGroups(spe, 
           group_var = "cluster",
           source = "assay",
           assay_name = "logcounts",
           features = c("CD3E", "CD8A", "CD4", "MS4A1", "CD14"),
           top_n = NULL)  # Show all specified genes

# Plot top differentially expressed genes per cluster
plotGroups(spe,
           group_var = "cluster",
           source = "assay",
           assay_name = "logcounts",
           top_n = 10)

# Using a list of objects (e.g., multiple tissue samples)
plotGroups(tissue_list, group_var = "cluster", top_n = 10)

# Custom appearance for cell type scores
plotGroups(spe, 
           group_var = "condition",
           source = "reducedDim",
           reducedDim_name = "PhiSpace",
           top_n = 8,
           color_scale = c("darkblue", "yellow", "darkred"),
           facet_ncol = 2,
           group_prefix = "Condition")

# Show all features without filtering
plotGroups(spe, group_var = "cluster", top_n = NULL)

# Use mean instead of median for ranking
plotGroups(spe, 
           group_var = "cluster",
           source = "assay",
           assay_name = "logcounts",
           metric = "mean")
} # }
```
