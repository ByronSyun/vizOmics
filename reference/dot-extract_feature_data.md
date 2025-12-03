# Extract Feature Data from Various Object Types and Sources

Internal helper function to extract numeric feature data from different
object types and data sources (assays, reduced dimensions, or column
metadata).

## Usage

``` r
.extract_feature_data(
  object,
  source,
  assay_name,
  reducedDim_name,
  features,
  group_var
)
```

## Arguments

- object:

  Input object (SpatialExperiment, SingleCellExperiment, list, or
  data.frame)

- source:

  Data source: "assay", "reducedDim", or "colData"

- assay_name:

  Name of the assay (for source = "assay")

- reducedDim_name:

  Name of the reduced dimension (for source = "reducedDim")

- features:

  Character vector of features to extract (NULL = all)

- group_var:

  Name of the grouping variable

## Value

A data.frame with feature values and group variable
