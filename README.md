# vizOmics

<!-- badges: start -->
[![R-CMD-check](https://github.com/ByronSyun/vizOmics/workflows/R-CMD-check/badge.svg)](https://github.com/ByronSyun/vizOmics/actions)
<!-- badges: end -->

**vizOmics** is an R package providing visualization and utility functions for multi-omics data analysis. It includes tools for clustering alignment, score matrix visualization, and Sankey diagrams for comparing classification results.

## Features

- **`alignClusters()`**: Align clustering results using optimal assignment (Hungarian algorithm)
- **`matrixPlot()`**: Create comprehensive visualizations of score matrices with **intelligent color detection** and multiple palettes
- **`plotSankey()`**: Generate interactive Sankey diagrams for 2 or 3 classification comparisons

### 🆕 What's New in vizOmics

**Enhanced `matrixPlot()` with Smart Coloring**:
- ✅ **Intelligent color type detection**: Automatically distinguishes between discrete (factors, clusters) and continuous (expression) variables
- ✅ **Ordinal label parsing**: Recognizes patterns like "1 or 2" and converts to 1.5 for proper ordering
- ✅ **Reproducible colors**: Fixed color mapping ensures consistency across runs
- ✅ **Gradient direction control**: `reverse_gradient` parameter for mapping high values to warm colors
- ✅ **Multiple palettes**: 5 continuous (MATLAB, viridis, plasma, inferno, magma) and 5 discrete (Set1, Set2, Set3, Dark2, Paired) options
- ✅ **Full control**: Manual override for color types and custom color specifications

## Installation

You can install the development version of vizOmics from GitHub:

```r
# install.packages("devtools")
devtools::install_github("ByronSyun/vizOmics")
```

## Quick Start

### Example: COVID-19 Multi-Omics Analysis

```r
library(vizOmics)

# Load DIVAS results and metadata
divasRes <- readRDS("divas_results.rds")
metadata <- readRDS("metadata.rds")

# Extract score matrix
scores <- divasRes$sampleScoreMatrix
colnames(scores) <- paste0("comp", 1:ncol(scores))

# WHO Ordinal Scale (disease severity: 1=mild, 7=severe)
wos <- metadata$`Who Ordinal Scale`

# 1. Basic matrix plot
matrixPlot(scores, max_ncomp = 3)

# 2. Color by disease severity
#    matrixPlot auto-detects discrete categories and sorts them
#    Use reverse_gradient=TRUE to map high severity (7) → red
matrixPlot(scores, max_ncomp = 3, colBy = factor(wos), 
           legendTitle = "WHO Ordinal Scale", 
           reverse_gradient = TRUE)

# 3. Find top components correlated with severity
#    Manual conversion needed for correlation analysis
wos_numeric <- as.numeric(gsub(" or ", ".", wos))  # "1 or 2" → 1.5
correlations <- cor(scores, wos_numeric, method = "spearman")
top_comps <- order(abs(correlations), decreasing = TRUE)[1:5]

# Visualize top components
matrixPlot(scores, comp_idx = top_comps, colBy = factor(wos),
           reverse_gradient = TRUE, pointSize = 2)

# 4. Sankey diagram: Timepoint → Severity → Cluster
timepoint <- metadata$timepoint
severity <- cut(wos_numeric, breaks = c(0, 2, 4, 8), 
                labels = c("Mild", "Moderate", "Severe"))
clusters <- kmeans(scores[, 1:3], centers = 3)$cluster

plotSankey(timepoint, severity, factor(clusters), 
           class_names = c("Timepoint", "Severity", "Cluster"))

# 5. Align clustering results
ref_clusters <- metadata$reference_clusters
aligned <- alignClusters(clusters, ref_clusters)
table(aligned, ref_clusters)  # Check alignment
```

## Functions Overview

### `alignClusters()`

Aligns query clustering results to reference clustering using the Hungarian algorithm. Useful for:
- Comparing clustering results from different methods
- Matching cluster labels across different parameter settings
- Evaluating clustering stability

### `matrixPlot()`

Creates comprehensive visualizations for score matrices:
- **1 component**: Density plot with jittered points
- **2 components**: Scatter plot
- **3+ components**: Matrix of density plots (diagonal) and scatter plots (off-diagonal)

**Enhanced Features** (v0.1.0):
- **Intelligent color type detection**: Automatically detects discrete (factors, integer clusters) vs continuous (numeric expression) variables
- **Ordinal label parsing**: Recognizes "1 or 2" patterns and converts to numeric (1.5) for proper ordering
- **Reproducible colors**: Fixed color mapping with sorted factor levels ensures consistency
- **Gradient direction control**: `reverse_gradient = TRUE` to map high values → red, low values → blue
- **Multiple color palettes**: 
  - Continuous: `matlab` (default), `viridis`, `plasma`, `inferno`, `magma`
  - Discrete: `Set1`, `Set2`, `Set3`, `Dark2`, `Paired`
- **Manual control**: Override auto-detection with `colBy_type` parameter
- **Customizable**: Point sizes, transparency, custom colors, legend titles

### `plotSankey()`

Generates interactive Sankey diagrams:
- **2 classifications**: Direct flow visualization
- **3 classifications**: Sequential flow through three levels
- Interactive HTML widget (powered by networkD3)
- Customizable font sizes and node labels

## Dependencies

vizOmics requires the following R packages:
- ggplot2
- cowplot
- gridExtra
- networkD3
- dplyr
- tidyr
- clue
- magrittr
- rlang
- viridisLite (for color palettes)
- grDevices (for color generation)
- utils

## License

MIT License. Copyright (c) 2025 Jiadong Mao and Yinuo Sun. See [LICENSE.md](LICENSE.md) for full details.


