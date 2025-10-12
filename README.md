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
- ✅ **Reproducible colors**: Fixed color mapping ensures consistency across runs
- ✅ **Multiple palettes**: 5 continuous (MATLAB, viridis, plasma, inferno, magma) and 5 discrete (Set1, Set2, Set3, Dark2, Paired) options
- ✅ **Full control**: Manual override for color types and custom color specifications

See [IMPROVEMENTS.md](IMPROVEMENTS.md) for detailed enhancement documentation.

## Installation

You can install the development version of vizOmics from GitHub:

```r
# install.packages("devtools")
devtools::install_github("ByronSyun/vizOmics")
```

## Quick Start

### Align Clustering Results

```r
library(vizOmics)

# Create example clustering results
clust_ref <- factor(c(rep("A", 50), rep("B", 50), rep("C", 50)))
clust_query <- factor(c(rep("1", 50), rep("2", 50), rep("3", 50)))

# Align query to reference
clust_aligned <- alignClusters(clust_query, clust_ref)

# Check alignment
table(clust_aligned, clust_ref)
```

### Visualize Score Matrix

```r
# Create example score matrix (e.g., PCA results)
scores <- data.frame(
  comp1 = rnorm(100),
  comp2 = rnorm(100),
  comp3 = rnorm(100)
)

# Basic matrix plot
matrixPlot(scores, max_ncomp = 3)

# Color by discrete groups (auto-detected)
groups <- factor(rep(c("A", "B", "C"), length.out = 100))
matrixPlot(scores, max_ncomp = 3, colBy = groups, legendTitle = "Group")

# Color by continuous variable (auto-detected)
expression <- rnorm(100)
matrixPlot(scores, max_ncomp = 3, colBy = expression, 
           color_palette = "viridis", legendTitle = "Expression")

# Integer clusters (auto-detected as discrete)
clusters <- rep(1:3, length.out = 100)
matrixPlot(scores, max_ncomp = 3, colBy = clusters, legendTitle = "Cluster")
```

### Create Sankey Diagram

```r
# Two classifications
class1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
class2 <- sample(c("X", "Y", "Z"), 100, replace = TRUE)
plotSankey(class1, class2)

# Three classifications
class3 <- sample(c("P", "Q", "R"), 100, replace = TRUE)
plotSankey(class1, class2, class3, fontsize = 14)
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
- **Reproducible colors**: Fixed color mapping with sorted factor levels ensures consistency
- **Multiple color palettes**: 
  - Continuous: `matlab` (default), `viridis`, `plasma`, `inferno`, `magma`
  - Discrete: `Set1` (default), `Set2`, `Set3`, `Dark2`, `Paired`
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

## Acknowledgments

Some functions in this package are adapted from the [PhiSpace](https://github.com/JiadongM/PhiSpace) package by Jiadong Mao. We thank the original authors for their excellent work.

## License

MIT License - see [LICENSE.md](LICENSE.md) for details.

## Citation

If you use vizOmics in your research, please cite:

```
Sun, B. (2025). vizOmics: Visualization Tools for Multi-Omics Data Analysis.
R package version 0.1.0. https://github.com/ByronSyun/vizOmics
```

## Issues and Contributions

Please report issues at: https://github.com/ByronSyun/vizOmics/issues

Contributions are welcome! Please feel free to submit pull requests.

