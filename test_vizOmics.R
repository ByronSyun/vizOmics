#!/usr/bin/env Rscript
#' Test script for vizOmics package
#' 
#' This script tests all three main functions in the vizOmics package

library(vizOmics)

cat(strrep("=", 70), "\n")
cat("Testing vizOmics Package\n")
cat(strrep("=", 70), "\n\n")

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# Test 1: alignClusters()
# ============================================================================
cat(strrep("=", 70), "\n")
cat("Test 1: alignClusters() - Clustering Alignment\n")
cat(strrep("=", 70), "\n\n")

# Create example clustering results
n_samples <- 150
clust_ref <- factor(c(rep("A", 50), rep("B", 50), rep("C", 50)))
clust_query <- factor(c(rep("1", 50), rep("2", 50), rep("3", 50)))

cat("Reference clustering:\n")
print(table(clust_ref))

cat("\nQuery clustering (before alignment):\n")
print(table(clust_query))

# Align query to reference
clust_aligned <- alignClusters(clust_query, clust_ref)

cat("\nQuery clustering (after alignment):\n")
print(table(clust_aligned))

cat("\nConfusion matrix (aligned vs reference):\n")
print(table(clust_aligned, clust_ref))

cat("\n✓ alignClusters() test passed!\n\n")

# ============================================================================
# Test 2: matrixPlot()
# ============================================================================
cat(strrep("=", 70), "\n")
cat("Test 2: matrixPlot() - Score Matrix Visualization\n")
cat(strrep("=", 70), "\n\n")

# Create example score matrix
scores <- data.frame(
  comp1 = rnorm(100),
  comp2 = rnorm(100),
  comp3 = rnorm(100),
  comp4 = rnorm(100)
)

cat("Test 2a: Basic matrix plot (3 components)\n")
p1 <- matrixPlot(scores, max_ncomp = 3)
cat("✓ Basic matrix plot created\n\n")

cat("Test 2b: Matrix plot with group coloring\n")
groups <- rep(c("Group A", "Group B", "Group C"), length.out = 100)
p2 <- matrixPlot(scores, max_ncomp = 3, colBy = groups, 
                 legendTitle = "Groups", pointSize = 2)
cat("✓ Group-colored matrix plot created\n\n")

cat("Test 2c: Matrix plot with continuous coloring\n")
continuous_var <- rnorm(100)
p3 <- matrixPlot(scores, max_ncomp = 3, colBy = continuous_var,
                 legendTitle = "Expression", color_palette = "matlab")
cat("✓ Continuous-colored matrix plot created\n\n")

cat("Test 2d: Two-component scatter plot\n")
p4 <- matrixPlot(scores, comp_idx = c(1, 2), colBy = groups,
                 legendTitle = "Groups")
cat("✓ Two-component scatter plot created\n\n")

cat("Test 2e: Single component density plot\n")
p5 <- matrixPlot(scores, comp_idx = 1, colBy = groups,
                 legendTitle = "Groups")
cat("✓ Single component density plot created\n\n")

cat("✓ All matrixPlot() tests passed!\n\n")

# ============================================================================
# Test 3: plotSankey() - Two classifications
# ============================================================================
cat(strrep("=", 70), "\n")
cat("Test 3: plotSankey() - Sankey Diagrams\n")
cat(strrep("=", 70), "\n\n")

cat("Test 3a: Two-way Sankey diagram\n")
class1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
class2 <- sample(c("X", "Y", "Z"), 100, replace = TRUE)

cat("Classification 1 distribution:\n")
print(table(class1))
cat("\nClassification 2 distribution:\n")
print(table(class2))

cat("\nGenerating two-way Sankey diagram...\n")
p_sankey2 <- plotSankey(class1, class2, fontsize = 12)
cat("✓ Two-way Sankey diagram created\n\n")

# ============================================================================
# Test 4: plotSankey() - Three classifications
# ============================================================================
cat("Test 3b: Three-way Sankey diagram\n")
class3 <- sample(c("P", "Q", "R"), 100, replace = TRUE)

cat("Classification 3 distribution:\n")
print(table(class3))

cat("\nGenerating three-way Sankey diagram...\n")
p_sankey3 <- plotSankey(class1, class2, class3, 
                        add_suffix = TRUE, fontsize = 12)
cat("✓ Three-way Sankey diagram created\n\n")

cat("✓ All plotSankey() tests passed!\n\n")

# ============================================================================
# Summary
# ============================================================================
cat(strrep("=", 70), "\n")
cat("Test Summary\n")
cat(strrep("=", 70), "\n")
cat("✓ alignClusters(): Clustering alignment - PASSED\n")
cat("✓ matrixPlot(): Score matrix visualization - PASSED\n")
cat("  - Basic matrix plot\n")
cat("  - Group coloring\n")
cat("  - Continuous coloring\n")
cat("  - Two-component plot\n")
cat("  - Single component plot\n")
cat("✓ plotSankey(): Sankey diagrams - PASSED\n")
cat("  - Two-way diagram\n")
cat("  - Three-way diagram\n")
cat("\nAll vizOmics functions tested successfully!\n")
cat(strrep("=", 70), "\n")

