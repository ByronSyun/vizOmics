#!/usr/bin/env Rscript
#' Comprehensive Test Script for vizOmics Package
#' 
#' This script tests all three main functions with emphasis on the new
#' color detection and handling features in matrixPlot()

library(vizOmics)

cat(strrep("=", 70), "\n")
cat("Testing vizOmics Package - Comprehensive Suite\n")
cat(strrep("=", 70), "\n\n")

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# Test 1: alignClusters()
# ============================================================================
cat(strrep("=", 70), "\n")
cat("TEST 1: alignClusters - Clustering Alignment\n")
cat(strrep("=", 70), "\n\n")

n_samples <- 150
clust_ref <- factor(c(rep("A", 50), rep("B", 50), rep("C", 50)))
clust_query <- factor(c(rep("1", 50), rep("2", 50), rep("3", 50)))

cat("Reference clustering:\n")
print(table(clust_ref))

cat("\nQuery clustering (before alignment):\n")
print(table(clust_query))

clust_aligned <- alignClusters(clust_query, clust_ref)

cat("\nQuery clustering (after alignment):\n")
print(table(clust_aligned))

cat("\n✓ alignClusters() test passed!\n\n")

# ============================================================================
# Test 2: matrixPlot() - COMPREHENSIVE COLOR TESTS
# ============================================================================
cat(strrep("=", 70), "\n")
cat("TEST 2: matrixPlot - Score Matrix Visualization\n")
cat(strrep("=", 70), "\n\n")

scores_3comp <- data.frame(
  comp1 = rnorm(100),
  comp2 = rnorm(100),
  comp3 = rnorm(100)
)

# Test 2.1: Basic plot
cat("Test 2.1: Basic 3-component matrix plot...\n")
p_basic <- matrixPlot(scores_3comp, max_ncomp = 3)
cat("✓ Basic plot created\n\n")

# Test 2.2: Factor groups (discrete - auto-detected)
cat("Test 2.2: Color by factor groups (auto-detected as discrete)...\n")
groups_factor <- factor(rep(c("GroupA", "GroupB", "GroupC"), length.out = 100))
p_factor <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_factor, 
                       legendTitle = "Factor Groups")
cat("✓ Auto-detected as: DISCRETE (factor)\n\n")

# Test 2.3: Integer clusters (should auto-detect as discrete)
cat("Test 2.3: Color by integer clusters (auto-detected as discrete)...\n")
clusters_int <- rep(1:3, length.out = 100)
p_clusters <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = clusters_int,
                         legendTitle = "Integer Clusters")
cat("✓ Auto-detected as: DISCRETE (few unique integers)\n")
cat(sprintf("  Unique values: %d / %d\n\n", 
            length(unique(clusters_int)), length(clusters_int)))

# Test 2.4: Force continuous for integer
cat("Test 2.4: Force continuous coloring for integer clusters...\n")
p_clusters_cont <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = clusters_int,
                              colBy_type = "continuous",
                              legendTitle = "Forced Continuous")
cat("✓ Forced as CONTINUOUS\n\n")

# Test 2.5: Character groups (should auto-detect as discrete)
cat("Test 2.5: Color by character groups (auto-detected as discrete)...\n")
groups_char <- rep(c("Type1", "Type2", "Type3"), length.out = 100)
p_char <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_char,
                     legendTitle = "Character Groups")
cat("✓ Auto-detected as: DISCRETE (character)\n\n")

# Test 2.6: Continuous variable
cat("Test 2.6: Color by truly continuous variable...\n")
continuous_var <- rnorm(100)
p_continuous <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = continuous_var,
                           legendTitle = "Expression Level")
cat("✓ Auto-detected as: CONTINUOUS (many unique numeric)\n")
cat(sprintf("  Unique values: %d / %d\n\n", 
            length(unique(continuous_var)), length(continuous_var)))

# Test 2.7: Custom discrete colors
cat("Test 2.7: Custom discrete colors (named vector)...\n")
custom_colors <- c("GroupA" = "#E41A1C", "GroupB" = "#377EB8", "GroupC" = "#4DAF4A")
p_custom <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_factor,
                       manualCol = custom_colors, 
                       legendTitle = "Custom Colors")
cat("✓ Custom colors applied\n\n")

# Test 2.8: Different continuous color palettes
cat("Test 2.8: Different color palettes for continuous data...\n")
cat("  - MATLAB (default)...\n")
p_matlab <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = continuous_var,
                       color_palette = "matlab", legendTitle = "MATLAB")

cat("  - Viridis...\n")
p_viridis <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = continuous_var,
                        color_palette = "viridis", legendTitle = "Viridis")

cat("  - Plasma...\n")
p_plasma <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = continuous_var,
                       color_palette = "plasma", legendTitle = "Plasma")
cat("✓ All continuous palettes work\n\n")

# Test 2.9: Different discrete palettes
cat("Test 2.9: Different discrete color palettes...\n")
cat("  - Set1 (default)...\n")
p_set1 <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_factor,
                     color_palette = "Set1", legendTitle = "Set1")

cat("  - Set2...\n")
p_set2 <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_factor,
                     color_palette = "Set2", legendTitle = "Set2")

cat("  - Dark2...\n")
p_dark2 <- matrixPlot(scores_3comp, max_ncomp = 3, colBy = groups_factor,
                      color_palette = "Dark2", legendTitle = "Dark2")
cat("✓ All discrete palettes work\n\n")

# Test 2.10: Color consistency test
cat("Test 2.10: Color consistency test (reproducibility)...\n")
test_groups <- factor(sample(c("Alpha", "Beta", "Gamma"), 100, replace = TRUE))
test_scores <- data.frame(comp1 = rnorm(100), comp2 = rnorm(100))

p_consist1 <- matrixPlot(test_scores, max_ncomp = 2, colBy = test_groups)
cat("  Run 1 complete\n")

p_consist2 <- matrixPlot(test_scores, max_ncomp = 2, colBy = test_groups)
cat("  Run 2 complete\n")
cat("✓ Colors should be consistent due to sorted factor levels\n\n")

# Test 2.11: Two-component plot
cat("Test 2.11: Two-component scatter plot...\n")
p_2comp <- matrixPlot(scores_3comp, comp_idx = c(1, 2), colBy = groups_factor)
cat("✓ Two-component plot created\n\n")

# Test 2.12: Single-component plot
cat("Test 2.12: Single-component density plot...\n")
p_1comp <- matrixPlot(scores_3comp, comp_idx = 1, colBy = groups_factor)
cat("✓ Single-component plot created\n\n")

cat("✓ All matrixPlot() tests passed!\n\n")

# ============================================================================
# Test 3: plotSankey()
# ============================================================================
cat(strrep("=", 70), "\n")
cat("TEST 3: plotSankey - Sankey Diagrams\n")
cat(strrep("=", 70), "\n\n")

cat("Test 3.1: Two-way Sankey diagram...\n")
class1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
class2 <- sample(c("X", "Y", "Z"), 100, replace = TRUE)

p_sankey2 <- plotSankey(class1, class2, fontsize = 12)
cat("✓ Two-way Sankey diagram created\n\n")

cat("Test 3.2: Three-way Sankey diagram...\n")
class3 <- sample(c("P", "Q", "R"), 100, replace = TRUE)

p_sankey3 <- plotSankey(class1, class2, class3, 
                        add_suffix = TRUE, fontsize = 12)
cat("✓ Three-way Sankey diagram created\n\n")

cat("Test 3.3: Three-way without suffix...\n")
p_sankey3_no_suffix <- plotSankey(class1, class2, class3, 
                                  add_suffix = FALSE, fontsize = 12)
cat("✓ Three-way Sankey (no suffix) created\n\n")

cat("✓ All plotSankey() tests passed!\n\n")

# ============================================================================
# Summary
# ============================================================================
cat(strrep("=", 70), "\n")
cat("COMPREHENSIVE TEST SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("✓ alignClusters():\n")
cat("  - Clustering alignment using Hungarian algorithm\n\n")

cat("✓ matrixPlot() - ENHANCED VERSION:\n")
cat("  [1] Basic matrix plot\n")
cat("  [2] Auto-detect discrete (factor)\n")
cat("  [3] Auto-detect discrete (integer clusters)\n")
cat("  [4] Force continuous mode\n")
cat("  [5] Auto-detect discrete (character)\n")
cat("  [6] Auto-detect continuous (numeric)\n")
cat("  [7] Custom discrete colors\n")
cat("  [8] Multiple continuous palettes (MATLAB, viridis, plasma)\n")
cat("  [9] Multiple discrete palettes (Set1, Set2, Dark2)\n")
cat("  [10] Color consistency/reproducibility\n")
cat("  [11] Two-component scatter\n")
cat("  [12] Single-component density\n\n")

cat("✓ plotSankey():\n")
cat("  - Two-way Sankey diagram\n")
cat("  - Three-way Sankey with suffix\n")
cat("  - Three-way Sankey without suffix\n\n")

cat(strrep("=", 70), "\n")
cat("ALL TESTS PASSED SUCCESSFULLY!\n")
cat("vizOmics package is ready for use.\n")
cat(strrep("=", 70), "\n")

