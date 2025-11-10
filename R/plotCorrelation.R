#' Plot Correlation Matrix as Heatmap
#'
#' Creates a heatmap visualization of a correlation or similarity matrix with
#' optional seriation (reordering) to reveal structure.
#'
#' @param mat A numeric matrix or data.frame containing correlation values.
#'   Can be a square correlation matrix or any matrix of numeric values.
#' @param use_seriation Logical indicating whether to reorder rows and columns
#'   using seriation to reveal structure. Default is TRUE.
#' @param seriation_method Character string specifying the seriation method.
#'   Options include "OLO" (Optimal Leaf Ordering), "GW" (Gruvaeus-Wainer),
#'   "Spectral", or any method supported by the seriation package. Default is
#'   "OLO". Ignored if \code{use_seriation = FALSE}.
#' @param show_row_names Logical indicating whether to show row names. 
#'   Default is TRUE.
#' @param show_column_names Logical indicating whether to show column names.
#'   Default is TRUE.
#' @param show_legend Logical indicating whether to show the color legend.
#'   Default is TRUE.
#' @param legend_title Character string for legend title. Default is "Correlation".
#' @param color_palette Character vector of colors for the heatmap, or a
#'   colorRamp2 function. Default is blue-white-red diverging palette.
#' @param row_fontsize Numeric value for row label font size. Default is 10.
#' @param column_fontsize Numeric value for column label font size. Default is 10.
#' @param cluster_rows Logical indicating whether to cluster rows using
#'   hierarchical clustering. Overridden by \code{use_seriation}. Default is FALSE.
#' @param cluster_columns Logical indicating whether to cluster columns using
#'   hierarchical clustering. Overridden by \code{use_seriation}. Default is FALSE.
#' @param row_names_side Character string specifying side for row names:
#'   "left" or "right". Default is "right".
#' @param column_names_side Character string specifying side for column names:
#'   "top" or "bottom". Default is "bottom".
#' @param name Character string for heatmap name (used internally by ComplexHeatmap).
#'   Default is "value".
#' @param ... Additional arguments passed to ComplexHeatmap::Heatmap().
#'
#' @return A ComplexHeatmap object
#'
#' @details
#' This function creates a heatmap visualization using ComplexHeatmap. When
#' \code{use_seriation = TRUE}, the function uses the seriation package to
#' reorder rows and columns to reveal structure in the matrix. This is
#' particularly useful for correlation matrices where related features should
#' appear together.
#'
#' The seriation algorithm tries to place similar rows/columns near each other,
#' making patterns and clusters more visible.
#'
#' @examples
#' \dontrun{
#' # Basic correlation heatmap
#' cor_mat <- cor(t(assay(spe, "logcounts")))
#' plotCorrelation(cor_mat)
#'
#' # Without seriation
#' plotCorrelation(cor_mat, use_seriation = FALSE)
#'
#' # Custom colors
#' plotCorrelation(cor_mat,
#'                 color_palette = c("blue", "white", "red"))
#'
#' # Hide column names for clarity
#' plotCorrelation(cor_mat,
#'                 show_column_names = FALSE,
#'                 row_fontsize = 8)
#'
#' # Cell type co-occurrence matrix
#' cooccur_mat <- calculateCooccurrence(spe)
#' plotCorrelation(cooccur_mat,
#'                 legend_title = "Co-occurrence",
#'                 use_seriation = TRUE)
#' }
#'
#' @import ComplexHeatmap 
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar
#' @importFrom seriation seriate get_order
#' @export
plotCorrelation <- function(mat,
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
                            ...) {
  
  # Convert to matrix if needed
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat)
  }
  
  if (!is.matrix(mat)) {
    stop("Input must be a matrix or data.frame")
  }
  
  # Check for numeric values
  if (!is.numeric(mat)) {
    stop("Matrix must contain numeric values")
  }
  
  # Set up color palette
  if (is.null(color_palette)) {
    # Default diverging palette for correlation values
    mat_range <- range(mat, na.rm = TRUE)
    if (mat_range[1] < 0 && mat_range[2] > 0) {
      # Diverging palette if values span zero
      col_fun <- circlize::colorRamp2(
        c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
        c("blue", "white", "red")
      )
    } else {
      # Sequential palette otherwise
      col_fun <- circlize::colorRamp2(
        c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE)),
        c("white", "red")
      )
    }
  } else if (is.function(color_palette)) {
    col_fun <- color_palette
  } else {
    # User provided colors
    if (length(color_palette) == 3) {
      col_fun <- circlize::colorRamp2(
        c(min(mat, na.rm = TRUE),
          mean(range(mat, na.rm = TRUE)),
          max(mat, na.rm = TRUE)),
        color_palette
      )
    } else {
      col_fun <- circlize::colorRamp2(
        seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE),
            length.out = length(color_palette)),
        color_palette
      )
    }
  }
  
  # Determine row and column order
  if (use_seriation) {
    if (!requireNamespace("seriation", quietly = TRUE)) {
      warning("Package 'seriation' not available. Using default ordering.")
      row_order <- NULL
      column_order <- NULL
    } else {
      tryCatch({
        o <- seriation::seriate(mat, method = seriation_method)
        row_order <- seriation::get_order(o, 1)
        column_order <- seriation::get_order(o, 2)
      }, error = function(e) {
        warning("Seriation failed: ", e$message, ". Using default ordering.")
        row_order <- NULL
        column_order <- NULL
      })
    }
  } else {
    row_order <- NULL
    column_order <- NULL
  }
  
  # Create heatmap
  hm <- ComplexHeatmap::Heatmap(
    mat,
    name = legend_title,
    col = col_fun,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    show_heatmap_legend = show_legend,
    row_order = row_order,
    column_order = column_order,
    cluster_rows = if (!use_seriation) cluster_rows else FALSE,
    cluster_columns = if (!use_seriation) cluster_columns else FALSE,
    row_names_gp = grid::gpar(fontsize = row_fontsize),
    column_names_gp = grid::gpar(fontsize = column_fontsize),
    row_names_side = row_names_side,
    column_names_side = column_names_side,
    ...
  )
  
  return(hm)
}


#' Plot Correlation Matrix with Annotations
#'
#' Enhanced version of plotCorrelation that adds row and column annotations.
#'
#' @inheritParams plotCorrelation
#' @param row_annotation Data.frame with row annotations. Row names should match
#'   matrix row names. Can include categorical or numeric variables.
#' @param column_annotation Data.frame with column annotations. Row names should
#'   match matrix column names.
#' @param annotation_colors Named list of color mappings for annotations.
#'
#' @return A ComplexHeatmap object with annotations
#'
#' @examples
#' \dontrun{
#' # With annotations
#' row_annot <- data.frame(
#'   celltype = spe$celltype,
#'   cluster = spe$cluster,
#'   row.names = colnames(spe)
#' )
#' plotCorrelationAnnotated(cor_mat, row_annotation = row_annot)
#' }
#'
#' @import ComplexHeatmap
#' @export
plotCorrelationAnnotated <- function(mat,
                                     row_annotation = NULL,
                                     column_annotation = NULL,
                                     annotation_colors = NULL,
                                     ...) {
  
  # Create base heatmap
  hm <- plotCorrelation(mat, ...)
  
  # Add row annotations
  if (!is.null(row_annotation)) {
    if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      stop("Package 'ComplexHeatmap' is required for annotations")
    }
    
    # Match row names
    row_annotation <- row_annotation[rownames(mat), , drop = FALSE]
    
    ra <- ComplexHeatmap::rowAnnotation(
      df = row_annotation,
      col = annotation_colors
    )
    hm <- hm + ra
  }
  
  # Add column annotations
  if (!is.null(column_annotation)) {
    # Match column names
    column_annotation <- column_annotation[colnames(mat), , drop = FALSE]
    
    ca <- ComplexHeatmap::columnAnnotation(
      df = column_annotation,
      col = annotation_colors
    )
    hm <- ca %v% hm
  }
  
  return(hm)
}