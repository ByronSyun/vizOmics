#' Plot Group Differences in Numeric Features
#'
#' Creates faceted boxplots showing the distribution of numeric features (e.g., 
#' gene expression, cell type scores, pathway scores) across different groups 
#' (e.g., clusters, conditions, spatial regions). The function automatically 
#' selects the top N features per group based on summary statistics and orders 
#' them within each facet.
#'
#' @param object A SpatialExperiment, SingleCellExperiment, or list of such objects.
#'   Can also be a data.frame with numeric features. When using experiment objects,
#'   data can be extracted from assays or reduced dimensions.
#' @param group_var Character string specifying the grouping variable (e.g., "cluster", 
#'   "condition"). If \code{object} is a data.frame, this should be a column name. 
#'   If \code{object} is an experiment object, this can be a column in colData.
#' @param features Character vector specifying which features (genes/variables) to 
#'   plot. If NULL (default), uses all features from the specified source. Ignored 
#'   when using reduced dimensions.
#' @param source Character string specifying the data source. Options are "assay" 
#'   to use expression data from assays, "reducedDim" to use dimension reduction 
#'   results (default), or "colData" to use column metadata. When source is "assay", 
#'   you must specify \code{assay_name}. When source is "reducedDim", you must 
#'   specify \code{reducedDim_name}. Ignored if \code{object} is a data.frame.
#' @param assay_name Character string specifying which assay to use when 
#'   \code{source = "assay"}. Common values are "counts", "logcounts", or 
#'   "normcounts". Default is "logcounts".
#' @param reducedDim_name Character string specifying the name of the reduced 
#'   dimension when \code{source = "reducedDim"}. Default is "PhiSpace".
#' @param top_n Integer specifying how many top features to show per group. 
#'   Default is 5. Set to NULL to show all features.
#' @param metric Character string specifying the metric for ranking features. 
#'   Options are "median" (default), "mean", or "max".
#' @param color_scale Character vector of length 3 specifying colors for the 
#'   gradient (low, mid, high). Default is c("blue", "white", "red").
#' @param facet_ncol Integer specifying number of columns in facet layout. 
#'   Default is 3.
#' @param facet_scales Character string specifying facet scales. Options are 
#'   "free" (default), "fixed", "free_x", or "free_y".
#' @param title Character string for plot title. Default is auto-generated.
#' @param subtitle Character string for plot subtitle. Default is auto-generated.
#' @param outlier_size Numeric value for outlier point size. Default is 0.5.
#' @param box_alpha Numeric value for boxplot transparency (0-1). Default is 0.7.
#' @param show_legend Logical indicating whether to show the color legend. 
#'   Default is FALSE.
#' @param group_prefix Character string to prefix group labels in facets. 
#'   Default is "Cluster". Set to "" for no prefix.
#' @param remove_na Logical indicating whether to remove NA values. Default is TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts numeric features from the specified source (assay, reducedDim, or colData)
#'   \item Assigns observations to groups based on the grouping variable
#'   \item Calculates summary statistics (median by default) per feature per group
#'   \item Selects top N features per group based on the summary statistic
#'   \item Creates faceted boxplots with features ordered by their summary statistic
#'   \item Colors boxplots by summary statistic value
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with reduced dimensions (e.g., PhiSpace scores)
#' plotGroups(spe, group_var = "cluster")
#' 
#' # Plot gene expression from assays
#' plotGroups(spe, 
#'            group_var = "cluster",
#'            source = "assay",
#'            assay_name = "logcounts",
#'            features = c("CD3E", "CD8A", "CD4", "MS4A1", "CD14"),
#'            top_n = NULL)  # Show all specified genes
#' 
#' # Plot top differentially expressed genes per cluster
#' plotGroups(spe,
#'            group_var = "cluster",
#'            source = "assay",
#'            assay_name = "logcounts",
#'            top_n = 10)
#' 
#' # Using a list of objects (e.g., multiple tissue samples)
#' plotGroups(tissue_list, group_var = "cluster", top_n = 10)
#' 
#' # Custom appearance for cell type scores
#' plotGroups(spe, 
#'            group_var = "condition",
#'            source = "reducedDim",
#'            reducedDim_name = "PhiSpace",
#'            top_n = 8,
#'            color_scale = c("darkblue", "yellow", "darkred"),
#'            facet_ncol = 2,
#'            group_prefix = "Condition")
#' 
#' # Show all features without filtering
#' plotGroups(spe, group_var = "cluster", top_n = NULL)
#' 
#' # Use mean instead of median for ranking
#' plotGroups(spe, 
#'            group_var = "cluster",
#'            source = "assay",
#'            assay_name = "logcounts",
#'            metric = "mean")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap coord_flip theme_minimal
#'   theme element_text scale_fill_gradient2 labs
#' @importFrom dplyr mutate group_by summarise ungroup inner_join left_join 
#'   select slice_max
#' @importFrom tidyr pivot_longer
#' @importFrom SingleCellExperiment reducedDim colData
#' @importFrom SummarizedExperiment assay
#' @importFrom stats reorder as.formula
#' @export
plotGroups <- function(object,
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
                       remove_na = TRUE) {
  
  # Match arguments
  metric <- match.arg(metric)
  facet_scales <- match.arg(facet_scales)
  source <- match.arg(source)
  
  # Load required packages
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  
  # ---- Extract data ----
  df <- .extract_feature_data(object, source, assay_name, reducedDim_name, 
                              features, group_var)
  
  # ---- Validate group variable ----
  if (!group_var %in% colnames(df)) {
    stop("group_var '", group_var, "' not found in the data. ",
         "Available columns: ", paste(colnames(df), collapse = ", "))
  }
  
  # Convert group variable to character for consistency
  df[[group_var]] <- as.character(df[[group_var]])
  
  # Remove NA if requested
  if (remove_na) {
    df <- df[!is.na(df[[group_var]]), ]
  }
  
  # ---- Reshape to long format ----
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = -tidyr::all_of(group_var),
      names_to = "feature",
      values_to = "value"
    )
  
  # ---- Calculate summary statistics per feature per group ----
  summary_func <- switch(metric,
                         median = function(x) median(x, na.rm = TRUE),
                         mean = function(x) mean(x, na.rm = TRUE),
                         max = function(x) max(x, na.rm = TRUE))
  
  group_feature_summary <- df_long %>%
    dplyr::group_by(!!rlang::sym(group_var), feature) %>%
    dplyr::summarise(summary_score = summary_func(value), .groups = 'drop')
  
  # ---- Select top N features per group ----
  if (!is.null(top_n)) {
    top_features <- group_feature_summary %>%
      dplyr::group_by(!!rlang::sym(group_var)) %>%
      dplyr::slice_max(order_by = summary_score, n = top_n, with_ties = FALSE) %>%
      dplyr::ungroup()
    
    # Filter boxplot data
    boxplot_data <- df_long %>%
      dplyr::inner_join(
        top_features %>% dplyr::select(tidyr::all_of(group_var), feature),
        by = c(group_var, "feature")
      )
  } else {
    boxplot_data <- df_long
    top_features <- group_feature_summary
  }
  
  # Add summary scores for ordering and coloring
  boxplot_data <- boxplot_data %>%
    dplyr::left_join(group_feature_summary, by = c(group_var, "feature"))
  
  # ---- Create plot titles ----
  if (is.null(title)) {
    if (!is.null(top_n)) {
      title <- sprintf("Top %d Features per %s (Ordered by %s)", 
                       top_n, 
                       tools::toTitleCase(gsub("_", " ", group_var)),
                       tools::toTitleCase(metric))
    } else {
      title <- sprintf("Feature Values by %s (Ordered by %s)",
                       tools::toTitleCase(gsub("_", " ", group_var)),
                       tools::toTitleCase(metric))
    }
  }
  
  if (is.null(subtitle)) {
    subtitle <- sprintf("Boxplots show distribution of values within each %s",
                        gsub("_", " ", group_var))
  }
  
  # ---- Create facet labels ----
  if (nchar(group_prefix) > 0) {
    boxplot_data[[group_var]] <- paste(group_prefix, boxplot_data[[group_var]])
  }
  
  # ---- Create plot ----
  p <- boxplot_data %>%
    dplyr::group_by(!!rlang::sym(group_var)) %>%
    dplyr::mutate(feature_ordered = stats::reorder(feature, summary_score)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = feature_ordered, 
                                 y = value, 
                                 fill = summary_score)) +
    ggplot2::scale_fill_gradient2(
      low = color_scale[1], 
      mid = color_scale[2], 
      high = color_scale[3],
      name = paste(tools::toTitleCase(metric), "Value")
    ) +
    ggplot2::geom_boxplot(alpha = box_alpha, outlier.size = outlier_size) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", group_var)), 
                        scales = facet_scales, 
                        ncol = facet_ncol) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = if(show_legend) "right" else "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Feature",
      y = "Value"
    )
  
  return(p)
}


#' Extract Feature Data from Various Object Types and Sources
#'
#' Internal helper function to extract numeric feature data from different 
#' object types and data sources (assays, reduced dimensions, or column metadata).
#'
#' @param object Input object (SpatialExperiment, SingleCellExperiment, list, or data.frame)
#' @param source Data source: "assay", "reducedDim", or "colData"
#' @param assay_name Name of the assay (for source = "assay")
#' @param reducedDim_name Name of the reduced dimension (for source = "reducedDim")
#' @param features Character vector of features to extract (NULL = all)
#' @param group_var Name of the grouping variable
#'
#' @return A data.frame with feature values and group variable
#' @keywords internal
.extract_feature_data <- function(object, source, assay_name, reducedDim_name, 
                                  features, group_var) {
  
  # Case 1: List of experiment objects
  if (is.list(object) && !is.data.frame(object)) {
    
    # Check if list contains experiment objects
    if (!all(sapply(object, function(x) {
      inherits(x, "SpatialExperiment") || 
        inherits(x, "SingleCellExperiment") ||
        inherits(x, "SummarizedExperiment")
    }))) {
      stop("If 'object' is a list, all elements must be SpatialExperiment, ",
           "SingleCellExperiment, or SummarizedExperiment objects")
    }
    
    # Extract data from each object based on source
    df_list <- lapply(object, function(x) {
      .extract_from_single_object(x, source, assay_name, reducedDim_name, features)
    })
    
    df <- do.call(rbind, df_list)
    
    # Try to extract group variable from colData of concatenated objects
    if (group_var %in% colnames(SummarizedExperiment::colData(object[[1]]))) {
      group_values <- do.call(c, lapply(object, function(x) {
        SummarizedExperiment::colData(x)[[group_var]]
      }))
      df[[group_var]] <- group_values
    } else {
      stop("group_var '", group_var, "' not found in colData of objects")
    }
    
    # Case 2: Single experiment object  
  } else if (inherits(object, "SpatialExperiment") || 
             inherits(object, "SingleCellExperiment") ||
             inherits(object, "SummarizedExperiment")) {
    
    df <- .extract_from_single_object(object, source, assay_name, 
                                      reducedDim_name, features)
    
    # Extract group variable from colData
    if (group_var %in% colnames(SummarizedExperiment::colData(object))) {
      df[[group_var]] <- SummarizedExperiment::colData(object)[[group_var]]
    } else {
      stop("group_var '", group_var, "' not found in colData")
    }
    
    # Case 3: Data frame  
  } else if (is.data.frame(object)) {
    df <- object
    
  } else {
    stop("'object' must be a SpatialExperiment, SingleCellExperiment, ",
         "SummarizedExperiment, list of such objects, or a data.frame")
  }
  
  return(df)
}


#' Extract Data from Single Experiment Object
#'
#' Internal helper to extract data from a single experiment object based on source.
#'
#' @keywords internal
.extract_from_single_object <- function(object, source, assay_name, 
                                        reducedDim_name, features) {
  
  if (source == "assay") {
    # Extract from assays
    if (!assay_name %in% SummarizedExperiment::assayNames(object)) {
      stop("Assay '", assay_name, "' not found. Available assays: ",
           paste(SummarizedExperiment::assayNames(object), collapse = ", "))
    }
    
    mat <- SummarizedExperiment::assay(object, assay_name)
    
    # Subset features if specified
    if (!is.null(features)) {
      missing_features <- setdiff(features, rownames(mat))
      if (length(missing_features) > 0) {
        warning("Features not found in assay: ", 
                paste(missing_features, collapse = ", "))
      }
      available_features <- intersect(features, rownames(mat))
      if (length(available_features) == 0) {
        stop("None of the specified features found in assay '", assay_name, "'")
      }
      mat <- mat[available_features, , drop = FALSE]
    }
    
    # Transpose so rows are observations (cells) and columns are features (genes)
    df <- as.data.frame(t(as.matrix(mat)))
    
  } else if (source == "reducedDim") {
    # Extract from reduced dimensions
    if (!reducedDim_name %in% SingleCellExperiment::reducedDimNames(object)) {
      stop("reducedDim '", reducedDim_name, "' not found. Available: ",
           paste(SingleCellExperiment::reducedDimNames(object), collapse = ", "))
    }
    
    rd <- SingleCellExperiment::reducedDim(object, reducedDim_name)
    df <- as.data.frame(rd)
    
    # Features parameter is ignored for reducedDim
    if (!is.null(features)) {
      warning("'features' parameter is ignored when source = 'reducedDim'")
    }
    
  } else if (source == "colData") {
    # Extract from colData
    cd <- as.data.frame(SummarizedExperiment::colData(object))
    
    # Select only numeric columns
    numeric_cols <- sapply(cd, is.numeric)
    if (sum(numeric_cols) == 0) {
      stop("No numeric columns found in colData")
    }
    
    df <- cd[, numeric_cols, drop = FALSE]
    
    # Subset features if specified
    if (!is.null(features)) {
      missing_features <- setdiff(features, colnames(df))
      if (length(missing_features) > 0) {
        warning("Features not found in colData: ",
                paste(missing_features, collapse = ", "))
      }
      available_features <- intersect(features, colnames(df))
      if (length(available_features) == 0) {
        stop("None of the specified features found in colData")
      }
      df <- df[, available_features, drop = FALSE]
    }
    
  } else {
    stop("Invalid source. Must be 'assay', 'reducedDim', or 'colData'")
  }
  
  return(df)
}


#' @describeIn plotGroups Legacy function name (deprecated)
#' @keywords internal
.extract_phispace_data <- function(object, reducedDim_name, group_var) {
  .Deprecated(".extract_feature_data")
  .extract_feature_data(object, source = "reducedDim", 
                        assay_name = NULL, 
                        reducedDim_name = reducedDim_name,
                        features = NULL,
                        group_var = group_var)
}


#' Plot Groups with Multiple Metrics
#'
#' Create a multi-panel plot showing group differences using different summary
#' metrics (median, mean, max) for comparison.
#'
#' @inheritParams plotGroups
#' @param metrics Character vector of metrics to display. Default is 
#'   c("median", "mean", "max").
#' @param ... Other input from plotGroups.
#'
#' @return A combined ggplot2 object (requires patchwork package)
#'
#' @examples
#' \dontrun{
#' # Compare different ranking metrics for cell type scores
#' plotGroupsMultiMetric(spe, 
#'                       group_var = "cluster", 
#'                       top_n = 5)
#' 
#' # Compare metrics for gene expression
#' plotGroupsMultiMetric(spe,
#'                       group_var = "cluster",
#'                       source = "assay",
#'                       assay_name = "logcounts",
#'                       features = c("CD3E", "CD8A", "CD4"),
#'                       top_n = NULL)
#' }
#'
#' @export
plotGroupsMultiMetric <- function(object,
                                  group_var,
                                  features = NULL,
                                  source = c("reducedDim", "assay", "colData"),
                                  assay_name = "logcounts",
                                  reducedDim_name = "PhiSpace",
                                  top_n = 5,
                                  metrics = c("median", "mean", "max"),
                                  ...) {
  
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for plotGroupsMultiMetric. ",
         "Please install it with: install.packages('patchwork')")
  }
  
  source <- match.arg(source)
  
  plots <- lapply(metrics, function(m) {
    plotGroups(object = object,
               group_var = group_var,
               features = features,
               source = source,
               assay_name = assay_name,
               reducedDim_name = reducedDim_name,
               top_n = top_n,
               metric = m,
               title = paste("Ranked by", tools::toTitleCase(m)),
               ...)
  })
  
  # Combine plots
  combined <- Reduce(`+`, plots) + 
    patchwork::plot_layout(ncol = 1) +
    patchwork::plot_annotation(
      title = sprintf("Feature Values by %s - Multiple Ranking Metrics",
                      tools::toTitleCase(gsub("_", " ", group_var)))
    )
  
  return(combined)
}