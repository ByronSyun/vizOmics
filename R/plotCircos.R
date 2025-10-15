# Helper function to wrap long labels
wrap_label <- function(text, max_length){
  if (nchar(text) <= max_length) {
    return(text)
  }
  
  # Try to break at spaces, underscores, or other delimiters
  words <- strsplit(text, "([_\\s-])")[[1]]
  delimiters <- gregexpr("([_\\s-])", text)[[1]]
  delimiter_chars <- if (delimiters[1] != -1) {
    substring(text, delimiters, delimiters)
  } else {
    character(0)
  }
  
  if (length(words) == 1) {
    # No delimiters, force break at max_length
    part1 <- substring(text, 1, max_length)
    part2 <- substring(text, max_length + 1)
    return(paste(part1, part2, sep = "\n"))
  }
  
  # Build lines respecting word boundaries
  lines <- character()
  current_line <- ""
  
  for (i in seq_along(words)) {
    word <- words[i]
    delimiter <- if (i <= length(delimiter_chars)) delimiter_chars[i] else ""
    
    test_line <- if (current_line == "") {
      word
    } else {
      paste0(current_line, delimiter, word)
    }
    
    if (nchar(test_line) <= max_length) {
      current_line <- test_line
    } else {
      if (current_line != "") {
        lines <- c(lines, current_line)
      }
      current_line <- word
    }
  }
  
  if (current_line != "") {
    lines <- c(lines, current_line)
  }
  
  return(paste(lines, collapse = "\n"))
}

#' Create circos plot for multimodal correlations
#'
#' @param feature_list Named list of vectors, each containing feature names from one modality
#' @param data_list Named list of data matrices from the same modalities as feature_list, rows = features, columns = samples; must have the same length as feature_list
#' @param pos_threshold Numeric threshold for positive correlations (default 0.3)
#' @param neg_threshold Numeric threshold for negative correlations (default 0.3)
#' @param cor_method Correlation method: "pearson", "spearman", or "kendall" (default "pearson")
#' @param link_transparency Transparency for links (default 0.5)
#' @param gap_size Gap between sectors in degrees (default 2)
#' @param feature_label_cex Font size for feature labels (default 0.6)
#' @param legend_cex Font size for legend (default 0.8)
#' @param link_lwd Line width for correlation links (default 1)
#' @param label_wrap_length Maximum character length before wrapping labels (default 15)
#' @param brewerPalette Name of the RColorBrewer palette, representing different modalities
#' 
#' @return A list containing:
#'   \item{correlations}{Data frame of filtered correlations}
#'   \item{plot_recorded}{Recorded plot object that can be replayed with replayPlot()}
#'
plotCircos <- function(feature_list, 
                       data_list, 
                       pos_threshold = 0.3,
                       neg_threshold = 0.3,
                       cor_method = "pearson",
                       link_transparency = 0.5,
                       gap_size = 2,
                       feature_label_cex = 0.6,
                       legend_cex = 0.8,
                       link_lwd = 1,
                       label_wrap_length = 15,
                       brewerPalette = "Set2") {
  
  # Validate inputs
  if (length(feature_list) != length(data_list)) {
    stop("feature_list and data_list must contain the same number of elements")
  }
  if (!all(names(feature_list) == names(data_list))) {
    stop("Names of feature_list and data_list must match")
  }
  if (!cor_method %in% c("pearson", "spearman", "kendall")) {
    stop("cor_method must be one of: 'pearson', 'spearman', 'kendall'")
  }
  
  modalities <- names(feature_list)
  
  # Create unique feature identifiers to handle duplicates across modalities
  feature_ids <- list()
  feature_to_modality <- list()
  
  for (mod in modalities) {
    mod_features <- feature_list[[mod]]
    feature_ids[[mod]] <- paste0(mod, ":", mod_features)
    names(feature_ids[[mod]]) <- mod_features
    
    for (feat in mod_features) {
      if (is.null(feature_to_modality[[feat]])) {
        feature_to_modality[[feat]] <- character(0)
      }
      feature_to_modality[[feat]] <- c(feature_to_modality[[feat]], mod)
    }
  }
  
  # Calculate pairwise correlations between all features
  # Store in a list of data frames for efficiency
  cor_links <- list()
  link_idx <- 1
  
  for (i in 1:(length(modalities) - 1)) {
    mod1 <- modalities[i]
    features1 <- feature_list[[mod1]]
    data1 <- data_list[[mod1]][features1, , drop = FALSE]
    
    for (j in (i + 1):length(modalities)) {
      mod2 <- modalities[j]
      features2 <- feature_list[[mod2]]
      data2 <- data_list[[mod2]][features2, , drop = FALSE]
      
      # Calculate correlation matrix between the two modalities
      cor_mat <- stats::cor(t(data1), t(data2), method = cor_method, use = "pairwise.complete.obs")
      
      # Extract correlations above threshold (different thresholds for pos/neg)
      for (f1_idx in 1:length(features1)) {
        for (f2_idx in 1:length(features2)) {
          cor_val <- cor_mat[f1_idx, f2_idx]
          
          if (!is.na(cor_val)) {
            # Apply different thresholds for positive and negative correlations
            passes_threshold <- (cor_val >= pos_threshold) || (cor_val <= -neg_threshold)
            
            if (passes_threshold) {
              cor_links[[link_idx]] <- data.frame(
                feature1 = feature_ids[[mod1]][features1[f1_idx]],
                feature2 = feature_ids[[mod2]][features2[f2_idx]],
                correlation = cor_val,
                modality1 = mod1,
                modality2 = mod2,
                stringsAsFactors = FALSE
              )
              link_idx <- link_idx + 1
            }
          }
        }
      }
    }
  }
  
  if (length(cor_links) == 0) {
    stop(paste0("No correlations above thresholds found (positive >= ", 
                pos_threshold, ", negative <= -", neg_threshold, ")"))
  }
  
  cor_df <- do.call(rbind, cor_links)
  
  # Prepare sector data for circos plot
  all_feature_ids <- unlist(feature_ids)
  
  # Assign colors to modalities
  modality_colors <- stats::setNames(
    RColorBrewer::brewer.pal(min(5, length(modalities)), brewerPalette)[1:length(modalities)], modalities
  )
  
  # Create color vector for all features
  feature_colors <- character(length(all_feature_ids))
  names(feature_colors) <- all_feature_ids
  for (mod in modalities) {
    feature_colors[feature_ids[[mod]]] <- modality_colors[mod]
  }
  
  # Set up gaps - larger gaps between modalities
  gap_after <- rep(1, length(all_feature_ids))
  names(gap_after) <- all_feature_ids
  
  # Add larger gaps after the last feature of each modality
  for (i in 1:(length(modalities) - 1)) {
    mod <- modalities[i]
    last_feature <- feature_ids[[mod]][length(feature_ids[[mod]])]
    gap_after[last_feature] <- gap_size * 5  # Larger gap between modalities
  }
  # Last feature overall gets normal gap to wrap around
  gap_after[length(gap_after)] <- gap_size * 5
  
  # Initialize circos plot
  circlize::circos.clear()
  circlize::circos.par(gap.after = gap_after, start.degree = 90)
  
  # Create the plot
  circlize::chordDiagram(
    cor_df[, c("feature1", "feature2", "correlation")],
    grid.col = feature_colors,
    transparency = link_transparency,
    directional = 0,
    annotationTrack = "grid",  # Remove axis track, keep only grid
    preAllocateTracks = list(
      track.height = 0.1
    ),
    link.border = NA,
    link.lwd = link_lwd,  # Control line width
    link.lty = 1,  # Solid lines
    col = ifelse(cor_df$correlation > 0, 
                 grDevices::rgb(1, 0, 0, link_transparency),  # Red for positive
                 grDevices::rgb(0, 0, 1, link_transparency))  # Blue for negative
  )
  
  # Add labels
  circlize::circos.track(
    track.index = 1,
    panel.fun = function(x, y) {
      xlim = circlize::get.cell.meta.data("xlim")
      ylim = circlize::get.cell.meta.data("ylim")
      sector.name = circlize::get.cell.meta.data("sector.index")
      
      # Extract original feature name (remove modality prefix)
      feature_name <- sub("^[^:]+:", "", sector.name)
      
      # Wrap long feature names
      feature_name_wrapped <- wrap_label(feature_name, label_wrap_length)
      
      # Rotate labels
      theta = circlize::circlize(mean(xlim), 1.3)[1, 1] %% 360
      dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
      aa = c(1, 0.5)
      if(theta < 90 || theta > 270)  aa = c(0, 0.5)
      
      circlize::circos.text(
        mean(xlim), ylim[1],
        feature_name_wrapped,
        facing = dd,
        niceFacing = TRUE,
        adj = aa,
        cex = feature_label_cex
      )
    },
    bg.border = NA
  )
  
  # Add legend
  graphics::legend(
    "topleft",
    legend = modalities,
    fill = modality_colors,
    bty = "n",
    cex = legend_cex,
    title = "Modalities",
    border = modality_colors  # Match border to fill color for cleaner look
  )
  
  graphics::legend(
    "topright",
    legend = c("Positive", "Negative"),
    col = c(grDevices::rgb(1, 0, 0, 0.8), grDevices::rgb(0, 0, 1, 0.8)),
    lty = 1,
    lwd = 2 * legend_cex,  # Scale line width with legend font size
    bty = "n",
    cex = legend_cex,
    title = "Correlation"
  )
  
  circlize::circos.clear()
  
  # Return results invisibly
  return(invisible(list(
    correlations = cor_df,
    plot_recorded = grDevices::recordPlot()
  )))
}