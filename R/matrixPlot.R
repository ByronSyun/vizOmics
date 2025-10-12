#' Score Matrix Plot
#'
#' Create density and pairwise scatter plots for visualizing score matrices,
#' such as PCA scores, DIVAS components, or other dimensionality reduction results.
#'
#' @param scores Matrix or data frame of scores to be plotted. Should have column
#'   names matching the pattern specified by \code{compName}.
#' @param max_ncomp Default NULL. Number of first components to plot. If specified,
#'   will override \code{comp_idx}.
#' @param comp_idx Default NULL. Indices of specific components to plot.
#' @param colBy Numeric, character, or factor vector to specify color of points. 
#'   Should have the same length as the number of rows in \code{scores}.
#' @param colBy_type Character. Type of coloring: "auto" (default), "discrete". The
#'   previous option "continuous" is deprecated and will be treated as "auto".
#' @param pointAlpha Numeric. Alpha transparency value for points (0-1).
#' @param pointSize Numeric. Size of points. Default is 1.
#' @param manualCol Character vector. Manual specification of colors for discrete
#'   \code{colBy} values.
#' @param manualAlpha Numeric vector. Manual specification of alpha values.
#' @param fsize Numeric. Base font size for plots. Default is 14.
#' @param returnPlotList Logical. Whether to return individual plots as a list.
#'   Default is FALSE.
#' @param legendTitle Character. Title for the legend. Default is empty string.
#' @param compName Character. Prefix for component column names. Default is "comp",
#'   so that the 1st column is named "comp1", etc.
#' @param color_palette Character. For continuous coloring: "matlab" (default),
#'   "viridis", "plasma", "inferno", "magma". For discrete coloring: "Set1", "Set2",
#'   "Set3", "Dark2", "Paired", or "custom".
#' @param sort_levels Logical. For discrete coloring, whether to sort factor levels
#'   to ensure consistent color mapping across runs. Default is TRUE.
#' @param treat_ordinal_as_continuous Logical. When TRUE, attempt to coerce
#'   character/factor ordinal labels (e.g. "1", "1 or 2", "7") into numeric values and
#'   use continuous gradients automatically if coercion succeeds. Default FALSE.
#' @param reverse_gradient Logical. If TRUE, reverses the color gradient direction.
#'   For example, if your variable has higher values = more severe (like WOS: 7 is
#'   most severe), set TRUE to map high values to red. Default is FALSE (low = blue,
#'   high = red for matlab palette).
#'
#' @return A combined plot (ggplot/grid object) showing:
#'   \itemize{
#'     \item Diagonal: Density plots with jittered points
#'     \item Off-diagonal: Pairwise scatter plots
#'   }
#'   If \code{returnPlotList = TRUE}, returns a list with the combined plot and
#'   individual plot components.
#'
#' @details
#' The function creates different visualizations depending on the number of components:
#' \itemize{
#'   \item 1 component: Single density plot with jittered points
#'   \item 2 components: Scatter plot
#'   \item 3+ components: Matrix of density plots (diagonal) and scatter plots (off-diagonal)
#' }
#'
#' ## Color Type Detection (when colBy_type = "auto")
#' The function intelligently determines whether to use discrete or continuous colors:
#' \itemize{
#'   \item Factor or character: Always discrete, unless \code{treat_ordinal_as_continuous = TRUE}
#'         and labels can be reliably coerced to numeric (e.g. "1 or 2" -> 1.5)
#'   \item Numeric with <20 unique values and <50\% of total: Discrete
#'   \item Numeric otherwise: Continuous
#' }
#'
#' ## Color Consistency
#' For discrete variables, colors are mapped consistently by sorting factor levels,
#' ensuring the same colors appear for the same groups across different runs.
#' Set \code{sort_levels = FALSE} to use the original factor level order.
#'
#' @examples
#' \dontrun{
#' # Create example score matrix
#' scores <- data.frame(
#'   comp1 = rnorm(100),
#'   comp2 = rnorm(100),
#'   comp3 = rnorm(100)
#' )
#'
#' # Basic plot
#' matrixPlot(scores, max_ncomp = 3)
#'
#' # Ordinal labels treated as discrete by default (7 fixed colours)
#' wos <- factor(c("1","1 or 2","2","3","4","5","6","7"))[sample(1:8, 100, TRUE)]
#' matrixPlot(scores, max_ncomp = 3, colBy = wos, legendTitle = "WOS")
#'
#' # For WOS where 7 is most severe, reverse gradient to map 7->red, 1->blue
#' matrixPlot(scores, max_ncomp = 3, colBy = wos, legendTitle = "WOS",
#'            reverse_gradient = TRUE)
#' }
#'
#' @export
matrixPlot <- function(
    scores,
    max_ncomp = NULL,
    comp_idx = NULL,
    colBy = NULL,
    colBy_type = c("auto", "discrete"),
    pointAlpha = NULL,
    pointSize = 1,
    manualCol = NULL,
    manualAlpha = NULL,
    fsize = 14,
    returnPlotList = FALSE,
    legendTitle = "",
    compName = "comp",
    color_palette = "matlab",
    sort_levels = TRUE,
    treat_ordinal_as_continuous = FALSE,
    reverse_gradient = FALSE
) {
  
  # Backward compatibility: if user passed now-deprecated "continuous"
  if (!missing(colBy_type)) {
    if (length(colBy_type) == 1 && is.character(colBy_type) && identical(colBy_type, "continuous")) {
      warning("colBy_type='continuous' is deprecated and ignored; using auto-detection.")
      colBy_type <- "auto"
    }
  }
  
  colBy_type <- match.arg(colBy_type)
  
  if (is.null(max_ncomp) & is.null(comp_idx)) {
    stop("Need to specify either max_ncomp or comp_idx.")
  }
  
  if (!is.null(max_ncomp)) comp_idx <- 1:max_ncomp
  
  if (!all(paste0(compName, comp_idx) %in% colnames(scores))) {
    missingComps <-
      paste0(compName, comp_idx)[!(paste0(compName, comp_idx) %in% colnames(scores))]
    stop(paste0("These components are missing from scores: ", 
                paste(missingComps, collapse = ", ")))
  }
  
  use_discrete <- FALSE
  used_ordinal_numeric <- FALSE
  if (!is.null(colBy)) {
    if (colBy_type == "auto") {
      # Try ordinal -> numeric coercion first for factors/characters
      if (treat_ordinal_as_continuous && (is.factor(colBy) || is.character(colBy))) {
        coerced <- coerce_ordinal_numeric(colBy)
        if (!is.null(coerced$numeric) && coerced$success) {
          colBy <- coerced$numeric
          use_discrete <- FALSE
          used_ordinal_numeric <- TRUE
        } else {
          use_discrete <- is_discrete_variable(colBy)
        }
      } else {
        use_discrete <- is_discrete_variable(colBy)
      }
    } else {
      use_discrete <- (colBy_type == "discrete")
    }
    
    if (use_discrete) {
      colBy_prepared <- prepare_discrete_coloring(colBy, sort_levels, reverse_gradient)
      colBy <- colBy_prepared$colBy
      color_mapping <- colBy_prepared$colors
      if (!is.null(manualCol)) {
        if (is.null(names(manualCol))) names(manualCol) <- levels(colBy)
        color_mapping <- manualCol
      }
    } else {
      color_scale <- get_color_palette(color_palette, reverse = reverse_gradient)
    }
  }
  
  Ngroups <- if (!is.null(colBy)) length(unique(colBy)) else 0
  
  if (length(comp_idx) >= 3) {
    out_diag <- vector("list", length(comp_idx))
    
    for (comp_i in 1:length(comp_idx)) {
      var2plot <- paste0(compName, comp_idx[comp_i])
      
      p <- scores %>%
        ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var2plot))) +
        ggplot2::geom_density(bw = "sj") +
        ggplot2::theme_bw(base_size = fsize) +
        ggplot2::theme(
          legend.position = "none",
          axis.title.y = ggplot2::element_blank()
        )
      
      if (is.null(colBy)) {
        p <- p +
          ggplot2::geom_jitter(
            ggplot2::aes(y = 0), 
            height = diff(ggplot2::layer_scales(p)$y$range$range) / 20, 
            size = pointSize, 
            stroke = 0
          )
      } else {
        p <- p +
          ggplot2::geom_jitter(
            ggplot2::aes(y = 0, colour = colBy),
            height = diff(ggplot2::layer_scales(p)$y$range$range) / 20,
            size = pointSize,
            stroke = 0
          )
        
        if (use_discrete) {
          p <- p + ggplot2::scale_color_manual(values = color_mapping)
        } else {
          p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
        }
      }
      
      out_diag[[comp_i]] <- p
    }
    
    if (!is.null(colBy)) {
      suppressWarnings(
        p_legend <- cowplot::get_legend(
          p + ggplot2::theme(legend.position = "right") +
            ggplot2::labs(colour = legendTitle)
        )
      )
    }
    
    combs <- utils::combn(comp_idx, 2) %>% t()
    out_nondiag <- vector("list", nrow(combs))
    
    for (comb in 1:nrow(combs)) {
      var1 <- paste0(compName, combs[comb, 1])
      var2 <- paste0(compName, combs[comb, 2])
      
      if (is.null(colBy)) {
        p <- scores %>%
          ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var1), y = !!rlang::sym(var2))) +
          ggplot2::geom_point(size = pointSize, stroke = 0) +
          ggplot2::theme_bw(base_size = fsize) +
          ggplot2::theme(
            legend.position = "none",
            axis.ticks = ggplot2::element_blank()
          ) +
          ggplot2::scale_x_continuous(name = NULL, labels = NULL) +
          ggplot2::scale_y_continuous(name = NULL, labels = NULL)
        
      } else {
        p <- scores %>%
          ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var1), y = !!rlang::sym(var2))) +
          ggplot2::geom_point(ggplot2::aes(colour = colBy), size = pointSize, stroke = 0) +
          ggplot2::theme_bw(base_size = fsize) +
          ggplot2::theme(
            legend.position = "none",
            axis.ticks = ggplot2::element_blank()
          ) +
          ggplot2::scale_x_continuous(name = NULL, labels = NULL) +
          ggplot2::scale_y_continuous(name = NULL, labels = NULL)
        
        if (use_discrete) {
          p <- p + ggplot2::scale_color_manual(values = color_mapping)
        } else {
          p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
        }
      }
      
      out_nondiag[[comb]] <- p
    }
    
    out <- c(out_nondiag, out_diag)
    
    diagIdx <- 1
    toAdd <- length(comp_idx)
    for (kk in 1:length(comp_idx)) {
      out[[diagIdx]] <- out_diag[[kk]]
      diagIdx <- diagIdx + toAdd
      toAdd <- toAdd - 1
    }
    
    startIdxMat <- 2
    toAdd <- length(comp_idx) - 2
    startIdxNondiag <- 1
    for (kk in 1:(length(comp_idx) - 1)) {
      endIdxMat <- startIdxMat + toAdd
      endIdxNondiag <- startIdxNondiag + toAdd
      out[startIdxMat:endIdxMat] <- out_nondiag[startIdxNondiag:endIdxNondiag]
      startIdxMat <- endIdxMat + 2
      startIdxNondiag <- endIdxNondiag + 1
      toAdd <- toAdd - 1
    }
    
    layoutM <- matrix(NA, length(comp_idx), length(comp_idx))
    layoutM[lower.tri(layoutM, diag = TRUE)] <- 1:length(out)
    
    if (!is.null(colBy)) {
      out[[length(out) + 1]] <- p_legend
      layoutM[1, length(comp_idx)] <- length(out)
    }
    
    p <- gridExtra::grid.arrange(grobs = out, layout_matrix = layoutM)
    
    if (returnPlotList) {
      return(list(matrixPlot = p, plotList = out))
    } else {
      return(p)
    }
    
  } else if (length(comp_idx) == 1) {
    var2plot <- paste0(compName, comp_idx)
    
    out <- scores %>%
      ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var2plot))) +
      ggplot2::geom_density(bw = "sj") +
      ggplot2::theme_bw(base_size = fsize) +
      ggplot2::ylab("Density")
    
    if (is.null(colBy)) {
      out <- out +
        ggplot2::geom_jitter(
          ggplot2::aes(y = 0),
          height = diff(ggplot2::layer_scales(out)$y$range$range) / 20,
          size = pointSize,
          shape = 16,
          stroke = 0
        )
      
    } else {
      out <- out +
        ggplot2::geom_jitter(
          ggplot2::aes(y = 0, colour = colBy),
          height = diff(ggplot2::layer_scales(out)$y$range$range) / 20,
          size = pointSize,
          shape = 16,
          stroke = 0
        ) +
        ggplot2::labs(colour = legendTitle)
      
      if (use_discrete) {
        out <- out + ggplot2::scale_color_manual(values = color_mapping)
      } else {
        out <- out + ggplot2::scale_colour_gradientn(colours = color_scale)
      }
    }
    
    if (is.null(manualAlpha)) {
      if (is.null(pointAlpha)) pointAlpha <- 1
      out <- out + ggplot2::scale_alpha_manual(values = rep(pointAlpha, max(Ngroups, 1)))
    } else {
      out <- out + ggplot2::scale_alpha_manual(values = manualAlpha)
    }
    
    return(out)
    
  } else {
    var1 <- paste0(compName, comp_idx[1])
    var2 <- paste0(compName, comp_idx[2])
    
    if (is.null(colBy)) {
      p <- scores %>%
        ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var1), y = !!rlang::sym(var2))) +
        ggplot2::geom_point(size = pointSize, stroke = 0) +
        ggplot2::theme_bw(base_size = fsize)
      
    } else {
      p <- scores %>%
        ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(var1), y = !!rlang::sym(var2))) +
        ggplot2::geom_point(ggplot2::aes(colour = colBy), size = pointSize, stroke = 0) +
        ggplot2::theme_bw(base_size = fsize) +
        ggplot2::labs(colour = legendTitle)
      
      if (use_discrete) {
        p <- p + ggplot2::scale_color_manual(values = color_mapping)
      } else {
        p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
      }
    }
    
    return(p)
  }
}


#' Detect if Variable Should Use Discrete Coloring
#'
#' @param x Vector to check
#' @return Logical indicating whether to use discrete colors
#' @keywords internal
is_discrete_variable <- function(x) {
  if (is.factor(x) || is.character(x)) {
    return(TRUE)
  }
  if (is.numeric(x)) {
    n_unique <- length(unique(x))
    n_total <- length(x)
    if (n_unique < 20 && n_unique / n_total < 0.5) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Coerce Ordinal Labels to Numeric (e.g. "1", "1 or 2")
#'
#' Attempts to convert factor/character ordinal labels to numeric values.
#' Recognises patterns like "a or b" and uses their average.
#'
#' @param x Character or factor vector
#' @return List with fields: numeric (numeric vector) and success (logical)
#' @keywords internal
coerce_ordinal_numeric <- function(x) {
  if (!(is.factor(x) || is.character(x))) return(list(numeric = NULL, success = FALSE))
  vals <- as.character(x)
  out <- suppressWarnings(as.numeric(vals))
  # Handle patterns like "1 or 2"
  need_parse <- is.na(out)
  if (any(need_parse)) {
    parse_pair <- function(s) {
      s_low <- tolower(s)
      if (grepl("[0-9]+\\s*or\\s*[0-9]+", s_low)) {
        nums <- as.numeric(unlist(regmatches(s_low, gregexpr("[0-9]+", s_low))))
        if (length(nums) >= 2) return(mean(nums[1:2]))
      }
      # Fallback: first numeric found
      num <- suppressWarnings(as.numeric(regmatches(s_low, regexpr("[0-9]+(\\.[0-9]+)?", s_low))))
      return(num)
    }
    out[need_parse] <- vapply(vals[need_parse], parse_pair, numeric(1))
  }
  success_ratio <- mean(!is.na(out))
  success <- isTRUE(success_ratio >= 0.8) && (length(unique(out[!is.na(out)])) >= 3)
  list(numeric = if (success) out else NULL, success = success)
}


#' Prepare Discrete Coloring Variable
#'
#' @param x Vector to prepare
#' @param sort_levels Whether to sort factor levels
#' @param reverse Whether to reverse color gradient
#' @return List with prepared factor and color mapping
#' @keywords internal
prepare_discrete_coloring <- function(x, sort_levels = TRUE, reverse = FALSE) {
  if (!is.factor(x)) {
    x <- as.factor(x)
  }
  if (sort_levels) {
    # Try numeric/ordinal ordering of levels if possible
    levs <- levels(x)
    ord_try <- coerce_ordinal_numeric(levs)
    if (!is.null(ord_try$numeric) && ord_try$success) {
      ord_idx <- order(ord_try$numeric)
      x <- factor(x, levels = levs[ord_idx])
    } else {
      x <- factor(x, levels = sort(levs))
    }
  }
  n_levels <- length(levels(x))
  colors <- get_discrete_colors(levels(x), "matlab", reverse = reverse)
  list(colBy = x, colors = colors)
}


#' Get Discrete Color Palette
#'
#' @param level_names Character vector of level names
#' @param palette_name Name of the palette
#' @param reverse Logical. If TRUE, reverse the color order
#' @return Named vector of colors
#' @keywords internal
get_discrete_colors <- function(level_names, palette_name = "matlab", reverse = FALSE) {
  n_colors <- length(level_names)
  # Default: use blue-to-red gradient (like continuous palettes) for ordinal data
  if (palette_name %in% c("matlab", "viridis", "plasma", "inferno", "magma")) {
    base <- get_color_palette(palette_name, reverse = reverse)
    colors <- grDevices::colorRampPalette(base)(n_colors)
    names(colors) <- level_names
    return(colors)
  }
  if (palette_name == "Set1") {
    base_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                     "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
  } else if (palette_name == "Set2") {
    base_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
                     "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
  } else if (palette_name == "Set3") {
    base_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
                     "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5")
  } else if (palette_name == "Dark2") {
    base_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                     "#66A61E", "#E6AB02", "#A6761D", "#666666")
  } else if (palette_name == "Paired") {
    base_colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                     "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                     "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  } else {
    base_colors <- grDevices::rainbow(n_colors)
  }
  if (n_colors > length(base_colors)) {
    colors <- grDevices::colorRampPalette(base_colors)(n_colors)
  } else {
    colors <- base_colors[1:n_colors]
  }
  names(colors) <- level_names
  colors
}


#' Get Continuous Color Palette
#'
#' Internal function to get color palettes for continuous variables.
#'
#' @param palette_name Character. Name of the palette.
#' @param reverse Logical. If TRUE, reverse the color order.
#'
#' @return Character vector of colors.
#' @keywords internal
get_color_palette <- function(palette_name = "matlab", reverse = FALSE) {
  if (palette_name == "matlab") {
    cols <- c(
      grDevices::rgb(54, 70, 157, maxColorValue = 255),
      grDevices::rgb(61, 146, 185, maxColorValue = 255),
      grDevices::rgb(126, 203, 166, maxColorValue = 255),
      grDevices::rgb(204, 234, 156, maxColorValue = 255),
      grDevices::rgb(249, 252, 181, maxColorValue = 255),
      grDevices::rgb(255, 226, 144, maxColorValue = 255),
      grDevices::rgb(253, 164, 93, maxColorValue = 255),
      grDevices::rgb(234, 95, 70, maxColorValue = 255),
      grDevices::rgb(185, 30, 72, maxColorValue = 255)
    )
  } else if (palette_name == "viridis") {
    cols <- viridisLite::viridis(9)
  } else if (palette_name == "plasma") {
    cols <- viridisLite::plasma(9)
  } else if (palette_name == "inferno") {
    cols <- viridisLite::inferno(9)
  } else if (palette_name == "magma") {
    cols <- viridisLite::magma(9)
  } else {
    warning("Unknown palette name. Using MATLAB colors.")
    cols <- get_color_palette("matlab", reverse = FALSE)
  }
  
  if (reverse) {
    cols <- rev(cols)
  }
  
  return(cols)
}
