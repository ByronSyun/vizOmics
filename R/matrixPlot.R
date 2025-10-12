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
#' @param colBy Numeric or character vector to specify color of points. Should have
#'   the same length as the number of rows in \code{scores}.
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
#' @param color_palette Character. Color palette to use. Options: "matlab" (default),
#'   "viridis", "plasma", "inferno", "magma". Only used for numeric \code{colBy}.
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
#' For numeric \code{colBy}, the MATLAB-style color gradient is used by default,
#' which provides good contrast for continuous variables.
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
#' # Color by groups
#' groups <- rep(c("A", "B", "C"), length.out = 100)
#' matrixPlot(scores, max_ncomp = 3, colBy = groups)
#'
#' # Color by continuous variable
#' continuous_var <- rnorm(100)
#' matrixPlot(scores, max_ncomp = 3, colBy = continuous_var,
#'            legendTitle = "Expression")
#'
#' # Custom colors
#' matrixPlot(scores, max_ncomp = 2, colBy = groups,
#'            manualCol = c("red", "blue", "green"))
#' }
#'
#' @export
matrixPlot <- function(
    scores,
    max_ncomp = NULL,
    comp_idx = NULL,
    colBy = NULL,
    pointAlpha = NULL,
    pointSize = 1,
    manualCol = NULL,
    manualAlpha = NULL,
    fsize = 14,
    returnPlotList = FALSE,
    legendTitle = "",
    compName = "comp",
    color_palette = "matlab"
) {
  
  # Input validation
  if (is.null(max_ncomp) & is.null(comp_idx)) {
    stop("Need to specify either max_ncomp or comp_idx.")
  }
  
  if (!is.null(max_ncomp)) comp_idx <- 1:max_ncomp
  
  # Check if components exist in scores
  if (!all(paste0(compName, comp_idx) %in% colnames(scores))) {
    missingComps <-
      paste0(compName, comp_idx)[!(paste0(compName, comp_idx) %in% colnames(scores))]
    stop(paste0("These components are missing from scores: ", 
                paste(missingComps, collapse = ", ")))
  }
  
  # Get color palette
  color_scale <- get_color_palette(color_palette)
  
  Ngroups <- length(unique(colBy))
  
  # Three or more components: matrix plot
  if (length(comp_idx) >= 3) {
    
    # Density plots on diagonal
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
        out_diag[[comp_i]] <- p
        
      } else {
        p <- p +
          ggplot2::geom_jitter(
            ggplot2::aes(y = 0, colour = colBy),
            height = diff(ggplot2::layer_scales(p)$y$range$range) / 20,
            size = pointSize,
            stroke = 0
          )
        
        if (!is.null(manualCol)) {
          p <- p + ggplot2::scale_color_manual(values = manualCol)
        } else {
          if (is.numeric(colBy)) {
            p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
          }
        }
        
        out_diag[[comp_i]] <- p
      }
    }
    
    # Get the legend
    if (!is.null(colBy)) {
      suppressWarnings(
        p_legend <- cowplot::get_legend(
          p + ggplot2::theme(legend.position = "right") +
            ggplot2::labs(colour = legendTitle)
        )
      )
    }
    
    # Scatter plots on non-diagonal
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
        
        if (!is.null(manualCol)) {
          p <- p + ggplot2::scale_color_manual(values = manualCol)
        } else {
          if (is.numeric(colBy)) {
            p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
          }
        }
      }
      
      out_nondiag[[comb]] <- p
    }
    
    # Arrange plots
    out <- c(out_nondiag, out_diag)
    
    # Arrange diagonal
    diagIdx <- 1
    toAdd <- length(comp_idx)
    for (kk in 1:length(comp_idx)) {
      out[[diagIdx]] <- out_diag[[kk]]
      diagIdx <- diagIdx + toAdd
      toAdd <- toAdd - 1
    }
    
    # Non-diagonal, arrange column by column
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
    
    # Return
    if (returnPlotList) {
      return(list(matrixPlot = p, plotList = out))
    } else {
      return(p)
    }
    
  } else if (length(comp_idx) == 1) {
    # Single component: density plot only
    
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
    }
    
    if (!is.null(manualCol)) {
      out <- out + ggplot2::scale_color_manual(values = manualCol)
    } else {
      if (is.numeric(colBy)) {
        out <- out + ggplot2::scale_colour_gradientn(colours = color_scale)
      }
    }
    
    # Alpha scale
    if (is.null(manualAlpha)) {
      if (is.null(pointAlpha)) pointAlpha <- 1
      out <- out + ggplot2::scale_alpha_manual(values = rep(pointAlpha, Ngroups))
    } else {
      out <- out + ggplot2::scale_alpha_manual(values = manualAlpha)
    }
    
    return(out)
    
  } else {
    # Two components: scatter plot
    
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
      
      if (!is.null(manualCol)) {
        p <- p + ggplot2::scale_color_manual(values = manualCol)
      } else {
        if (is.numeric(colBy)) {
          p <- p + ggplot2::scale_colour_gradientn(colours = color_scale)
        }
      }
    }
    
    return(p)
  }
}


#' Get Color Palette
#'
#' Internal function to get color palettes for continuous variables.
#'
#' @param palette_name Character. Name of the palette.
#'
#' @return Character vector of colors.
#' @keywords internal
get_color_palette <- function(palette_name = "matlab") {
  
  if (palette_name == "matlab") {
    # MATLAB-style color gradient
    return(c(
      grDevices::rgb(54, 70, 157, maxColorValue = 255),
      grDevices::rgb(61, 146, 185, maxColorValue = 255),
      grDevices::rgb(126, 203, 166, maxColorValue = 255),
      grDevices::rgb(204, 234, 156, maxColorValue = 255),
      grDevices::rgb(249, 252, 181, maxColorValue = 255),
      grDevices::rgb(255, 226, 144, maxColorValue = 255),
      grDevices::rgb(253, 164, 93, maxColorValue = 255),
      grDevices::rgb(234, 95, 70, maxColorValue = 255),
      grDevices::rgb(185, 30, 72, maxColorValue = 255)
    ))
  } else if (palette_name == "viridis") {
    return(viridisLite::viridis(9))
  } else if (palette_name == "plasma") {
    return(viridisLite::plasma(9))
  } else if (palette_name == "inferno") {
    return(viridisLite::inferno(9))
  } else if (palette_name == "magma") {
    return(viridisLite::magma(9))
  } else {
    warning("Unknown palette name. Using MATLAB colors.")
    return(get_color_palette("matlab"))
  }
}

