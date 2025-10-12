#' Plot Sankey Diagram for Classification Results
#'
#' Create an interactive Sankey diagram to visualize the flow between 2 or 3
#' classification results. Useful for comparing clustering or classification
#' outcomes across different methods or timepoints.
#'
#' @param ... Two or three classification vectors. Each should be a vector of
#'   class labels (character, factor, or numeric).
#' @param add_suffix Logical. If TRUE (default), adds suffixes to distinguish
#'   between classification levels when plotting 3 classifications. Only used
#'   when 3 classifications are provided.
#' @param fontsize Numeric. Font size for node labels. Default is 12.
#' @param class_names Character vector. Optional names for the classification
#'   levels. If NULL, uses "Class1", "Class2", etc.
#'
#' @return An interactive Sankey diagram (networkD3 htmlwidget object).
#'
#' @details
#' The function automatically detects whether 2 or 3 classification vectors are
#' provided and creates the appropriate Sankey diagram:
#' \itemize{
#'   \item For 2 classifications: Shows direct flow from first to second
#'   \item For 3 classifications: Shows flow from first -> second -> third
#' }
#'
#' When \code{add_suffix = TRUE} with 3 classifications, suffixes ("-", "_", "")
#' are added to class labels to ensure uniqueness across levels.
#'
#' @examples
#' \dontrun{
#' # Two classifications
#' class1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' class2 <- sample(c("X", "Y", "Z"), 100, replace = TRUE)
#' plotSankey(class1, class2)
#'
#' # Three classifications
#' class3 <- sample(c("P", "Q", "R"), 100, replace = TRUE)
#' plotSankey(class1, class2, class3, fontsize = 14)
#'
#' # With custom names
#' plotSankey(class1, class2, class3, 
#'            class_names = c("Method1", "Method2", "Method3"))
#' }
#'
#' @export
plotSankey <- function(..., add_suffix = TRUE, fontsize = 12, class_names = NULL) {
  
  # Capture all classification vectors
  class_list <- list(...)
  n_classes <- length(class_list)
  
  # Validate input
  if (n_classes < 2 || n_classes > 3) {
    stop("plotSankey requires 2 or 3 classification vectors")
  }
  
  # Check all vectors have same length
  vec_lengths <- sapply(class_list, length)
  if (length(unique(vec_lengths)) > 1) {
    stop("All classification vectors must have the same length")
  }
  
  # Extract classifications
  class1 <- class_list[[1]]
  class2 <- class_list[[2]]
  class3 <- if (n_classes == 3) class_list[[3]] else NULL
  
  # Two-way Sankey
  if (n_classes == 2) {
    
    confTab <- table(class1, class2)
    
    # Create links
    colLinks <- c('source', 'target', 'value')
    sankeyLinks <- matrix(ncol = length(colLinks), nrow = 0)
    colnames(sankeyLinks) <- colLinks
    
    for (i in 1:nrow(confTab)) {
      for (j in 1:ncol(confTab)) {
        if (confTab[i, j] > 0) {
          sankeyLinks <- rbind(
            sankeyLinks,
            c(i - 1, nrow(confTab) + j - 1, confTab[i, j])
          )
        }
      }
    }
    
    sankeyNodes <- data.frame(name = c(rownames(confTab), colnames(confTab)))
    
    p <- networkD3::sankeyNetwork(
      Links = as.data.frame(sankeyLinks),
      Nodes = sankeyNodes, 
      NodeID = 'name',
      Source = 'source', 
      Target = 'target',
      Value = 'value',
      fontSize = fontsize
    )
    
  } else {
    # Three-way Sankey
    
    # Add suffixes to distinguish levels if requested
    if (add_suffix) {
      class1 <- paste0(class1, "-")
      class2 <- paste0(class2, "_")
    }
    
    # Define flow incidence matrix
    rowNams <- colNams <- unique(c(class1, class2, class3))
    
    incidMat <- matrix(
      0, 
      nrow = length(rowNams), 
      ncol = length(colNams),
      dimnames = list(rowNams, colNams)
    )
    
    # Fill incidence matrix for class1 -> class2
    confTab12 <- table(class1, class2)
    for (i in 1:nrow(confTab12)) {
      fromClass <- rownames(confTab12)[i]
      for (j in 1:ncol(confTab12)) {
        toClass <- colnames(confTab12)[j]
        incidMat[fromClass, toClass] <- confTab12[i, j]
      }
    }
    
    # Fill incidence matrix for class2 -> class3
    confTab23 <- table(class2, class3)
    for (i in 1:nrow(confTab23)) {
      fromClass <- rownames(confTab23)[i]
      # Check if fromClass exists in incidMat row names
      if (!(fromClass %in% rownames(incidMat))) next
      for (j in 1:ncol(confTab23)) {
        toClass <- colnames(confTab23)[j]
        # Check if toClass exists in incidMat column names
        if (!(toClass %in% colnames(incidMat))) next
        incidMat[fromClass, toClass] <- confTab23[i, j]
      }
    }
    
    # Create Sankey links
    sankeyLinks <- as.data.frame(incidMat) %>%
      dplyr::mutate(source = rownames(incidMat)) %>%
      tidyr::pivot_longer(!source, names_to = "target", values_to = "value") %>%
      dplyr::filter(value != 0)
    
    sankeyNodes <- data.frame(
      name = c(as.character(sankeyLinks$source),
               as.character(sankeyLinks$target)) |> unique()
    )
    
    sankeyLinks$IDsource <- match(sankeyLinks$source, sankeyNodes$name) - 1
    sankeyLinks$IDtarget <- match(sankeyLinks$target, sankeyNodes$name) - 1
    
    p <- suppressMessages(
      networkD3::sankeyNetwork(
        Links = sankeyLinks, 
        Nodes = sankeyNodes,
        Source = "IDsource", 
        Target = "IDtarget",
        Value = "value", 
        NodeID = "name",
        sinksRight = FALSE, 
        fontSize = fontsize
      )
    )
  }
  
  print(p)
  invisible(p)
}

