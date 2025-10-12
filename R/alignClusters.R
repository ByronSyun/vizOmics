#' Align Clustering Results
#'
#' Given a reference clustering result, align a query clustering result to match
#' the reference labels using optimal assignment (Hungarian algorithm).
#'
#' @param clust Query clustering result, can be a numerical, character or factor vector.
#' @param clust_ref Reference clustering result to align to.
#'
#' @return Re-aligned clustering result, represented as a factor vector with levels
#'   matching the reference clustering.
#'   
#' @details
#' This function uses the Hungarian algorithm (via \code{clue::solve_LSAP}) to find
#' the optimal alignment between query and reference cluster labels. This is useful
#' when comparing clustering results from different methods or parameters, where
#' cluster labels may be arbitrary.
#'
#' @examples
#' \dontrun{
#' # Create example clustering results
#' clust_ref <- factor(c(rep("A", 50), rep("B", 50), rep("C", 50)))
#' clust_query <- factor(c(rep("1", 50), rep("2", 50), rep("3", 50)))
#' 
#' # Align query to reference
#' clust_aligned <- alignClusters(clust_query, clust_ref)
#' 
#' # Check alignment
#' table(clust_aligned, clust_ref)
#' }
#'
#' @export
alignClusters <- function(clust, clust_ref) {
  
  clust <- as.factor(clust)
  clust_ref <- as.factor(clust_ref)
  lvls <- levels(clust)
  lvls_old <- levels(clust_ref)
  
  # Use Hungarian algorithm to find optimal assignment
  sol <- clue::solve_LSAP(
    table(clust, clust_ref), 
    maximum = TRUE
  )
  
  kclust <- length(levels(clust_ref))
  adj <- (1:kclust)[sol]
  temp <- rep(NA, length(clust))
  
  # Remap cluster labels
  for (jj in 1:length(adj)) {
    temp[clust == lvls[jj]] <- lvls_old[adj[jj]]
  }
  
  clust <- factor(temp, levels = lvls_old)
  
  return(clust)
}

