#' Create a binary adjacency matrix from a distance matrix.
#'
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param threshold The minimum distance one unit should be from another (expressed in same units as distance matrix)
#' @param zero_diagonal A logical argument indicating whether the diagonal of the adjacency matrix should be all 0's, such that a unit is not defined as adjacent to itself.
#' @return An adjacency matrix with binary indicators for adjacency.
#' @export
create_adjacency_matrix <- function(distance_matrix,threshold,zero_diagonal = T,...){
     adjacency_matrix <- (distance_matrix<threshold)*1

     if(zero_diagonal)
          diag(adjacency_matrix) <- 0

     return(adjacency_matrix)

}



