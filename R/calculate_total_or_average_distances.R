#' Calculate the total or average distance between all units for each set in a sample_selection object
#'
#' @param sample_selection_object A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)
#' @param distance_matrix A distance matrix, see function create_distance_matrix()
#' @param total If total = TRUE, the total distance between all units is returned, if total=FALSE, the average distance between all units is returned
#' @export


calculate_total_or_average_distances <- function(sample_selection_object, distance_matrix, total = TRUE) {

  if(!class(sample_selection_object)=="sample_selection")
    stop("sample_selection_object must be a sample_selection object, created with a create_set() function.")

  set_total_distances <- sapply(sample_selection_object, function(set, distance_matrix) {
    return(sum(distance_matrix[set,set])/2)
  }, distance_matrix)

  if (total == FALSE){

    set_average_distances <- sapply(1:length(sample_selection_object), function(set_number, set_total_distances) {
      set_total_distances[set_number]/(0.5*length(sample_selection_object[[set_number]])^2)
    }, set_total_distances)

    return(sort(set_average_distances))
  }

  return(sort(set_total_distances))
}


#' Subset a distance matrix by a sample_selection object
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param sample_selection A sample_selection object (see find_sets()).
#' @return A subsetted distance matrix.
#' @export
dist_mat_subset <- function(distance_matrix,set){
     distance_matrix[set,set]
}


#' Calculate average distance in a given set
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param set A set of unit IDs
#' @param unit_distances If unit_distances = TRUE, calculates distances at unit level.
#' @param sum_distances If sum_distances = TRUE, sums instead of averaging.
#' @export
calculate_set_distances <- function(distance_matrix,set = NULL,unit_distances = FALSE,sum_distances = FALSE,min_distances = FALSE){
     if(min_distance&sum_distances&!unit_distances){
          warning("Cannot sum minimum distances when unit_distances is FALSE.")
     }
     if(!is.null(set)){
          dmat <-
               dist_mat_subset(distance_matrix = distance_matrix,
                               set = set)}

     if(min_distances&unit_distances){
          diag(dmat) <- NA
          min_dists <- apply(dmat,1,min,na.rm = T)
          if(sum_distances){
          return(sum(min_dists))
               }else{min_dists}
     }

     if(min_distances&!unit_distances){
          diag(dmat) <- NA
          return(min(dmat,na.rm = T))
     }


     if(!unit_distances&!sum_distances){
          return(c(
               average_all_distances = mean(dmat)))
     }

     if(!unit_distances&sum_distances){
          return(c(
               sum_all_distances = sum(dmat)))
     }

     if(unit_distances&sum_distances){
          sum_unit_distances <- rowSums(dmat)
          return(data.frame(sum_unit_distances=sum_unit_distances))
     }

     if(unit_distances&!sum_distances){
          mean_unit_distances <- rowMeans(dmat)
          return(data.frame(mean_unit_distances=mean_unit_distances))
     }



}



#' Summarize the distances in a set of sets
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param sample_selection A sample_selection object (see find_sets()).
#' @param unit If unit == TRUE, calculates distances at unit level.
#' @param unit_distances If unit_distances = TRUE, calculates distances at unit level.
#' @param sum_distances If sum_distances = TRUE, sums instead of averaging.
#' @export
summarize_set_distances <- function(distance_matrix,sample_selection,unit_distances = FALSE,sum_distances = FALSE,min_distances = FALSE){
     sapply(X = sample_selection,
            FUN = calculate_set_distances,
            distance_matrix = distance_matrix,
            unit_distances = unit_distances,
            sum_distances = sum_distances,
            min_distances = min_distances)
}



















