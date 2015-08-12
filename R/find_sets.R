#' Find the sets within a sample_selection object that meet some criterion.
#'
#' @param sample_selection_object A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)
#' @param criteria A vector of criteria to be met
#' @return A list of admissable sets of units that meet the spatial criterion.
#' @export


find_sets <- function(sample_selection_object,criteria){
     if(!class(sample_selection_object)=="sample_selection")
          stop("sample_selection_object must be a sample_selection object, created with a create_set() function.")
     # Put all the constituent find_set functions in here
     return ("Some stuff.")
}


#' Find the biggest sets within a sample_selection object
#'
#' @param sample_selection_object A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)
#' @return A list of the biggest admissable sets.
#' @export

find_sets_biggest <- function(sample_selection_object){
     if(!class(sample_selection_object)=="sample_selection")
          stop("sample_selection_object must be a sample_selection object, created with a create_set() function.")

     set_lengths <- sapply(sample_selection_object,length)

     maximal_sets <- sample_selection_object[which(set_lengths==max(set_lengths))]

     return (maximal_sets)
}

#' Find the sets that are above a given percentile of the distribution of total distances between units within a sample_selection object
#' #'
#' @param sample_selection_object A list of admissable samples, created by a create_set() function (create_set_spatial_buffer() etc.)
#' @param distance_matrix A distance matrix, see function create_distance_matrix()
#' @param prob A probability, the function finds all sets that fall above the corresponding quantile of the distribution of total distances between units among the sets within a sample_selection object
#' @return A list of the  admissable sets that maximize the distance between the units in the set.
#' @export



find_sets_total_distance <- function(sample_selection_object, distance_matrix, prob=1) {
     
  if(!class(sample_selection_object)=="sample_selection")
        stop("sample_selection_object must be a sample_selection object, created with a create_set() function.")
     
     set_total_distances <- calculate_total_or_average_distances(sample_selection_object, distance_matrix, total=TRUE)
     
     sets_distance <- sample_selection_object[which(set_total_distances>=quantile(set_total_distances, prob))]
     
     return(sets_distance)
}



