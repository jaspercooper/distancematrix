#' Find sets of units that are separated by a minimum distance (spatial buffer).
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param sims The number of iterations to run in order to find sets.
#' @param threshold The minimum distance one unit should be from another (expressed in same units as distance matrix)
#' @return A list of admissable sets of units that meet the spatial criterion.
#' @export
create_set_spatial_buffer <- function(distance_matrix,threshold,sims = 1000, probability_weights = NULL,...){
     exclude_set <- find_proximate_units(distance_matrix = distance_matrix,
                                         threshold = threshold,...)


     if(is.null(probability_weights)){

     proximity_summary <- sapply(exclude_set,length)
     probability_weights <- proximity_summary/sum(proximity_summary)
     probability_weights <- probability_weights/sum(probability_weights)

     }


     sample_list <- 1:dim(distance_matrix)[1]

     create_set <- function(exclude_set,sample_list,probability_weights){
          while(!all(is.na(unlist(exclude_set)))){
               remove <-
                    sample(x = subset(sample_list,is.na(sample_list)==F),
                           size = 1,replace = F,prob = subset(probability_weights,is.na(sample_list)==F))

               sample_list[remove] <- NA

               remove_list <- exclude_set[[remove]]

               for(i in 1:length(exclude_set)){
                    if(any(remove_list%in%exclude_set[[i]])){
                         exclude_set[[i]] <- exclude_set[[i]][-which(exclude_set[[i]]%in%remove_list)]
                    }else{}
               }

               exclude_set[remove_list] <- NA


          }

          # Return the villages that made it through
          return(which(sapply(exclude_set,class)=="integer"))
     }

     admissable_sets <-
          replicate(n = sims,
                    expr = create_set(exclude_set = exclude_set,
                                      sample_list = sample_list,
                                      probability_weights = probability_weights))

     class(admissable_sets) <- "sample_selection"

     return(admissable_sets)

}


#' For each unit, find the units that are within some distance
#' @param distance_matrix A distance matrix (see create_distance_matrix()).
#' @param threshold The minimum distance one unit should be from another (expressed in same units as distance matrix)
#' @return A list of vectors indicating, for each unit, the units within the spatial threshold.
#' @export
find_proximate_units <- function(distance_matrix,threshold,...) {
     adjacency_matrix <-
          create_adjacency_matrix(distance_matrix = distance_matrix,
                                  threshold = threshold,
                                  ...)
     proximate_unit_list <- sapply(1:(dim(adjacency_matrix)[1]),function(i){
          which(adjacency_matrix[i,]==1)
     })
     return(proximate_unit_list)
}



