#' Calculate the distance of each unit to every other unit.
#'
#' @param latitudes A vector of latitudes of length N
#' @param longitudes A vector of longitudes of length N
#' @param ... Takes distances arguments. Set km, meters, miles or feet to true in order to change the measure of distance that is output by the function.
#' @return An N x N distance matrix
#' @export
create_distance_matrix <- function(latitudes,longitudes,...){

     if(length(latitudes)!=length(longitudes))stop(
          "Latitudes and longitudes vectors are not of same length. They should correspond to the same unit."
     )

     lonlat <- cbind(longitudes,latitudes)

     distance_matrix <- apply(lonlat,1,function(origin){
          calculate_spherical_distance(long1 = origin[1],lat1 = origin[2],
                                       long2 = lonlat[,1],lat2 = lonlat[,2],
                                       ...
          )
     })

     dimnames(distance_matrix) <- list(1:dim(distance_matrix)[1],
                                       1:dim(distance_matrix)[1])

     return(distance_matrix)
}

