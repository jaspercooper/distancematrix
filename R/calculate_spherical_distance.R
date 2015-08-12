#' Calculate the spherical distance between two coordinates
#'
#' @param long1 Longitude of point 1
#' @param lat1 Latitude of point 1
#' @param long2 Longitude of point 2
#' @param lat2 Latitude of point 2
#' @param km Express distance in kilometers
#' @param miles Express distance in miles
#' @param meters Express distance in meters
#' @param feet Express distance in feet
#' @return The spherical distance between point 1 and point 2.
#' @export

calculate_spherical_distance <-
     function(long1, lat1, long2, lat2,km = NULL,miles = NULL,meters = NULL,feet = NULL,...) {

          degrees_to_radians <- function(x) {
               return(x * pi / 180)
          }

          if (sum(!is.null(c(km,miles,meters))) > 1)
               stop("You must select only one unit of measurement (km, miles, meters).")

          if (!is.null(km))
               Radius <- 6371 # Earth mean radius in km
          if (!is.null(miles))
               Radius <- 3959 # Earth mean radius in miles
          if (!is.null(meters))
               Radius <- 6371 * 1000 # Earth mean radius in meters
          if (!is.null(feet))
               Radius <- 3959 * 5280 # Earth mean radius in feet

          if(all(is.null(c(km,miles,meters,feet)))){
               warning("Defaulting to kilometers.")
               Radius <- 6371
               }

          delta.long <- degrees_to_radians(long2 - long1)
          delta.lat  <- degrees_to_radians(lat2 - lat1)
          a <- sin(delta.lat / 2) ^ 2 + cos(degrees_to_radians(lat1)) *
               cos(degrees_to_radians(lat2)) * sin(delta.long / 2) ^ 2
          c <- 2 * atan2(sqrt(a),sqrt(1 - a))

          distance <- Radius * c

          return(c(distance = distance))
     }


