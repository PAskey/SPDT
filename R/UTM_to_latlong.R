
#Spatial data re-formatting

#' A function to convert from UTM to lat long
#'
#' @details Lat Long format is prefferred for some mapping functions. This function is used within SLD2R().
#' @title UTM_to_latlong
#' @name UTM_to_latlong
#' @param x,y,z x = utm easting, y = utm northing, z = zone
#' @keywords UTM latlong
#' @export
#' @importFrom sp CRS SpatialPoints spTransform
#' @examples
#' x = 407985
#' y = 5937037
#' z = 10
#' UTM_to_latlong(x,y,z)

UTM_to_latlong <- function(x,y,z) {
  xy = cbind(x,y)
  pro = sp::CRS(paste("+proj=utm +zone=",z,sep=""))
  utmcoor<-sp::SpatialPoints(xy, proj4string = pro)
  longlatcoor<-sp::spTransform(utmcoor,CRS("+proj=longlat"))
  return(longlatcoor)
}
