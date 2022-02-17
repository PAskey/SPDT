#' @title Predict Millar's selectivity
#
#' @description  This function returns the predicted selectivity of a gillnet gang based Millar selectivity functions. 
#' @export
#' @param rtype a character string indicating which method for the estimation of selection
#'    curves should be used:
#'    \code{"norm.loc"} for normal with common spread method,
#'    \code{"norm.sca"} for normal with variable spread method,
#'    \code{"lognorm"} for lognormal method,
#'    \code{"binorm.sca"} for bi-normal method,
#'    \code{"bilognorm"} for bi-lognormal method,
#'    \code{"tt.logistic"} for control and logistic method,
#'    \code{"gamma"} for gamma method.
#'    
#'  @param meshSizes a vector of gillnet mesh sizes in mm
#'  @param classes a vector of fork lengths (mm) for which you want to calculate selectivity
#'  @param th a vector of selectivity parameters
#'
#' @source https://www.stat.auckland.ac.nz/~millar/selectware/
#'
#' @details Function adapted from TropFishR and the selectivity functions provided by
#'   Prof. Dr. Russell Millar (https://www.stat.auckland.ac.nz/~millar/).
#'   This function might be useful if you are trying to use published fit values instead of your own fit.
#'   Otherwise, yo ucan access predictions within model fit attributes. e.g. models[[5]]$selection_ogive_mat
#'   Until now following curves are incorporated:
#'   \code{"norm.loc"} for a normal curve with common spread,
#'    \code{"norm.sca"} for a normal curve with variable spread,
#'    \code{"lognorm"} for a lognormal curve,
#'    \code{"binorm.sca"} for a bi-normal curve,
#'    \code{"bilognorm"} for a bi-lognormal curve,
#'    \code{"tt.logistic"} for a control and logistic curve.
#'
#' @references
#'  Millar, R. B., Holst, R., 1997. Estimation of gillnet and hook selectivity
#'  using log-linear models. \emph{ICES Journal of Marine Science: Journal du Conseil},
#'  54(3), 471-477.

predict_Millar <- function(rtype, classes, meshSizes, th, rel.power = NULL) {

#you need to use the $par instead of $estimates from model fit in order to make predictions.  
# If for some reason you only have access to estmimates, then need to check source code and back transform.

  
   
  # Input checks
  if(sum(sort(meshSizes)==meshSizes) != length(meshSizes))
    stop("Mesh size must be in ascending order!")
  
  if(is.null(rel.power)) rel.power <- rep(1,length(meshSizes))
  
  if(!is.null(rel.power) & length(rel.power) != length(meshSizes))
    stop("Length of rel.power should match length meshSizes")
  
  r <- TropFishR:::rtypes_Millar(rtype) #Get selection curve function
  
  rmatrix = outer(classes, meshSizes, r, th)
  rmatrix <- t(t(rmatrix) * rel.power)
  v = apply(rmatrix,1,sum,na.rm=TRUE)
  v = v/max(v)
  return(v)
}

