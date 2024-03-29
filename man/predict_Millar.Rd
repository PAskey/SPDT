% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_Millar.R
\name{predict_Millar}
\alias{predict_Millar}
\title{Predict Millar's selectivity}
\source{
https://www.stat.auckland.ac.nz/~millar/selectware/
}
\usage{
predict_Millar(
  rtype,
  classes,
  meshSizes,
  theta,
  rel.power = NULL,
  theta_min_mesh = 1
)
}
\arguments{
\item{rtype}{a character string indicating which method for the estimation of selection
  curves should be used:
  \code{"norm.loc"} for normal with common spread method,
  \code{"norm.sca"} for normal with variable spread method,
  \code{"lognorm"} for lognormal method,
  \code{"binorm.sca"} for bi-normal method,
  \code{"bilognorm"} for bi-lognormal method,
  \code{"tt.logistic"} for control and logistic method,
  \code{"gamma"} for gamma method.
  
@param meshSizes a vector of gillnet mesh sizes in mm
@param classes a vector of fork lengths (mm) for which you want to calculate selectivity
@param theta a vector of selectivity parameters
@param rel.power a vector of the relative fishing power of each mesh size
@param theta_min_mesh the smallest mesh used in the fitting process to acquire theta values. Defaults to 1 which is RIC net smallest mesh used to fit BC nets.}
}
\description{
This function returns the predicted selectivity of a gillnet gang based Millar selectivity functions.
}
\details{
Function adapted from TropFishR and the selectivity functions provided by
  Prof. Dr. Russell Millar (https://www.stat.auckland.ac.nz/~millar/).
  This function might be useful if you are trying to use published fit values instead of your own fit.
  Otherwise, you can access predictions within model fit attributes. e.g. models[[5]]$selection_ogive_mat
  Until now following curves are incorporated:
  \code{"norm.loc"} for a normal curve with common spread,
   \code{"norm.sca"} for a normal curve with variable spread,
   \code{"lognorm"} for a lognormal curve,
   \code{"binorm.sca"} for a bi-normal curve,
   \code{"bilognorm"} for a bi-lognormal curve,
   \code{"tt.logistic"} for a control and logistic curve.
}
\references{
Millar, R. B., Holst, R., 1997. Estimation of gillnet and hook selectivity
 using log-linear models. \emph{ICES Journal of Marine Science: Journal du Conseil},
 54(3), 471-477.
}
