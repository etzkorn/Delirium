#' Draw from weibull distrubution
#'
#' @description Simulate numbers from weibull distribution using different parameter specification than the base R function rweibull.
#'
#' @param n replicate count
#' @param k shape parameter from rweibull.
#' @param b transformed version of scale from rweibull. Specifically scale^-k or scale^-shape. The hazard will be proportional to b.
#' @param shape shape parameter from rweibull.
#' @param scale shape parameter from rweibull.
#' @param rh relative hazard which compared to the hazard defined by shape and scale alone.
#'
#' @return Vector of n replicates
#'
#' @examples
#' set.seed(100)
#' rweibull(5, shape = 3, scale = 2)
#' set.seed(100)
#' rweib(5, k = 3, b = 2^-3)
#'
#' @export

rweib <- function(n,k,b){
	rweibull(n, shape = k, scale = b^(-1/k))
}

rweibRH <- function(n, shape ,scale , rh){
	rweibull(n, shape = shape, scale = scale * rh^(-1/shape))
}

pweibRH <- function(q, shape, scale, rh){
	pweibull(q, shape = shape, scale = scale * rh^(-1/shape))
}

