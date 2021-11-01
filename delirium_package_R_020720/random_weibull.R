#' Draw from weibull distrubution
#'
#' @description Simulate numbers from weibull distribution using different parameter specification than the base R function rweibull.
#'
#' @param n replicate count
#' @param k shape parameter from rweibull.
#' @param b transformed version of scale from rweibull. Specifically scale^-k or scale^-shape. The hazard will be proportional to b.
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
