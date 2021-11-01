#' Draw from log normal distrubution
#'
#' @description Simulate numbers from lognomral distribution using different parameter specification than the base R function lnorm
#'
#' @param n replicate count
#' @param mean mean of random variable
#' @param var variance of random variable
#'
#' @return Vector of n replicates
#'
#' @examples
#' set.seed(100)
#' rlognorm(5, mean = 3, var = 5)
#'
#' @export

rlognorm <- function(n, mean, var){
	sig2 = log(var/mean^2 + 1)
	mu = log(mean) - sig2/2
	rlnorm(n, meanlog = mu, sdlog = sig2^.5)
}
