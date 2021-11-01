#' Weibull Gap Newton Raphson Step
#'
#' @description Calculate the next parameter set in the newton raphson sequence for the weibull gap model.
#'
#' @param data data frame
#' @param phi starting values for parameter estimates
#' @param covar.names character vector of covariate names for linear predictor in model.
#'
#' @return named numeric vector of estimates for next NR step
#'
#' @export

weibull.gap.nr.step <- function(phi, data, covar.names, frailty.center = "linear"){
	score <- score.weibull.gap(phi = phi, data = data, covar.names = covar.names, frailty.center = frailty.center)
	dscore <- dscore.weibull.gap(phi = phi, data = data, covar.names = covar.names)
	phi.next <- phi - score %*% solve(dscore)
	p <- as.vector(phi.next)
	names(p) <- colnames(phi.next)
	return(p)
}
