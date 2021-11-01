#' Fit Weibull Gap Model
#'
#' @description Fit Weibull Gap Model
#'
#' @param data data frame
#' @param e.iter number of times to recalculate the expectations
#' @param m.iter each time a new expectation is calculated, how many iterations to maximize parameters
#' @param par.start starting values for parameter estimates
#' @param frailty distributional assumption on random frailty effects
#'
#' @return List with estimate vector, sequence of estimates from maximization procedure, standard errors, and individual frailty estimates.
#'
#' @export

weibull.gap.nr.step <- function(phi, phi0, data, frailty = "gamma"){
	score <- score.weibull.gap(phi = phi, phi0 = phi0, data = data)
	dscore <- dscore.weibull.gap(phi = phi, phi0 = phi0, data = data)
	phi.next <- phi - score %*% solve(dscore)
	p <- as.vector(phi.next)
	names(p) <- colnames(phi.next)
	return(p)
}
weibull.gap.nr.step2 <- function(phi, data, covar.names){
	score <- score.weibull.gap2(phi = phi, data = data, covar.names = covar.names)
	dscore <- dscore.weibull.gap2(phi = phi, data = data, covar.names = covar.names)
	phi.next <- phi - score %*% solve(dscore)
	p <- as.vector(phi.next)
	names(p) <- colnames(phi.next)
	return(p)
}
