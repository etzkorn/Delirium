#' Fit Weibull Gap Model with Optim
#'
#' @description Fit Weibull Gap Model using direct maximization of marginal likelihood with optim with two competing terminal events.
#'
#' @param data data frame
#' @param covar.names vector of characters of variable names to include as linear predictor in model
#' @return List with estimate vector, sequence of estimates from maximization procedure, standard errors, and individual frailty estimates.
#'
#' @export
#' @examples
#' data <- simulate.data(c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  kd = 1, kx = 1, theta = .1, alpha = 1), n = 10, frailty.center = "linear")
#'
#' weibull.gap.model.optim(data = data, covar.names = "trt",
#' phi = c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  log.kd = 0, log.kx = 0, log.theta = -1, alpha = 1))

competing.weibull.gap.optim <- function(data, covar.names="trt",
			    GH = 30){
	# check variable names
	if(! all(c("id","g","t", "event", "terminal1", "terminal2") %in% colnames(data))){
		stop("Variables id (subject id), g (gap time), t (time since start),
		event (event indicator), terminal1, terminal2 (terminal event indicators)
		must be present in data.")
	}

	par.start = c(rep(0,length(covar.names)*3 + 9))
	names(par.start) <- c(paste0(rep(c("beta","eta"),3),rep(c("R","D","D2"), each = 2)),
		          "theta", "alpha1","alpha2",
		          paste0(covar.names,c("R","D","D2")))
	optim.out <-
	optim(par = par.start,
	      fn = competing.marg.likelihood.weibull.gap,
	      data = data,
	      GH = GH,
	      covar.names = covar.names,
	      method = "L-BFGS-B",
	      hessian = T)
	# Prepare Results
	model.result <-
		list(estimates = optim.out$par,
		     se = diag(solve(optim.out$hessian)) ^.5)
	return(model.result)
}
