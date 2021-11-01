#' Calculate expected value of the random frailty
#'
#' @description Given an individual's data frame, calculate the expected value of a transformation of the random frailty.
#'
#' @param d person-specific data frame.
#' @param phi parameter vector (named)
#' @param f.w names list of functions of the random frailty of which to take the expectation (ex. E(ln(w)))
#' @param J number of integration points for gaussian-laguerre quadrature
#' @param covar.names names of model covariates for linear predictor
#'
#' @return Single scalar estimate of frailty.
#'
#' @export

integrate.w <- function(d, phi, f.w, J=50, covar.names, frailty.center = "linear"){
	beta.x <- grep("beta.x",names(phi))
	beta.d <- grep("beta.d",names(phi))
	X <- cbind(1, d[,covar.names]) %>% as.matrix

	quad.rules <- gauss.quad(n = J, kind = "hermite")
	nodes <- quad.rules$nodes

	if(frailty.center == "linear"){
		frailty.contribution <- nodes^2/2/phi["theta"]* sum(1-d$event)
	}else if(frailty.center == "hazard"){
		frailty.contribution <- (nodes + 0.5*phi["theta"])^2 /2/phi["theta"]*sum(1-d$event)
	}

	lnf <-
	# contribution from gauss-laguerre quadrature factorization
	nodes^2 +
	# event contribution
	nodes * sum(d$event) -
		exp(nodes) * sum(exp(X %*% phi[beta.x]) * d$g^phi["kx"]) +
	# terminal contribution
	nodes * phi["alpha"] * sum(d$terminal) -
		exp(phi["alpha"]*nodes) * sum(exp(X %*% phi[beta.d]) * d$t^phi["kd"] * (1-d$event)) -
	# frailty density contribution
		frailty.contribution

	lapply(f.w, function(f){
		sum(f(nodes, phi) * quad.rules$weights * exp(lnf))/
	       	sum(quad.rules$weights * exp(lnf))
		}) %>% as.data.frame
}
