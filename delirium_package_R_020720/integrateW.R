#' Calculate expected value of the random frailty
#'
#' @description Given an individual's data frame, calculate the expected value of a transformation of the random frailty.
#'
#' @param d person-specific data frame.
#' @param phi parameter vector (named)
#' @param f.w function of the random frailty of which to take the expectation (ex. E(ln(w)))
#' @param J number of integration points for gaussian-laguerre quadrature
#' @param frailty distributional assumption on the random frailties. Currently either "gamma" or "lognormal".
#'
#' @return Single scalar estimate of frailty.
#'
#' @export

integrate.w <- function(d, phi, f.w, J=50, frailty = "gamma"){
	if(!frailty %in% c("gamma", "lognormal")){
		stop("frailty must be character string. either 'gamma' or 'lognormal'.")
	}

	quad.rules <- gauss.quad(n = J, kind = "laguerre")
	nodes <- quad.rules$nodes

	if(frailty == "lognormal"){
		sigma2 <- log(1 + 1/phi["gam"])
		mu <- -0.5*log(1 + 1/phi["gam"])
		frailty.contribution <- dlnorm(nodes, mu, sigma2^.5, log = T)
	}else{
		frailty.contribution <- -nodes*phi["gam"] + (phi["gam"] - 1)*log(nodes)
	}

	lnf <-
		# contribution from gauss-laguerre quadrature factorization
		nodes +
		# contribution from event count
		log(nodes) * (length(d$g) - 1) +
		- nodes*phi["rx"]^d$trt[1] * phi["bx"] * sum(d$g^phi["kx"])   +
		# contribution of death
		phi["alpha"]*log(nodes)  +
		-nodes^phi["alpha"] * phi["rd"] ^ d$trt[1] *  phi["bd"] * max(d$t)^phi["kd"] +
		# contribution from frailty variance (gamma)
		+ frailty.contribution

	sum(f.w(nodes) * quad.rules$weights * exp(lnf))/
		sum(quad.rules$weights * exp(lnf))
}


integrate.w2 <- function(d, phi, f.w, J=50, covar.names){
	beta.x <- grep("beta.x",names(phi))
	beta.d <- grep("beta.d",names(phi))
	X <- cbind(1, d[,covar.names]) %>% as.matrix

	quad.rules <- gauss.quad(n = J, kind = "hermite")
	nodes <- quad.rules$nodes

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
	nodes^2/2/phi["theta"]* sum(1-d$event)

	lapply(f.w, function(f){
		sum(f(nodes, phi) * quad.rules$weights * exp(lnf))/
	       	sum(quad.rules$weights * exp(lnf))
		}) %>% as.data.frame
}
