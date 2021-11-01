#' Calculate the conditional log likelihood of a parameter combination for the weibull gap model
#'
#' @description Calculate the score of a parameter combination for the weibull gap model with two competing terminal events.
#'
#' @param phi parameter vector (named)
#' @param w current fixed value of random frailty
#' @param data nested data frame on the individual level
#' @param covar.names names of covariates in data frame
#'
#' @return scalar, log likelihood
#'
#' @examples
#' data <- simulate.data(c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  kd = 1, kx = 1, theta = .1, alpha = 1), n = 10, frailty.center = "linear")
#'
#' cond.likelihood.weibull.gap(w = 0, data = data, covar.names = "trt",
#' phi = c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  log.kd = 0, log.kx = 0, log.theta = -1, alpha = 1))
#'
#' @export

competing.cond.likelihood.weibull.gap <- function(phi, w, data, covar.names, frailty.center = "hazard"){
	# phi needs to have unconstrained parameters

	# need the following variables from the data
		# event: event indicator
		# terminal1: terminal event indicator
		# terminal2: terminal event indicator
		# g: vector of gap times
		# t: vector of calendar times
		# X: Model matrix with intercept

	beta.x <- grep("beta.x",names(phi))
	beta1 <- grep("beta1",names(phi))
	beta2 <- grep("beta2",names(phi))
	X <- cbind(1, data[,covar.names]) %>% as.matrix


	# frailty contribution
	if(frailty.center == "hazard"){
		frailty.contribution <-
		(1-data$event) * (-0.5 * phi["log.theta"] - 0.5/exp(phi["log.theta"]) * (w + 0.5*exp(phi["log.theta"]))^2)
	}else if(frailty.center == "linear"){
		frailty.contribution <-
		(1-data$event) * (-0.5 * phi["log.theta"] - 0.5/exp(phi["log.theta"]) * w^2)
	}

	sum(
	# contribution for recurrent events
	data$event * (phi["log.kx"] + (exp(phi["log.kx"])-1)*log(data$g) + X%*%phi[beta.x] + w) +
	-exp(X%*%phi[beta.x] + w) * data$g^exp(phi["log.kx"]) +

	# contribution for terminal event 1
	data$terminal1 * (phi["log.k1"] + (exp(phi["log.k1"])-1)*log(data$t) + X%*%phi[beta1] + phi["alpha1"] * w) +
	-exp(X%*%phi[beta1] + phi["alpha1"]*w) * data$t^exp(phi["log.k1"]) * (1-data$event) +

	# contribution for terminal event 1
	data$terminal2 * (phi["log.k2"] + (exp(phi["log.k2"])-1)*log(data$t) + X%*%phi[beta2] + phi["alpha2"] * w) +
	-exp(X%*%phi[beta2] + phi["alpha2"]*w) * data$t^exp(phi["log.k2"]) * (1-data$event) +

	# random frailty
	frailty.contribution
	)
}
