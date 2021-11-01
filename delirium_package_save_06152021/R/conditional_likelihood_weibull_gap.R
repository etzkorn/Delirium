#' Calculate the conditional log likelihood of a parameter combination for the weibull gap model
#'
#' @description Calculate the score of a parameter combination for the weibull gap model
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

cond.likelihood.weibull.gap <- function(phi, w, data, covar.names,
			    frailty.center,
			    frailty.distribution){
	# phi needs to have unconstrained parameters

	# need the following variables from the data
		# event: event indicator
		# terminal: terminal event indicator
		# g: vector of gap times
		# t: vector of calendar times
		# X: Model matrix with intercept

	beta.x <- grep("beta.x",names(phi))
	beta.d <- grep("beta.d",names(phi))
	X <- cbind(1, data[,covar.names]) %>% as.matrix

	# frailty contribution
	if(frailty.center == "linear" & frailty.distribution == "gamma"){
		stop("Cannot assume E(log(w)) = 0 when w ~ gamma")
	}else if(frailty.center == "hazard" & frailty.distribution == "gamma"){
		gamma.variance <- log(exp(phi["log.theta"]) + 1)
		frailty.contribution <-
		(1-data$event) * (dgamma(exp(w),
				 shape = 1/gamma.variance,
				 scale = gamma.variance, log = T) + w)
	}else if(frailty.center == "hazard" & frailty.distribution == "logN"){
		frailty.contribution <-
		(1-data$event) * (-0.5 * phi["log.theta"] - 0.5/exp(phi["log.theta"]) * (w + 0.5*exp(phi["log.theta"]))^2)
	}else if(frailty.center == "linear" & frailty.distribution == "logN"){
		frailty.contribution <-
		(1-data$event) * (-0.5 * phi["log.theta"] - 0.5/exp(phi["log.theta"]) * w^2)
	}

	sum(
	# contribution for observed recurrent events
	data$event * (phi["log.kx"] + (exp(phi["log.kx"])-1)*log(data$g) + X%*%phi[beta.x] + w) +

	# contribution for observed terminal events
	data$terminal * (phi["log.kd"] + (exp(phi["log.kd"])-1)*log(data$t) + X%*%phi[beta.d] + phi["alpha"] * w) +

	# all event gap times
	-exp(X%*%phi[beta.x] + w) * data$g^exp(phi["log.kx"]) +

	# all total observation times
	-exp(X%*%phi[beta.d] + phi["alpha"]*w) * data$t^exp(phi["log.kd"]) * (1-data$event) +

	# random frailty
	frailty.contribution
	)
}
