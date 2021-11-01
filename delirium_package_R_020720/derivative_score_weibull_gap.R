#' Calculate the derivative of score of a parameter combination for the weibull gap model
#'
#' @description Calculate the derivative of score of a parameter combination for the weibull gap model
#'
#' @param phi parameter vector (named)
#' @param phi0 parameter vector (named) to be supplied to the random effects estimation.
#' @param data nested data frame on the individual level
#' @param frailty distributional assumption on frailty term ("gamma" or "lognormal")
#'
#' @return Vector of scores, same length as phi.
#'
#' @export
#'
dscore.weibull.gap <- function(phi, phi0, data){
	map(data$data, ~ dscore.weibull.single(phi, phi0, .)) %>%
		abind(along = 3) %>%
		apply(1:2, sum)
}
dscore.weibull.single <- function(phi, phi0, data, frailty){
	# calculate expectations of transformations of random effects
	# conditinal on paramaters from previous step (phi0)
	Ew <- integrate.w(data, phi = phi0, f.w = function(w) w)
	Elnw <- integrate.w(data, phi = phi0, f.w = log)
	Elnwwa <- integrate.w(data, phi = phi0, f.w = function(w) log(w)*w^phi["alpha"])
	Ewa <- integrate.w(data, phi = phi0, f.w = function(w) w^phi["alpha"])
	Elnw2wa <- integrate.w(data, phi = phi0, f.w = function(w) (log(w))^2*w^phi["alpha"])

	# alpha score derivatives
	s.alpha.alpha = -Elnw2wa * phi["rd"]^data$trt[1] * phi["bd"] * data$y[1]^phi["kd"]
	s.alpha.bd = - Elnwwa * phi["rd"]^data$trt[1] * data$y[1]^phi["kd"]
	s.alpha.kd = - Elnwwa * phi["rd"]^data$trt[1] * phi["bd"] * log(data$y[1]) * data$y[1]^phi["kd"]
	s.alpha.rd = - Elnwwa * data$trt[1] * phi["bd"] * data$y[1]^phi["kd"]

	# gamma score derivative
	s.gam.gam = 1/phi["gam"] - trigamma(phi["gam"])

	# bd score derivative
	s.bd.bd = -1/phi["bd"]^2
	s.bd.rd = -Ewa * data$trt[1] * data$y[1]^phi["kd"]
	s.bd.kd = -Ewa * phi["rd"]^data$trt[1] * log(data$y[1]) * data$y[1]^phi["kd"]
	s.bd.alpha = - Elnwwa * phi["rd"]^data$trt[1] * data$y[1]^phi["kd"]

	# kd score derivative
	s.kd.kd = -1/phi["kd"]^2 - Ewa * phi["rd"]^data$trt[1] * phi["bd"] * log(data$y[1])^2 * data$y[1]^phi["kd"]
	s.kd.bd = -Ewa * phi["rd"]^data$trt[1] * log(data$y[1]) * data$y[1]^phi["kd"]
	s.kd.rd = -Ewa * data$trt[1] * phi["bd"] * log(data$y[1]) * data$y[1]^phi["kd"]
	s.kd.alpha = -Elnwwa * phi["rd"]^data$trt[1] * phi["bd"] * log(data$y[1]) * data$y[1]^phi["kd"]

	# rd score derivative
	s.rd.rd = -1/phi["rd"]^2
	s.rd.bd = -Ewa * data$trt[1] * data$y[1]^phi["kd"]
	s.rd.kd = -Ewa * data$trt[1] * phi["bd"] * log(data$y[1]) * data$y[1]^phi["kd"]
	s.rd.alpha = -Elnwwa * data$trt[1] * phi["bd"] * data$y[1]^phi["kd"]

	# bx score derivative
	s.bx.bx = -data$x[1]/phi["bx"]^2
	s.bx.rx = -Ew * data$trt[1] * sum(data$g^phi["kx"])
	s.bx.kx = -Ew * phi["rx"]^data$trt[1] * sum(log(data$g) * data$g^phi["kx"])

	# rx score derivative
	s.rx.rx = - data$x[1] * data$trt[1]/phi["rx"]^2
	s.rx.bx = - Ew * data$trt[1] * sum(data$g^phi["kx"])
	s.rx.kx = - Ew * data$trt[1] * phi["bx"] * sum(log(data$g) * data$g^phi["kx"])

	# kx score derivative
	s.kx.kx = -data$x[1]/phi["kx"]^2 -
		Ew * phi["rx"]^data$trt[1] * phi["bx"] * sum(log(data$g)^2 * data$g^phi["kx"])
	s.kx.rx = -Ew * phi["bx"] * data$trt[1] * sum(log(data$g) * data$g^phi["kx"])
	s.kx.bx = -Ew * phi["rx"]^data$trt[1] * sum(log(data$g) * data$g^phi["kx"])

	# construct derivative matrix
	rbind(bd = c(bd = unname(s.bd.bd), kd = unname(s.bd.kd), rd = unname(s.bd.rd),
		 bx = 0, kx = 0, rx = 0, gam = 0, alpha = unname(s.bd.alpha)),
		 kd = c(s.kd.bd, s.kd.kd, s.kd.rd, 0, 0, 0, 0, s.kd.alpha),
		 rd = c(s.rd.bd, s.rd.kd, s.rd.rd, 0, 0, 0, 0, s.rd.alpha),
		 bx = c(0, 0, 0, s.bx.bx, s.bx.kx, s.bx.rx, 0, 0),
		 kx = c(0, 0, 0, s.kx.bx, s.kx.kx, s.kx.rx, 0, 0),
		 rx = c(0, 0, 0, s.rx.bx, s.rx.kx, s.rx.rx, 0, 0),
		 gam = c(0, 0, 0, 0, 0, 0, s.gam.gam, 0),
		 alpha = c(s.alpha.bd, s.alpha.kd, s.alpha.rd, 0, 0, 0, 0, s.alpha.alpha))
}

dscore.weibull.gap2 <- function(phi, data, covar.names){
	# need the following variables from the data
	# event: event indicator
	# terminal: terminal event indicator
	# g: vector of gap times
	# t: vector of calendar times
	# Ew2, Eew, Eeaw, Ew2eaw, Eweaw: expectations of random effects
	# X: Model matrix with intercept

	beta.x <- grep("beta.x",names(phi))
	beta.d <- grep("beta.d",names(phi))
	X <- cbind(1, data[,covar.names]) %>% as.matrix

	# alpha score derivatives
	s.alpha.alpha =  sum(- (1-data$event) * data$t^phi["kd"] * exp(X %*%phi[beta.d]) * data$Ew2eaw)
	s.alpha.bd = t(X) %*% (-(1-data$event)*data$t^phi["kd"]* data$Eweaw * exp(X%*% phi[beta.d]))
	s.alpha.kd = sum(-(1-data$event)*log(data$t)*data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eweaw)

	# gamma score derivative
	s.theta.theta = sum((1 - data$event) * (1/2/phi["theta"]^2 - 1/phi["theta"]^3 * data$Ew2))

	# beta d score derivative
	s.bd.bd = - t(X) %*% diag(as.vector((1-data$event) * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eeaw)) %*% X
	s.bd.kd = t(X) %*% (- (1-data$event) *exp(X %*% phi[beta.d]) * data$Eeaw * data$t^phi["kd"] * log(data$t))
	s.bd.alpha = t(X) %*% (-(1 - data$event) * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eweaw)

	# kd score derivative
	s.kd.kd = sum(-data$terminal/phi["kd"]^2 -
		(1 - data$event) * log(data$t)^2 * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eeaw)
	s.kd.bd = t(X) %*% (- (1 - data$event) * log(data$t) * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eeaw)
	s.kd.alpha = sum(- (1-data$event) * log(data$t) * data$t^phi["kd"] * exp(X %*% phi[beta.d])* data$Eweaw)

	# beta x score derivative
	s.bx.bx = - t(X) %*% diag(as.vector(data$g^phi["kx"] * exp(X %*% phi[beta.x]) * data$Eew)) %*% X
	s.bx.kx =  t(X) %*% (-exp(X %*% phi[beta.x]) * data$Eew * log(data$g) * data$g^phi["kx"])

	# kx score derivative
	s.kx.kx = -sum(data$event)/phi["kx"]^2 -
		sum( log(data$g)^2 * data$g^phi["kx"] * data$Eew * exp(X %*% phi[beta.x]))
	s.kx.bx = t(X) %*% (-log(data$g) *data$g^phi["kx"] *exp(X%*%phi[beta.x]) * data$Eew)

	# construct derivative matrix
	dscore <-
	rbind(
	cbind(s.bd.bd, matrix(0, nrow = 2, ncol = 2), s.bd.kd, matrix(0, ncol = 2, nrow = 2), s.bd.alpha),
	cbind(matrix(0, nrow = 2, ncol = 2), s.bx.bx,  matrix(0, ncol = 1, nrow = 2), s.bx.kx, matrix(0, ncol = 2, nrow = 2) ),
	cbind(t(s.kd.bd), 0, 0, s.kd.kd, 0, 0, s.kd.alpha),
	cbind(0,0,t(s.kx.bx), 0, s.kx.kx, 0, 0),
	cbind(0,0,0,0,0,0,s.theta.theta, 0),
	cbind(t(s.alpha.bd), 0, 0, s.alpha.kd, 0, 0, s.alpha.alpha)
	)

	rownames(dscore) <- names(phi)
	colnames(dscore) <- names(phi)
	return(dscore)
}
