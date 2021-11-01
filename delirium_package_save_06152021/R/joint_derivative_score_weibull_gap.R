#' Calculate the derivative of score of a parameter combination for the weibull gap model
#'
#' @description Calculate the derivative of score of a parameter combination for the weibull gap model
#'
#' @param phi parameter vector (named)
#' @param data nested data frame on the individual level
#' @param covar.names covariate names
#'
#' @return Matrix of score derivatives, same dimension as phi.
#'
#' @export
#'

dscore.weibull.gap <- function(phi, data, covar.names){
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

	# gamma score derivative (same for both type of centers)
	s.theta.theta = sum((1 - data$event) * (1/2/phi["theta"]^2 - 1/phi["theta"]^3 * data$Ew2))

	# beta d score derivative
	s.bd.bd = - t(X) %*% diag(as.vector((1-data$event) * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eeaw)) %*% X
	s.bd.kd = t(X) %*% (- (1-data$event) * exp(X %*% phi[beta.d]) * data$t^phi["kd"] * log(data$t) * data$Eeaw )
	s.bd.alpha = t(X) %*% (-(1-data$event)*data$t^phi["kd"]* data$Eweaw * exp(X%*% phi[beta.d]))

	# kd score derivative
	s.kd.kd = sum(-data$terminal/phi["kd"]^2 -
		(1 - data$event) * log(data$t)^2 * data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eeaw)
	s.kd.bd = t(X) %*% (- (1-data$event) * exp(X %*% phi[beta.d]) * data$t^phi["kd"] * log(data$t) * data$Eeaw )
	s.kd.alpha = sum(-(1-data$event)*log(data$t)*data$t^phi["kd"] * exp(X %*% phi[beta.d]) * data$Eweaw)

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
