#' Calculate the score of a parameter combination for the weibull gap model
#'
#' @description Calculate the score of a parameter combination for the weibull gap model
#'
#' @param phi parameter vector (named)
#' @param phi0 parameter vector (named) to be supplied to the random effects estimation.
#' @param data nested data frame on the individual level
#' @param frailty distributional assumption on frailty term ("gamma" or "lognormal")
#'
#' @return Vector of scores, same length as phi.
#'
#' @export
score.weibull.gap <- function(phi, phi0, data, frailty = "gamma"){
	map(data$data, ~ score.weibull.single(phi, phi0, .)) %>%
		do.call(what = "rbind") %>% colSums
}
score.weibull.single <- function(phi, phi0, data, frailty = "gamma"){
	# calculate expectations of transformations of random effects
	# conditinal on paramaters from previous step (phi0)
	Ew <- integrate.w(data, phi = phi0, f.w = function(w) w)
	Elnw <- integrate.w(data, phi = phi0, f.w = log)
	Elnwwa <- integrate.w(data, phi = phi0, f.w = function(w) log(w)*w^phi["alpha"])
	Ewa <- integrate.w(data, phi = phi0, f.w = function(w) w^phi["alpha"])

	# calculate score of a given parameter combination from the current step (phi)
	s.alpha = Elnw - Elnwwa * phi["rd"]^data$trt[1] * phi["bd"] * data$y[1]^phi["kd"]
	if(frailty=="gamma"){
		s.gamma = 1 + log(phi["gam"]) - digamma(phi["gam"]) + Elnw - Ew
	}else{
		s.gamma = -1/2/phi["gam"]^2/ln(1+1/phi["gam"]) *
			(1 + 0.5*(Elnw+ 0.5*ln(1+1/phi["gam"]) )^2/ln(1+1/phi["gam"]) +
			 	0.5*(Elnw + 0.5*ln(1+1/phi["gam"]) ))
	}
	s.rd = data$trt[1]/phi["rd"] - Ewa * data$trt[1] * phi["bd"] * data$y[1]^phi["kd"]
	s.kd = 1/phi["kd"] + log(data$y[1]) -
		Ewa * phi["rd"]^data$trt[1] * phi["bd"] * log(data$y[1]) *data$y[1]^phi["kd"]
	s.bd = 1/phi["bd"] - Ewa * phi["rd"]^data$trt[1] * data$y[1]^phi["kd"]
	s.rx = data$trt[1]*data$x[1]/phi["rx"] -
		Ew* data$trt[1] * phi["bx"] * sum(data$g^phi["kx"])
	s.bx = data$x[1]/phi["bx"] - Ew * phi["rx"]^data$trt[1] * sum(data$g^phi["kx"])
	s.kx = data$x[1]/phi["kx"] + sum(log(data$g[-length(data$g)])) -
		Ew * phi["rx"]^data$trt[1] * phi["bx"] * sum(log(data$g) * data$g^phi["kx"])

	# return
	c(bd = unname(s.bd), kd = unname(s.kd), rd = unname(s.rd),
	  bx = unname(s.bx), kx = unname(s.kx), rx = unname(s.rx),
	  gam = unname(s.gamma), alpha =unname( s.alpha))
}

score.weibull.gap2 <- function(phi, data, covar.names){
	# need the following variables from the data
		# event: event indicator
		# terminal: terminal event indicator
		# g: vector of gap times
		# t: vector of calendar times
		# Ew, Ew2, Eew, Eeaw: expectations of random effects
		# X: Model matrix with intercept

	beta.x <- grep("beta.x",names(phi))
	beta.d <- grep("beta.d",names(phi))
	X <- cbind(1, data[,covar.names]) %>% as.matrix

	# calculate score of a given parameter combination from the current step (phi)
	s.theta = sum((1-data$event)/2 * (data$Ew2 / phi["theta"]^2 - phi["theta"]))
	s.alpha = sum(data$terminal * data$Ew -
		(1-data$event) * data$Eweaw * data$t^phi["kd"] * exp(X %*% phi[beta.d]))

	s.kd = sum(data$terminal*(1/phi["kd"] + log(data$t)) -
		(1-data$event) * data$Eeaw * exp(X %*% phi[beta.d]) *
		log(data$t) * data$t^phi["kd"])
	s.kx = sum(data$event* (1/phi["kx"] + log(data$g)) -
		data$Eew * exp(X %*% phi[beta.x]) *
		log(data$g) * data$g^phi["kx"])

	s.bd = t(X) %*% (data$terminal - (1-data$event) * exp(X %*% phi[beta.d]) * data$Eeaw * data$t^phi["kd"])
	s.bx = t(X) %*% (data$event - exp(X %*% phi[beta.x]) * data$Eew * data$g^phi["kx"])

	# return
	score <-
	c(unname(s.bd), unname(s.bx),
	  s.kd = unname(s.kd), s.kx = unname(s.kx),
	  s.theta = unname(s.theta), s.alpha = unname(s.alpha))
	names(score)[1:4] <- names(phi)[c(beta.d,beta.x)]
	return(score)
}
