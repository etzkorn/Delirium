#' Calculate the score of a parameter combination for the weibull gap model
#'
#' @description Calculate the score of a parameter combination for the weibull gap model
#'
#' @param phi parameter vector (named)
#' @param data nested data frame on the individual level
#' @param covar.names names of covariates in data frame
#'
#' @return Vector of scores, same length as phi.
#'
#' @export

score.weibull.gap <- function(phi, data, covar.names, frailty.center = "linear"){
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

	# calculate

	# calculate score of a given parameter combination from the current step (phi)
	if(frailty.center == "linear"){
		s.theta = sum((1-data$event) * -1/2 * (1/phi["theta"] - data$Ew2 / phi["theta"]^2 ))
	}else{
		s.theta = sum((1-data$event) * -1/2 * (1/phi["theta"] - data$Ew2/phi["theta"]^2 + .25 ))
	}

	s.alpha = sum(data$terminal * data$Ew -
		(1-data$event) * data$Eweaw * data$t^phi["kd"] * exp(X %*% phi[beta.d]))

	s.kd = sum(data$terminal*(1/phi["kd"] + log(data$t)) -
		(1-data$event) * exp(X %*% phi[beta.d]) *
		log(data$t) * data$t^phi["kd"] * data$Eeaw)
	s.kx = sum(data$event* (1/phi["kx"] + log(data$g)) -
		data$Eew * exp(X %*% phi[beta.x]) *
		log(data$g) * data$g^phi["kx"])

	s.bd = t(X) %*% (data$terminal - (1-data$event) * exp(X %*% phi[beta.d]) * data$t^phi["kd"] * data$Eeaw)
	s.bx = t(X) %*% (data$event - exp(X %*% phi[beta.x]) * data$Eew * data$g^phi["kx"])

	# return
	score <-
	c(unname(s.bd), unname(s.bx),
	  s.kd = unname(s.kd), s.kx = unname(s.kx),
	  s.theta = unname(s.theta), s.alpha = unname(s.alpha))
	names(score)[1:length(c(beta.d,beta.x))] <- names(phi)[c(beta.d,beta.x)]
	return(score)
}
