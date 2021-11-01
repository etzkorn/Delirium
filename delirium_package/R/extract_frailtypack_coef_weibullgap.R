#' Extract parameters for weibull model from frailty pack
#'
#' @description none
#'
#' @param model frailty pack model assuming weibull hazards (gap or calendar).
#' @return Vector of named parameter estimates.
#'
#' @export

extract.fp.coef <- function(model, weibull = T){
	if(weibull){
	b <- model$b
	b[1:5] <- (b[1:5])^2

	SE <- sqrt(diag(model$varHtotal))
	SE[1:5] <- SE[1:5]*2*(b^.5)[1:5]
		# used delta method for standard errors
	coef <-
	tibble(Parameter = c("betaR", "etaR",
		         "betaD", "etaD",
		         "theta",
		         "alpha1",
		         "trtR", "trtD"),
	       Estimate = b,
	       SE = SE)
	}else{
	b <- model$b
	b<- (b[(length(b) - 3):length(b)])
	b[1] <- b[1]^2
	SE <- sqrt(diag(model$varHIHtotal))
	SE <- (SE[(length(SE) - 3):length(SE)]) # obtain only theta, alpha, and coef
	SE[1] <- SE[1]*2*(b^.5)[1] # delta method for theta only
		# delta method
	coef <-tibble(Parameter = c("theta","alpha1","trtR", "trtD"),
		  Estimate = b,
		  SE = SE)
	}
	return(coef)
}
