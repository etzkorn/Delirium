#' Calculate the marginal log likelihood of a parameter combination for the weibull gap model
#'
#' @description Calculate the marginal (negative) log likelihood of a parameter combination for the weibull gap model
#'
#' @param phi parameter vector (named)
#' @param data nested data frame on the individual level
#' @param covar.names names of covariates in data frame
#' @param frailty.center where should the random frailty terms be centered?
#'
#' @return scalar, log likelihood
#'
#' @examples
#' data <- simulate.data(c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  kd = 1, kx = 1, theta = .1, alpha = 1), n = 10, frailty.center = "linear")
#'
#' marg.likelihood.weibull.gap(data = data, covar.names = "trt",
#' phi = c(beta.d0 = 0, beta.dtrt = 0, beta.x0 = .5, beta.xtrt = 0,
#'  log.kd = 0, log.kx = 0, log.theta = -1, alpha = 1))
#'
#' @export

marg.likelihood.weibull.gap <- function(phi, data, covar.names, GH = 30, optim = F){
	names(phi) <- c(paste0(rep(c("beta","eta"),2),rep(c("R","D"), each = 2)),
		    "theta", "alpha1",
		    paste0(covar.names,c("R","D")))
	phi[1:5] <- exp(phi[1:5])

	rules <- statmod::gauss.quad(GH, kind = "hermite")

	# Calculate Log Likelihood Components
	data <-
		data %>%
		mutate(predR = exp(as.vector(as.matrix(data[,covar.names]) %*% t(phi[paste0(covar.names,"R")]))),
		       predD = exp(as.vector(as.matrix(data[,covar.names]) %*% t(phi[paste0(covar.names,"D")]))),
		       #predD2 = exp(as.vector(as.matrix(data[,covar.names]) %*% t(phi[paste0(covar.names,"D2")]))),
		       hazR.event = event*((phi["betaR"]-1)*log(g)+log(phi["betaR"])-
		       	phi["betaR"]*log(phi["etaR"])+log(predR)),
		       cumhazR.event = ((g/phi["etaR"])^phi["betaR"])*predR) %>%
		group_by(id) %>%
		summarise(hazR = sum(hazR.event),
		          hazD = sum(terminal1*((phi["betaD"]-1)*log(t)+log(phi["betaD"])-
		          	          	phi["betaD"]*log(phi["etaD"])+log(predD))),
		          #hazD2 = sum(terminal2*((phi["betaD2"]-1)*log(t)+log(phi["betaD2"])-
		          #	           	phi["betaD2"]*log(phi["etaD2"])+log(predD2))),
		          cumhazR = sum(cumhazR.event),
		          cumhazD = mean(((t/phi["etaD"])^phi["betaD"])*predD),
		          #cumhazD2 = sum((1 - event)*((t/phi["etaD2"])^phi["betaD2"])*predD2),
		          nR = sum(event),
		          #terminal2 = sum(terminal2),
		          terminal1 = sum(terminal1))

	cll <- sapply(rules$nodes,
		  FUN = function(frail){
		  	data %>%
		  		with(frail * nR - exp(frail)*cumhazR +
		  		     	phi["alpha1"] * frail * terminal1 - exp(frail*phi["alpha1"]) * cumhazD +
		  		     	#phi["alpha2"] * frail * terminal2 - exp(frail*phi["alpha2"]) * cumhazD2 +
		  		     	-(frail^2)/(2*phi["theta"]))
		  })
	data$integral = as.vector(exp(cll) %*% (rules$weights * exp(rules$nodes^2)))
	data$integral = ifelse(!is.finite(log(data$integral)),
		           10^-10,
		           data$integral)

	-sum(data$hazR + data$hazD + #data$hazD2 +
	    + log(data$integral) +
	    - 0.5*log(2*pi) - log(sqrt(phi["theta"])))
}
