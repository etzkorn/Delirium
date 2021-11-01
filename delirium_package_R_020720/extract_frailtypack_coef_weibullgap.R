#' Extract parameters for weibull gap model from frailty pack
#'
#' @description none
#'
#' @param model frailty pack model assuming weibull gap times.
#' @return Vector of named parameter estimates.
#'
#' @export

extract.fp.coef <- function(model){
	# assume recurrent event parameter comes first
	# and terminal event parameter comes se
	fp.model$coef
	fp.model$scale.weib
	fp.model$shape.weib
	
	c(bd = model$scale.weib[2]^-model$shape.weib[2], kd = model$shape.weib[2], rd = unname(exp(model$coef[2])), 
	  bx = model$scale.weib[1]^-model$shape.weib[1], kx = model$shape.weib[1], rx = unname(exp(model$coef[1])), 
	  gam=1/model$theta, alpha= model$alpha)
}