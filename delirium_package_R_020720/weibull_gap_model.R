#' Fit Weibull Gap Model
#'
#' @description Fit Weibull Gap Model
#'
#' @param data data frame
#' @param e.iter number of times to recalculate the expectations
#' @param m.iter each time a new expectation is calculated, how many iterations to maximize parameters
#' @param par.start starting values for parameter estimates
#' @param frailty distributional assumption on random frailty effects
#'
#' @return List with estimate vector, sequence of estimates from maximization procedure, standard errors, and individual frailty estimates.
#'
#' @export
weibull.gap.model <- function(data, e.iter = 15, m.iter = 5, par.start = rep(1, 8), frailty = "gamma"){
	# check variable names
	if(! all(c("id","y", "x", "trt", "g") %in% colnames(data))){
		stop("Variables id (subject id), y (survival time), x (event count),
		     trt (treatment indicator) and g (gap times)
		     must be present in data.")
	}

	data <- data %>% nest(-id)

	# Find MLE
	par.seq <- t(par0)
	par.E <- par0
	par.step <- par0
	for(i in 1:e.iter){
		for(i in 1:m.iter){
			# Maximization Step
			# find MLEs given current estimates of random effects
			par.step <- weibull.gap.nr.step(phi = (par.step),
					        phi0 = (par.E),
					        data = data)
			par.seq <- rbind(par.seq, par.step)
		}
		# Expectation Step
		# maximized parameters will now be used to calculate expected random effects
		par.E <- par.step
	}

	# estimate standard errors
	se = diag(solve(-dscore.weibull.gap(phi = par.step, phi0 = par.step, data = data))) ^.5

	# Estimate Final Random Effects
	re = map(data$data, ~integrate.w(d = ., phi = par.step, f.w = function(w) w)) %>%
		unlist

	# Prepare Results
	model.result <-
		list(estimates = par.step,
		     estimate.sequence = par.seq %>% as_tibble ,
		     se = se,
		     frailty.estimate = re)
}

weibull.gap.model2 <- function(data, covar.names="trt", e.iter = 15, m.iter = 5){
	# check variable names
	if(! all(c("id","g","t", "event", "terminal") %in% colnames(data))){
		stop("Variables id (subject id), g (gap time), t (time since start),
		     event (event indicator), terminal (terminal event/death indicator)
		     must be present in data.")
	}

	# Find MLE
	par.start = c(rep(0, 2*length(covar.names) + 2), rep(1, 4))
	names(par.start) <- c(paste0("beta.d", c(0, covar.names)), paste0("beta.x", c(0, covar.names)),
		          "kd", "kx", "theta", "alpha")
	par.seq <- t(par.start)
	par.step <- par.start
	for(i in 1:e.iter){
		# Expectation Step
		# calculate expectations of a few transformations of the variables
		data <-
		data %>%
		expected.frailty2(phi = par.step,
			      covar.names =covar.names,
			      f.w = list(Ew = function(w, phi) w,
			                 Ew2 = function(w, phi) w^2,
			                 Eew = function(w, phi) exp(w),
			                 Eeaw = function(w, phi) exp(phi["alpha"] * w),
			                 Ew2eaw = function(w, phi) w^2 *exp(phi["alpha"] * w),
			                 Eweaw = function(w, phi) w * exp(phi["alpha"] * w)
			      ))
		for(j in 1:m.iter){
			# Maximization Step
			# find MLEs given current estimates of random effects
			par.step <- weibull.gap.nr.step2(phi = par.step,
					        data = data,
					        covar.names = covar.names)
			par.seq <- rbind(par.seq, par.step)
		}
	}

	# estimate standard errors
	se = diag(solve(-dscore.weibull.gap2(phi = par.step, data = data, covar.names = covar.names))) ^.5

	# Prepare Results
	model.result <-
		list(estimates = par.step,
		     estimate.sequence = par.seq %>% as_tibble ,
		     se = se,
		     data = data)
	return(model.result)
}
