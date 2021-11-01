#' Fit Weibull Gap Model
#'
#' @description Fit Weibull Gap Model
#'
#' @param data data frame
#' @param covar.names vector of characters of variable names to include as linear predictor in model
#' @param e.iter number of times to recalculate the expectations
#' @param m.iter each time a new expectation is calculated, how many iterations to maximize parameters
#'
#' @return List with estimate vector, sequence of estimates from maximization procedure, standard errors, and individual frailty estimates.
#'
#' @export

weibull.gap.model <- function(data, covar.names="trt", e.iter = 15, m.iter = 5,
		      frailty.center = "linear", add = F, model = NULL,
		      method = c("EM", "optim")){
	# check variable names

	if(method == "EM" & !add){
		if(! all(c("id","g","t", "event", "terminal") %in% colnames(data))){
			stop("Variables id (subject id), g (gap time), t (time since start),
			     event (event indicator), terminal (terminal event/death indicator)
			     must be present in data.")
		}

		# Find MLE
		par.start = c(rep(0, 2*length(covar.names) + 2), rep(1, 2), .1,.5)
		names(par.start) <- c(paste0("beta.d", c(0, covar.names)), paste0("beta.x", c(0, covar.names)),
			          "kd", "kx", "theta", "alpha")
		par.seq <- t(par.start)
		par.step <- par.start
	}else if(method == "EM" & add){
		par.step = model$estimates
		par.seq = model$estimate.sequence
		data = model$data
	}
	if(method == "EM"){
	for(i in 1:e.iter){
		# Expectation Step
		# calculate expectations of a few transformations of the variables
		data <-
		data %>%
		expected.frailty(phi = par.step,
			      covar.names =covar.names,
			      f.w = list(Ew = function(w, phi) w,
			                 Ew2 = function(w, phi) w^2,
			                 Eew = function(w, phi) exp(w),
			                 Eeaw = function(w, phi) exp(phi["alpha"] * w),
			                 Eweaw = function(w, phi) w * exp(phi["alpha"] * w),
			                 Ew2eaw = function(w, phi) w^2 *exp(phi["alpha"] * w)
			      ), frailty.center = frailty.center)
		for(j in 1:m.iter){
			# Maximization Step
			# find MLEs given current estimates of random effects
			par.step <-
				tryCatch({
				weibull.gap.nr.step(phi = par.step,
					        data = data,
					        covar.names = covar.names,
					        frailty.center = frailty.center)
				}, error = function(e) rep(NA, 8))
			# force MLE for theta, random effect variance
			par.seq <- rbind(par.seq, par.step)
			if(any(is.na(par.step))) break
		}
		if(any(is.na(par.step))) break
	}
	}

	# estimate standard errors
	se = tryCatch({
		diag(solve(-dscore.weibull.gap(phi = par.step, data = data,
				       covar.names = covar.names))) ^.5
	}, error = function(e) rep(NA, 8))

	# Prepare Results
	model.result <-
		list(estimates = par.step,
		     estimate.sequence = par.seq %>% as_tibble ,
		     se = se,
		     data = data)
	return(model.result)
}
