#' Simulate joint recurrent event and survival data
#'
#' @description Simulate times of recurrent events and terminal events using joint model.
#'
#' @param par0 named parameter vector
#' @param n number of individuals in data
#' @param frailty distributional assumption on random frailty term
#'
#' @return tibble with variables
#'
#' @examples
#' simulate.data(par0 = c(bd = 1, kd = 1, rd = 1, bx=1, kx=1, rx=1, gam=1, alpha=1),
#'               n = 3, frailty = "gamma")
#' @export

simulate.data <- function(par0 = rep(1,8), n = 100, frailty = "gamma"){
	if(!frailty %in% c("gamma", "lognormal")){
		stop("frailty must be character string. either 'gamma', 'lognormal', or 'normal'.")
	}

	if(frailty == "gamma"){
		w = rgamma(n, shape = par0["gam"], scale = 1/par0["gam"])
	}else if(frailty == "lognormal"){
		w = rlognorm(n, mean = 0, var = 1/par0["gam"])
	}else if(frailty == "normal"){
		w = log(rnorm(n, 0, 1/par0["gam"]))
	}
	tibble(
		id = 1:n,
		trt = rbinom(n,1,.5),
		w = w,
		# k is shape, b is scale
		y = rweib(n,
		          k = par0["kd"],
		          b = (par0["bd"]*w^par0["alpha"]*par0["rd"]^trt)),
		t =  map2(w,trt, ~cumsum(rweib(90,
				       k = par0["kx"],
				       b = par0["bx"]*.x*par0["rx"]^.y)))) %>%
		unnest %>%
		gather("event", "t", -id, -trt, -w) %>%
		distinct() %>%
		arrange(id, t) %>%
		group_by(id) %>%
		filter(t <= t[event=="y"]) %>%
		mutate(x = n()-1,
		       y = t[event=="y"],
		       terminal = as.numeric(event=="y"),
		       event = as.numeric(event=="t"),
		       g = diff(c(0,t)))%>%
		ungroup
}
