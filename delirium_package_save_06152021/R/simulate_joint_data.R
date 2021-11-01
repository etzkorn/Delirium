#' Simulate joint recurrent event and survival data
#'
#' @description Simulate times of recurrent events and terminal events using joint model. Recurrent events have weibull gap times.
#'
#' @param par0 named parameter vector
#' @param n number of individuals in data
#'
#' @return tibble with variables
#'
#' @examples
#' simulate.data(n =3)
#' @export

simulate.joint.data <- function(n = 100, truncate = 28, gap = T,
			par0 = c(betaR = 1, etaR = 40,
			         betaD = 1, etaD = 20,
			         theta = .1,
			         alpha1 = -.1,
			         trtR = 1, trtD = 0)){
	if(gap){
		tibble(id = 1:n,
		       trt = rbinom(n,1,.5),
		       w = rnorm(n, 0, par0["theta"]^.5),
		       y1 = rweibRH(n,
		       	 shape = par0["betaD"],
		       	 scale = par0["etaD"],
		       	 rh = exp(w*par0["alpha1"] + par0["trtD"] * trt)),
		       y = pmin(y1, truncate),
		       terminal1 = as.numeric(y1 < truncate),
		       t =  map2(w,trt,
		                 ~cumsum(rweibRH(150,
		                 	    shape = par0["betaR"],
		                 	    scale = par0["etaR"],
		                 	    rh = exp(.x + par0["trtR"] * .y))))) %>%
			dplyr::select(-y1) %>%
			unnest(t) %>%
			gather("event", "t", -id, -trt, -w, -terminal1) %>%
			distinct() %>%
			dplyr::arrange(id, t) %>%
			dplyr::group_by(id) %>%
			dplyr::filter(t <= t[event=="y"]) %>%
			dplyr::mutate(terminal1 = sum(as.numeric(event=="y" & terminal1==1 & t < truncate)),
				  event = as.numeric(event=="t"),
				  g = diff(c(0,t)),
				  t0 = t - g)%>%
			dplyr::ungroup() %>%
			dplyr::select(id, trt, w, event, terminal1, g,t, t0)
	}else{
		tibble(id = 1:n,
		       trt = rbinom(n,1,.5),
		       w = rnorm(n, 0, par0["theta"]^.5),
		       y1 = rweibRH(n,
		       	 shape = par0["betaD"],
		       	 scale = par0["etaD"],
		       	 rh = exp(w*par0["alpha1"] + par0["trtD"] * trt)),
		       y = pmin(y1, truncate),
		       terminal1 = as.numeric(y1 < truncate),
		       t =  map2(w,trt,
		                 ~rweibRH(150,
		                          shape = par0["betaR"],
		                          scale = par0["etaR"],
		                          rh = exp(.x + par0["trtR"] * .y)))) %>%
			dplyr::select(-y1) %>%
			unnest %>%
			gather("event", "t", -id, -trt, -w, -terminal1) %>%
			distinct() %>%
			dplyr::arrange(id, t) %>%
			dplyr::group_by(id) %>%
			dplyr::filter(t <= t[event=="y"]) %>%
			dplyr::mutate(terminal1 = as.numeric(event=="y" & terminal1==1 & t < truncate),
				  event = as.numeric(event=="t"),
				  g = diff(c(0,t)),
				  t0 = t - g)%>%
			dplyr::ungroup() %>%
			dplyr::select(id, trt, w, event, terminal1,g,t, t0)
	}
}
