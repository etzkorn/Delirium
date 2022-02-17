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

simulate.competing.data <- function(n, truncate = 28, par0, version = 1){
if(version==1){
	tibble(id = 1:n,
	       trt = rbinom(n,1,.5),
	       w = rnorm(n, 0, par0["sigma"]),
	       y1 = rweibRH(n,
	       	 shape = par0["betaD"],
	       	 scale = par0["etaD"],
	       	 rh = exp(w*par0["alpha1"] + par0["trtD"] * trt)),
	       y2 = rweibRH(n,
	       	 shape = par0["betaD2"],
	       	 scale = par0["etaD2"],
	       	 rh = exp(w*par0["alpha2"] + par0["trtD2"] * trt)),
	       y = pmin(y1, y2, truncate),
	       terminal1 = as.numeric(y1 < y2 & y1 < truncate),
	       t =  map2(w,trt,
	                 ~cumsum(rweibRH(150,
	                 	    shape = par0["betaR"],
	                 	    scale = par0["etaR"],
	                 	    rh = exp(.x + par0["trtR"] * .y))))) %>%
		dplyr::select(-y1, -y2) %>%
		unnest(t) %>%
		gather("event", "t", -id, -trt, -w, -terminal1) %>%
		distinct() %>%
		dplyr::arrange(id, t) %>%
		dplyr::group_by(id) %>%
		dplyr::filter(t <= t[event=="y"]) %>%
		dplyr::mutate(terminal2 = as.numeric(event=="y" & terminal1==0 & t < truncate),
		       terminal1 = as.numeric(event=="y" & terminal1==1 & t < truncate),
		       event = as.numeric(event=="t"),
		       g = diff(c(0,t)),
	                   t0 = t - g)%>%
		dplyr::ungroup() %>%
		dplyr::select(id, trt, w, event, terminal1, terminal2, g,t, t0)
}else{
tibble(
id = 1:n,
trt = rbinom(n,1,.5),
w = rnorm(n, 0, par0["sigma"]),
T1 = rweibRH(n,
	 shape = par0["shapeM"],
	 scale = par0["scaleM"],
	 rh = exp(w*par0["alphaM"] + par0["betaM"] * trt)),
T2 = rweibRH(n,
	 shape = par0["shapeD"],
	 scale = par0["scaleD"],
	 rh = exp(w*par0["alphaD"] + par0["betaD"] * trt)),
y = pmin(T1, T2, K),
terminal1 = as.numeric(T1 < T2 & T1 < K),
terminal2 = as.numeric(T2 < T1 & T2 < K),
t =  map2(w,trt,
          ~ rweibRH(50,
          	            shape = par0["shapeR"],
          	            scale = par0["scaleR"],
          	            rh = exp(.x + par0["betaR"] * .y)) %>%
	cumsum)

)%>%
dplyr::select(-T1, -T2, -w) %>%
unnest(t) %>%
group_by(id) %>%
mutate(tstart = c(0, t[-n()])) %>%
dplyr::filter(tstart<=y)%>%
mutate(terminal1 = terminal1*(t > y),
	   terminal2 = terminal2*(t > y),
	   event = event*(t < y),
	   t = ifelse(t > y, y, t)) %>%
dplyr::select(-y) %>%
	#the timing of the terminal event is no longer needed as a separate variable.
ungroup
}
}
