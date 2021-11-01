#' Calculate expected value of the random frailty for many individuals
#'
#' @description Given data frames for multiple individuals, calculate a vector of random frailty for each. Wrapper for integrate.w
#'
#' @param df list of person-specific data frames
#' @param phi parameter vector (named)
#' @param f.w function of the random frailty of which to take the expectation (ex. E(ln(w)))
#' @param J number of integration points for gaussian-laguerre quadrature
#' @param frailty distributional assumption on the random frailties. Currently either "gamma" or "lognormal".
#'
#' @return Vector of scalar estimates of frailty.
#'
#' @export
expected.frailty <- function(df, phi, f.w, J=50, frailty = "gamma"){
	map(df, ~integrate.w(d = ., J = J, phi = phi, f.w = f.w, frailty = frailty)) %>% unlist
}

expected.frailty2 <- function(df, phi, f.w, J=50, covar.names){
	if(is.null(names(f.w))) names(f.w) <- paste0("f",1:length(f.w))
	df %>%
	select(id, g, t, event, terminal, covar.names, w) %>%
	nest(-id) %>%
	mutate(E = map(data, ~integrate.w2(d = ., J = J, phi = phi, f.w = f.w, covar.names = covar.names)) ) %>%
	unnest(E) %>%
	unnest(data)
}
