#' Calculate expected value of the random frailty for many individuals
#'
#' @description Given panel data frame for multiple individuals, calculate a vector of random frailty for each. Wrapper for integrate.w
#'
#' @param df list of person-specific data frames
#' @param phi parameter vector (named)
#' @param f.w function of the random frailty of which to take the expectation (ex. E(ln(w)))
#' @param J number of integration points for gaussian-laguerre quadrature
#' @param covar.names covariate names for linear predictor
#'
#' @return Vector of scalar estimates of frailty.
#'
#' @export
#'
expected.frailty <- function(df, phi, f.w, J=50, covar.names, frailty.center = "linear"){
	if(is.null(names(f.w))) names(f.w) <- paste0("f",1:length(f.w))
	df %>%
	dplyr::select(id, g, t, event, terminal, covar.names, w) %>%
	nest(-id) %>%
	mutate(E = map(data, ~integrate.w(d = ., J = J, phi = phi, f.w = f.w, covar.names = covar.names, frailty.center = frailty.center)) ) %>%
	unnest(E) %>%
	unnest(data)
}
