load(file = "../package_tests/processed_data.rdata")
require(frailtypack)
model0f <- multivPenal(formula = Surv(tstart, tstop, delirium) ~ treatment +
		terminal(death) + terminal2(discharge) + cluster(id),
	formula.terminalEvent = ~treatment,
	formula.terminalEvent2= ~treatment,
	data = df2,
	recurrentAG=FALSE,
	jointGeneral = F,
	hazard = "Splines",
	n.knots = as.integer(c(5,5,5)),
	kappa = c(10000,10000,10000),
	maxit = 100,
	initialize = F,
	init.hazard = c(rep(1,21)),
	init.Theta = 0.425,
	init.Alpha1 = -0.27,
	init.Alpha2 = -0.53,
	init.B = c(0.29,0,0))
