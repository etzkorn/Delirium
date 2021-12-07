
# (0) Receive Parameter Values (par), Simulation ID (id), and Sample Size (n)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

library(frailtypack); library(tidyverse)
source("competing_simulate_data.R")
source("random_weibull.R")

# (1) Simulate Data Set
data <- simulate.competing.data(n = n, truncate = 28, par0 = par)

# (2) Fit Model
mod <-
multivPenal(formula = Surv(t0, t, event)~cluster(id)+trt+terminal(terminal1)+terminal2(terminal2),
	formula.terminalEvent=~trt,
	formula.terminalEvent2=~trt,
	data=data,
	jointGeneral = F,
	initialize = T,
	save.progress = T,
	hazard = "Weibull",
	gapTimes=T,
	maxit = 350)

# Add identifiers to model object for further analysis
mod$simulation.values <- par
mod$simulation.id <- id

# (3) Save Results Under Simulation ID
save(mod, file = paste("Simulation_Results/Sim_",id,".rdata" ))

