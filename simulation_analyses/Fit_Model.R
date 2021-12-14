# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID
#
# Notes:
# cd $ups/numerical_experiments/R
# Rnosave Fit_Model.R -t 1-5000 -tc 40 -N JOB_lm

library(frailtypack); library(tidyverse)
source("competing_simulate_data.R")
source("random_weibull.R")
metadata <- readRDS(file = "Simulation_MetaData.rdata")
par <- metadata[[simid]]$par
new.seed <- metadata[[simid]]$seed
set.seed(new.seed)

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
mod$simulation.id <- simid

# (3) Save Results Under Simulation ID
saveRDS(mod, file = paste("Simulation_Results/Sim_",simid,".rdata" ))

