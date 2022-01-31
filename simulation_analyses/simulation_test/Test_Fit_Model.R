# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

simid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
metadata <- readRDS(file = "Simulation_Values_MetaData_Test.rdata")
par <- unlist(metadata[simid,
	           c("betaR", "etaR", "betaD", "etaD", "betaD2", "etaD2",
	             "theta", "alpha1", "alpha2", "trtR", "trtD", "trtD2")])
seed <- metadata$seed[simid]
n <- metadata$n[simid]

set.seed(seed)

# (1) Simulate Data Set
data <- simulate.competing.data(n = n, truncate = 28, gap = T, par0 = par)

# (2) Fit Model
mod <-
	multivPenal(formula = Surv(t0, t, event)~cluster(id)+trt+terminal(terminal1)+terminal2(terminal2),
		formula.terminalEvent=~trt,
		formula.terminalEvent2=~trt,
		data=data,
		jointGeneral = F,
		initialize = T,
		save.progress = F,
		hazard = "Weibull",
		gapTimes=T,
		maxit = 350)

# Add identifiers to model object for further analysis
mod$simulation.values <- par
mod$simulation.id <- simid
mod$seed <- seed

# (3) Save Results Under Simulation ID
saveRDS(mod, file = paste0("Simulation_Results_Test/Sim_",simid,".rdata" ))
