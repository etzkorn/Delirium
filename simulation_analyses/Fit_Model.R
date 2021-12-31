# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

message("Starting script...")

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

message("Code Sourced...")

simid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
metadata <- readRDS(file = "Simulation_Values_MetaData.rdata")
par <- unlist(metadata[simid,1:12])
seed <- metadata$seed[simid]

set.seed(seed)

message("Seed Set...")

# (1) Simulate Data Set
data <- simulate.competing.data(n = n, truncate = 28, par0 = par)

message("Data Simulated...")

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

message("Model Fit...")

# Add identifiers to model object for further analysis
mod$simulation.values <- par
mod$simulation.id <- simid

# (3) Save Results Under Simulation ID
saveRDS(mod, file = paste0("Simulation_Results/Sim_",simid,".rdata" ))

message("Results Saved...")
