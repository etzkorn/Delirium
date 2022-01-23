# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

#### Limit Scope
metadata <- readRDS( file = "Simulation_Values_MetaData_Limited.rdata")
simids <- metadata$simid[metadata$simid > 155000]
simid <- simids[as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))]

#### Full Sim
#metadata <- readRDS(file = "Simulation_Values_MetaData.rdata")
#simid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))

par <- unlist(metadata[metadata$simid == simid,
	           c("betaR", "etaR", "betaD", "etaD", "betaD2", "etaD2",
	             "theta", "alpha1", "alpha2", "trtR", "trtD", "trtD2")])
seed <- as.integer(metadata$seed[metadata$simid == simid])
n <- metadata$n[metadata$simid == simid]

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
mod <- mod[c("summary.table","Initialization", "critCV","icdc0", "vaxdc0", "ic0", "vax0")]
mod$simulation.values <- par
mod$simulation.id <- simid
mod$seed <- seed
mod$initialization$istop1 <- mod$initialization$joint1$istop
mod$initialization$istop2 <- mod$initialization$joint2$istop
mod$initialization$joint1 <- NULL
mod$initialization$joint2 <- NULL

# (3) Save Results Under Simulation ID
saveRDS(mod, file = paste0("Simulation_Results/Sim_",simid,".rdata" ))
