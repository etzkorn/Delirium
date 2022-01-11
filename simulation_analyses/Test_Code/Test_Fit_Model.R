# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

message("Starting script...")

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

#source("../delirium_package/R/competing_simulate_data.R")
#source("../delirium_package/R/random_weibull.R")

par <- c(betaR = 1, etaR = 25,
         betaD = 1.85, etaD = 20,
         betaD2 = 1.15, etaD2 = 10,
         theta = .1,
         alpha1 = 0, alpha2 = 0,
         trtR = 0.5, trtD = 0, trtD2 = 0)
seed <- 256
n <- 1500

set.seed(seed)

message(paste("seed:", seed))
message(paste("n:", n))
message(paste("\npar:", par))


# (1) Simulate Data Set
data <- simulate.competing.data(n = n, truncate = 28, gap = T, par0 = par)

message({
data %>% group_by(trt) %>% summarise(sum(event))
})

message({
data %>% filter(terminal1 + terminal2 == 1) %>%
	group_by(trt) %>% summarise(mean(t), sum(terminal1), sum(terminal2))
})


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

message("\nEstimates:")

message(mod$b)

message("\nSummary Table:")


message(mod$summary.table)


message(mod$initialization$summary.table1)


message(mod$initialization$summary.table2)



