# This script is to be run in parallell by each
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

#### Limit Scope
load(file = "metaData.rdata")
simid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))

par <- meta[simid,c("betaR", "etaR", "betaD", "etaD", "betaD2", "etaD2",
	             "theta", "alpha1", "alpha2", "trtR", "trtD", "trtD2")]
seed <- meta$seed[simid]
n <- meta$n[simid]
results <- tibble()

set.seed(seed)
for(i in 1:1000){
# (1) Simulate Data Set
data <- simulate.competing.data(n = n, truncate = 28, gap = T, par0 = unlist(par))
data.summary <- data %>%
	group_by(id) %>%
	summarise(
	         w = w[1],
	         event = sum(event),
	         trt = mean(trt),
	         terminal1 = sum(terminal1),
	         terminal2 = sum(terminal2),
	         t = max(t)) %>%
	group_by(trt, terminal1, terminal2) %>%
	summarise(n = n(),
	          t25 = quantile(t,probs = 0.25),
	          t75 = quantile(t,probs = 0.75),
	          t50 = median(t),
	          event = sum(event)) %>%
	ungroup


# Add identifiers to model object for further analysis
results <-
tibble(
i = i,
scenario = meta$scenario[simid],
simid = simid,
seed = seed,
truth = list(par),
data.summary = list(data.summary)) %>%
bind_rows(results)
}

# (3) Save Results Under Simulation ID
save(results, file = paste0("Data_Gen_Results/Sim_",simid,".rdata" ))
