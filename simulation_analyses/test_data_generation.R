# This script is to be run in parallell
# (0) Receive Simulation ID (simid)
# (1) Simulate Data Set
# (2) Fit Model
# (3) Save Results Under Simulation ID

library(frailtypack); library(tidyverse)
source("Simulation_Scripts/competing_simulate_data.R")
source("Simulation_Scripts/random_weibull.R")

#### Limit Scope
load(file = "metaData.rdata")
meta <- filter(meta, i==1)
results <- tibble()

for(simid in 1:nrow(meta)){
par <- meta[simid,c("betaR", "etaR", "betaD", "etaD", "betaD2", "etaD2",
	             "theta", "alpha1", "alpha2", "trtR", "trtD", "trtD2")]
seed <- meta$seed[simid]
n <- meta$n[simid]

set.seed(seed)
# (1) Simulate Data Set
data <- simulate.competing.data(n = 1500, truncate = 28, gap = T, par0 = unlist(par))
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
scenario = meta$scenario[simid],
simid = simid,
truth = list(par),
data.summary = list(data.summary)) %>%
bind_rows(results)
}

# (3) Save Results Under Simulation ID
save(results, file = paste0("Data_Gen_Results/Sim_",simid,".rdata" ))

results %>% unnest(truth) %>% unnest(data.summary) %>%
filter(trtD2 ==0, alpha1==0, alpha2==0, trtD2 == 0, theta == 0.2) %>%
group_by(trtD, terminal1, terminal2, trt) %>%
summarise(mean(n))

data <- simulate.competing.data(n = 5000, truncate = 28, gap = T,
		        par0 = c(betaR=1, etaR=10, betaD=1, etaD=10, betaD2=1, etaD2=10,
		                 theta=0.1, alpha1=0, alpha2=0, trtR=0, trtD=5, trtD2=0))
data %>%
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
	ungroup %>%
	group_by(trt) %>%
	mutate(p = n / sum(n))
