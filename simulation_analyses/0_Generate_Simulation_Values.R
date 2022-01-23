
library(tidyverse)
set.seed(20220107)

simValues <-
expand.grid(betaR = 1, etaR = 10,
	betaD = 1.85, etaD = 25,
	betaD2 = 1.15, etaD2 = 10,
	theta = c(0.1, 0.2, 0.5),
	alpha1 = c(-0.5, 0, 0.5),
	alpha2 = c(-0.5, 0, 0.5),
	trtR = c(-0.1, 0, 0.1),
	trtD = c(-0.1, 0, 0.1),
	trtD2 = c(-0.1, 0, 0.1),
	i = 1:5000) %>%
mutate(simid = 1:n(),
       scenario = ((simid-1) %% 729)+1,
       seed = cumsum(1000 + rpois(n(),10)),
       n = 1500)

saveRDS(simValues, file = "../simulation_results/Simulation_Values_MetaData.rdata")

##### Limited Scope
simValues <-
simValues %>%
filter(theta == 0.2 & alpha1 == 0.5 & trtR == -0.1 & trtD == -0.1)

saveRDS(simValues, file = "../simulation_results/Simulation_Values_MetaData_Limited.rdata")


######## Tests
simValues <-
	expand.grid(betaR = 1, etaR = 10,
		betaD = 1.85, etaD = 25,
		betaD2 = 1.15, etaD2 = 10,
		theta = 0.2,
		alpha1 = 0,
		alpha2 = 0,
		trtR = 0,
		trtD = 0,
		trtD2 = 0,
		i = 1:1000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(1000 + rpois(n(),10)),
	       n = 1500)

saveRDS(simValues, file = "../simulation_results/Simulation_Values_MetaData_Test.rdata")

