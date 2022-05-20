
# These data generating values are based on the REDUCE trial.
# Values are Estimated in 1a_Weibull_REDUCE_Model_20220113
library(tidyverse)

set.seed(20220122)

meta <-     expand.grid(betaR = 0.5, etaR = 66,
		betaD = 1.6, etaD = 25.2,
		betaD2 = 1.3, etaD2 = 7.7,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-0.5, 0, 0.5),
		alpha2 = c(-0.5, 0, 0.5),
		trtR = c(-0.25, 0, 0.25),
		trtD = c(-0.25, 0, 0.25),
		trtD2 = c(-0.25, 0, 0.25),
		i = 1:1000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500)
save(meta, file = "../simulation_results/metaData.rdata")
# 9 primary scenarios:
# 8   17   26  251  260  269  494  503  512
# 487  488  489 493  494  495

set.seed(20220203)

meta <-     expand.grid(betaR = 0.5, etaR = 66,
		betaD = 1.6, etaD = 25.2,
		betaD2 = 1.3, etaD2 = 7.7,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-0.5, 0, 0.5),
		alpha2 = c(-0.5, 0, 0.5),
		trtR = c(-0.25, 0, 0.25),
		trtD = c(-0.25, 0, 0.25),
		trtD2 = c(-0.25, 0, 0.25),
		i = 1:4000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500) %>%
	filter(scenario %in% c(8,17,26,251,260,269,494,503,512))
save(meta, file = "../simulation_results/metaData2.rdata")



set.seed(20220206)

meta <-     expand.grid(betaR = 0.5, etaR = 66,
		betaD = 1.6, etaD = 25.2,
		betaD2 = 1.3, etaD2 = 7.7,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-5, 0, 5),
		alpha2 = c(-5, 0, 5),
		trtR = c(-0.5, 0, 0.5),
		trtD = c(-0.5, 0, 0.5),
		trtD2 = c(-0.5, 0, 0.5),
		i = 1:5000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500) %>%
	filter(scenario %in% c(8,17,26,251,260,269,494,503,512))
save(meta, file = "../simulation_results/metaData3.rdata")


meta <-     expand.grid(betaR = 0.5, etaR = 66,
		betaD = 1.6, etaD = 25.2,
		betaD2 = 1.3, etaD2 = 7.7,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-5, 0, 5),
		alpha2 = c(-5, 0, 5),
		trtR = c(-0.5, 0, 0.5),
		trtD = c(-0.5, 0, 0.5),
		trtD2 = c(-0.5, 0, 0.5),
		i = 1:5000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500) %>%
	filter(scenario %in% c(8,17,26,251,260,269,494,503,512))
save(meta, file = "../simulation_results/metaData3.rdata")

###########################################################3
# Version 4 (5/16/22)
# Here, I make the hazards of death and discharge equal to give similar prevalence
# to both types of terminal events.
# betaR is set so the distribution is less skewed than an exponential.

meta <-     expand.grid(betaR = 1.5, etaR = 10,
		betaD = 1.75, etaD = 16,
		betaD2 = 1.75, etaD2 = 16,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-1, 0, 1),
		alpha2 = c(-1, 0, 1),
		trtR = c(-0.5, 0, 0.5),
		trtD = c(-0.5, 0, 0.5),
		trtD2 = c(-0.5, 0, 0.5),
		i = 1:5000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500) %>%
	filter(scenario %in% c(8,17,26,251,260,269,494,503,512))
save(meta, file = "../simulation_results/metaData4.rdata")

###########################################################3
# Version 5 (5/20/22)
# Here, I increase the relative time of death so that

meta <-     expand.grid(betaR = 1.5, etaR = 10,
		betaD = 1.75, etaD = 16,
		betaD2 = 1.75, etaD2 = 16,
		sigma = c(0.5, 0.75, 1),
		alpha1 = c(-1, 0, 1),
		alpha2 = c(-1, 0, 1),
		trtR = c(-0.5, 0, 0.5),
		trtD = c(-0.5, 0, 0.5),
		trtD2 = c(-0.5, 0, 0.5),
		i = 1:5000) %>%
	mutate(simid = 1:n(),
	       scenario = ((simid-1) %% 729)+1,
	       seed = cumsum(10 + rpois(n(),10)),
	       n = 1500) %>%
	filter(scenario %in% c(8,17,26,251,260,269,494,503,512))
save(meta, file = "../simulation_results/metaData4.rdata")
