
load(file = "../reduce_analysis/processed_data.rdata")
library(frailtypack); library(gt); library(tidyverse); library(survival)

model0a <-
	frailtyPenal(data = df2,
		 formula = Surv(tstart, tstop, delirium) ~ treatment +
		 	terminal(death) + cluster(id),
		 formula.terminalEvent = ~ treatment ,
		 recurrentAG = T, hazard = "Weibull",RandDist = "LogN")


my_function_call <-
	as.call(list(formula = Surv(tstart, tstop, delirium) ~ treatment +
