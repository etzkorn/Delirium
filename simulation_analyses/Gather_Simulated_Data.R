
############## Cluster
library(tidyverse)
output.files <- dir("Simulation_Results", full.names = T)

summaries <- tibble()

for(i in 1:length(output.files)){
	model <- readRDS(output.files[i])

	summaries <- bind_rows(
	summaries,
	tibble(id = model$simulation.id,
	       par = list(model$simulation.values),
	       deaths = mean(model$icdc0),
	       discharges = mean(1 - model$icdc0),
	       events =  sum(model$ic0)/1500,
	       deaths.trt = mean(model$icdc0[model$vaxdc0==1]),
	       discharges.trt = mean(1 - model$icdc0[model$vaxdc0==1]),
	       events.trt =  sum(model$ic0[model$vax0 == 1])/sum(model$vax0 == 1),
	       deaths.pl = mean(model$icdc0[model$vaxdc0==0]),
	       discharges.pl = mean(1 - model$icdc0[model$vaxdc0==0]),
	       events.pl =  sum(model$ic0[model$vax0 == 0])/sum(model$vax0 == 0))
	)
}

save(summaries,
     file = paste0("Gathered_Results/Data_Simulation_Results",length(output.files),".rdata"))


