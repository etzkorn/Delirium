
############## Cluster
library(tidyverse)
nodeid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
#minid <- as.numeric(as.character(Sys.getenv("MINID")))
#maxid <- as.numeric(as.character(Sys.getenv("MAXID")))

# File names
output.files <- dir("Simulation_Results_Test", full.names = T)

# Get File IDs
simids <- gsub("Simulation_Results_Test/Sim_","", output.files)
simids <- gsub(".rdata","", simids)
simids <- as.numeric(simids)

# Filter File IDS
#output.files <- output.files[simids >= 80003 & simids <= 155000]

# Create Empty Objects for Results
results <-
	tibble(simid = numeric(),
	       competingJoint = list(),
	       deathJoint = list(),
	       dischargeJoint = list(),
	       competingError = numeric(),
	       deathError = numeric(),
	       dischargeError = numeric(),
	       deaths = numeric(),
	       discharges = numeric(),
	       events =  numeric(),
	       deaths.trt = numeric(),
	       discharges.trt = numeric(),
	       events.trt =  numeric(),
	       deaths.pl = numeric(),
	       discharges.pl = numeric(),
	       events.pl =  numeric(),
	       betaR= numeric(), etaR= numeric(),
	       betaD= numeric(), etaD= numeric(),
	       betaD2= numeric(), etaD2= numeric(),
	       theta= numeric(), alpha1= numeric(), alpha2= numeric(),
	       trtR= numeric(), trtD= numeric(), trtD2= numeric())

for(i in 1:length(output.files)){
	model <- readRDS(output.files[i])

	results <-
		tibble(simid = model$simulation.id,
		       competingJoint = list(model$summary.table),
		       deathJoint = list(model$initialization$summary.table1),
		       dischargeJoint = list(model$initialization$summary.table2),
		       competingError = model$critCV[2],
		       deathError = model$initialization$joint1$istop,
		       dischargeError = model$initialization$joint2$istop,
		       deaths = mean(model$icdc0),
		       discharges = mean(1 - model$icdc0),
		       events =  sum(model$ic0)/1500,
		       deaths.trt = mean(model$icdc0[model$vaxdc0==1]),
		       discharges.trt = mean(1 - model$icdc0[model$vaxdc0==1]),
		       events.trt =  sum(model$ic0[model$vax0 == 1])/sum(model$vax0 == 1),
		       deaths.pl = mean(model$icdc0[model$vaxdc0==0]),
		       discharges.pl = mean(1 - model$icdc0[model$vaxdc0==0]),
		       events.pl =  sum(model$ic0[model$vax0 == 0])/sum(model$vax0 == 0)) %>%
		bind_cols(as.data.frame(t(model$simulation.values))) %>%
		bind_rows(results)
}


minid <- min(results$simid)
maxid <- max(results$simid)

save(results,
     file = paste0("Gathered_Results/Test_Simulation_Results_",minid,"_",maxid,".rdata"))
