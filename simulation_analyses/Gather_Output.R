
############## Cluster
nodeid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
minid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
maxid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))

# File names
output.files <- dir("Simulation_Results", full.names = T)

# Get File IDs
simids <- gsub("Simulation_Results/Sim_","", output.files)
simids <- gsub(".rdata","", simids)
simids <- as.numeric(simids)

# Filter File IDS
output.files <- output.files[simids >= minid & simids <= maxid]

# Create Empty Objects for Results
results <-
tibble(simid = numeric(),
       competingJoint = list(),
       deathJoint = list(),
       dischargeJoint = list(),
       competingError = numeric(),
       deathError = numeric(),
       dischargeError = numeric(),
       betaR= numeric(), etaR= numeric(),
       betaD= numeric(), etaD= numeric(),
       betaD2= numeric(), etaD2= numeric(),
       theta= numeric(), alpha1= numeric(), alpha2= numeric(),
       trtR= numeric(), trtD= numeric(), trtD2= numeric())

for(i in 1:length(output.files)){
	model <- readRDS(output.files[i])

	results <-
	tibble(simid = numeric(),
	       competingJoint = list(model$summary.table),
	       deathJoint = list(model$initialization$summary.table1),
	       dischargeJoint = list(model$initialization$summary.table2),
	       competingError = numeric(model$critCV[2]),
	       deathError = numeric(model$initialization$joint1$istop),
	       dischargeError = numeric(model$initialization$joint2$istop)) %>%
	bind_cols(as.data.frame(t(model$simulation.values))) %>%
	bind_rows(results)

}

minid <- min(results$simid)
maxid <- max(results$simid)

save(results,
     file = paste0("Gathered_Results/Simulation_Results_",minid,"_",maxid,".rdata"))
