
############## Cluster
nodeid <- as.numeric(as.character(Sys.getenv("SGE_TASK_ID")))
# Task ids will be 1:8
#start.index <- ((0:7)*10000 + 1)[nodeid]
output.files <- dir("Simulation_Results", full.names = T)#[start.index:(start.index+9999)]
simids <- gsub("Simulation_Results/Sim_","", output.files)
simids <- gsub(".rdata","", simids)
simids <- as.numeric(simids)
output.files <- output.files[simids > 80000]

# Create Empty Objects for Results
competingJoint <- list()
deathJoint <- list()
dischargeJoint <- list()

competingError <- rep(0, length = length(output.files))
deathError <- rep(0, length = length(output.files))
dischargeError <- rep(0, length = length(output.files))

Truth <- list()
simid <- rep(0, length = length(output.files))

b0 <- matrix(0, nrow = length(output.files),ncol = 12)

for(i in 1:length(output.files)){
	model <- readRDS(output.files[i])
	competingJoint[[i]] <- model$summary.table
	deathJoint[[i]] <- model$initialization$summary.table1
	dischargeJoint[[i]] <- model$initialization$summary.table2

	competingError[i] <- model$critCV[2]
	deathError[i] <- model$initialization$joint1$istop
	dischargeError[i] <- model$initialization$joint2$istop

	Truth[[i]] <- model$simulation.values
	simid[i] <- model$simulation.id

	b0[i,] <- model$b
}

now <- gsub(" ","_",date())

save(Truth, simid, competingJoint, deathJoint, dischargeJoint,
     competingError, deathError, dischargeError, b0,
     file = paste0("Gathered_Results/Simulation_Results_",now,".rdata"))
