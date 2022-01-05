
############## Cluster
output.files <- dir("Simulation_Results", full.names = T)

competingJoint <- list()
deathJoint <- list()
dischargeJoint <- list()

competingError <- rep(0, length = length(output.files))
deathError <- rep(0, length = length(output.files))
dischargeError <- rep(0, length = length(output.files))


for(i in 1:length(output.files)){
	model <- readRDS(output.files[i])
	competingJoint[[i]] <- model$summary.table
	deathJoint[[i]] <- model$initialization$summary.table1
	dischargeJoint[[i]] <- model$initialization$summary.table2

	competingError[i] <- model$critCV[2]
	deathError[i] <- model$initialization$joint1$istop
	dischargeError[i] <- model$initialization$joint2$istop

	if(i%%100 == 0) cat(i,"\n")
}

save(competingJoint, deathJoint, dischargeJoint, competingError, deathError, dischargeError,
     file = "Simulation_Results.rdata")
