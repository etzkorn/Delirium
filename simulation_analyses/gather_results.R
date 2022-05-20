############## Cluster
library(tidyverse)

# File names
output.files <- dir("Simulation_Results_4", full.names = T)

# Get File IDs
#scenario <- gsub("Simulation_Results/Scenario_","", output.files)
#scenario <- gsub(".rdata","", scenario)
#scenario <- as.numeric(scenario)

# Filter File IDS
#output.files <- output.files[simids > 80000 & simids < 155001]

# Create Empty Objects for Results
sumtab <- tibble()

for(i in 1:length(output.files)){
	message(output.files[i],"/n")
	load(output.files[i])

	truth <- dplyr::select(results, scenario, truth) %>%
		unnest(truth) %>%
		distinct()%>%
		pivot_longer(names_to = "Parameter", values_to = "Truth", cols = betaR:trtD2) %>%
		mutate(Parameter = results$summary.table[[1]]$Parameter)

	# Competing Joint
	results1 <- results %>%
		unnest(truth) %>%
		select(summary.table, simid, betaR:trtD2,  competingError=critCV) %>%
		unnest(summary.table) %>%
		select(-Raw, -Raw.SE, -p, -H0)

	# Death Joint Model
	results2 <-results %>%
		select(summary.table1, simid, deathError=istop1) %>%
		unnest(summary.table1) %>%
		mutate(
			LB95 = Estimate - 2*Estimate.SE,
			UB95 = Estimate + 2*Estimate.SE,
			Estimate = ifelse(Parameter =="Sigma", Estimate^.5,Estimate),
			LB95 = ifelse(Parameter =="Sigma", LB95^.5,LB95),
			UB95 = ifelse(Parameter =="Sigma", UB95^.5,UB95),
			Estimate = ifelse(Parameter =="Alpha, Terminal1", Raw, Estimate),
			LB95 = ifelse(Parameter =="Alpha, Terminal1", Raw - 2*Raw.SE,LB95),
			UB95 = ifelse(Parameter =="Alpha, Terminal1", Raw + 2*Raw.SE,UB95)) %>%
		select(-Raw, -Raw.SE, -p)

	# Discharge Joint Model
	results3 <- results %>%
		select(summary.table2, simid, dischargeError=istop2) %>%
		unnest(summary.table2) %>%
		mutate(
			LB95 = Estimate - 2*Estimate.SE,
			UB95 = Estimate + 2*Estimate.SE,
			Estimate = ifelse(Parameter =="Sigma", Estimate^.5,Estimate),
			LB95 = ifelse(Parameter =="Sigma", LB95^.5,LB95),
			UB95 = ifelse(Parameter =="Sigma", UB95^.5,UB95),
			Estimate = ifelse(Parameter =="Alpha, Terminal2", Raw, Estimate),
			LB95 = ifelse(Parameter =="Alpha, Terminal2", Raw - 2*Raw.SE, LB95),
			UB95 = ifelse(Parameter =="Alpha, Terminal2", Raw + 2*Raw.SE, UB95),
			Parameter = gsub("1","2",Parameter))%>%
		select(-Raw, -Raw.SE, -p)

	### Merge Results
	sumtab <- results1 %>% ungroup %>%
		left_join(results2, by = c("Parameter","simid"), suffix = c("",".death")) %>%
		left_join(results3, by = c("Parameter","simid"), suffix = c("",".discharge")) %>%
		left_join(truth, by = c("Parameter")) %>%
		group_by(Parameter,
		         betaR, etaR, betaD, etaD, betaD2, etaD2, sigma, alpha1, alpha2,
		         trtR, trtD, trtD2) %>%
		summarise(
			Truth = Truth[1],
			Mean = mean(Estimate[competingError==1]),
			SD = sd(Estimate[competingError==1], na.rm=T),
			Correct = 100*mean((LB95 < Truth & UB95 > Truth)[competingError==1]),
			competingError = mean(competingError != 1),
			Mean.death = mean(Estimate.death[deathError==1]),
			SD.death = sd(Estimate.death[deathError==1], na.rm=T),
			Correct.death = 100*mean((LB95.death < Truth & UB95.death > Truth)[deathError==1]),
			deathError = mean(deathError != 1),
			Mean.discharge = mean(Estimate.discharge[dischargeError==1]),
			SD.discharge = sd(Estimate.discharge[dischargeError==1], na.rm=T),
			Correct.discharge = 100*mean((LB95.discharge < Truth & UB95.discharge > Truth)[dischargeError==1]),
			dischargeError = mean(dischargeError != 1)
		) %>%
		bind_rows(sumtab)
}
save(sumtab,
     file = paste0("Gathered_Results/Simulation_Results_4_",gsub(" ","_",substr(date(),0,11)),".rdata"))
