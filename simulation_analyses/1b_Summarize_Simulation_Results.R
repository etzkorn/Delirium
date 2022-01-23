
rm(list = ls())
library(tidyverse)
library(gt)
library(grDevices)

#file.list <- c("../simulation_results/Simulation_Results_Fri_Jan_14_12:04:38_2022.rdata")
file.list <- c("../simulation_results/Simulation_Results_100001_152000.rdata")
#file = "../simulation_results/Test_Simulation_Results_1_1000.rdata"
#file = "../simulation_results/Simulation_Results_155001_161746.rdata"

#file = file.list[2]
#meta <- readRDS("../simulation_results/Simulation_Values_MetaData.rdata")

for(file in file.list){
load(file)

if(file == "../simulation_results/Simulation_Results80000.rdata"){
	results <-
		tibble(simid = simid,
		       competingJoint = competingJoint,
		       deathJoint = deathJoint,
		       dischargeJoint = dischargeJoint,
		       competingError = competingError,
		       deathError = deathError,
		       dischargeError = dischargeError) %>%
		bind_cols(as.data.frame(do.call(Truth, what = "rbind"))) %>%
	filter(simid < 80001)
}

truth <- dplyr::select(results, simid, betaR:trtD2) %>%
	pivot_longer(names_to = "Parameter", values_to = "Truth", cols = betaR:trtD2) %>%
	mutate(Parameter = rep(results$competingJoint[[1]]$Parameter, nrow(results)),
	       scenario = ((simid-1) %% 729)+1)

results1 <- results %>%
	select(competingJoint, simid,betaR:trtD2, competingError) %>%
	unnest(competingJoint)%>%
	select(-Raw, -Raw.SE, -p, -H0) %>%
	mutate(Estimate = ifelse(Parameter == "Sigma", Estimate^2, Estimate),
	       LB95 = ifelse(Parameter == "Sigma", LB95^2, LB95),
	       UB95 = ifelse(Parameter == "Sigma", UB95^2, UB95))

#results1$scenario <- select(results1, betaR:trtD2) %>% apply(1, paste0, collapse=" ") %>% factor %>% as.numeric

# Death Joint Model
results2 <-results %>%
	select(deathJoint, simid, deathError) %>%
	unnest(deathJoint) %>%
	select(-Raw, -Raw.SE, -p) %>%
	mutate(
	LB95 = Estimate - 2*Estimate.SE,
	UB95 = Estimate + 2*Estimate.SE)

# Discharge Joint Model
results3 <- results %>%
	select(dischargeJoint, simid, dischargeError) %>%
	unnest(dischargeJoint) %>%
	select(-Raw, -Raw.SE, -p) %>%
	mutate(
		LB95 = Estimate - 2*Estimate.SE,
		UB95 = Estimate + 2*Estimate.SE,
		Parameter = gsub("1","2",Parameter))

### Merge Results
merged <- results1 %>% ungroup %>%
left_join(results2, by = c("Parameter","simid"), suffix = c("",".death")) %>%
left_join(results3, by = c("Parameter","simid"), suffix = c("",".discharge")) %>%
left_join(truth, by = c("Parameter","simid"))

rm(results, results1, results2, results3)
}

######################################################################
### Summarize Results

sumtab <-
merged %>%
#mutate( old = simid <80001) %>%
group_by(scenario, Parameter,
         betaR, etaR, betaD, etaD, betaD2, etaD2, theta, alpha1, alpha2,
         trtR, trtD, trtD2) %>%
summarise(
     Truth = Truth[1],
     Mean = mean(Estimate[competingError==1]),
     SD = sd(Estimate[competingError==1], na.rm=T),
     Correct = 100*mean((LB95 < Truth & UB95 > Truth)[competingError==1]),
     Mean.death = mean(Estimate.death[deathError==1]),
     SD.death = sd(Estimate.death[deathError==1], na.rm=T),
     Correct.death = 100*mean((LB95.death < Truth & UB95.death > Truth)[deathError==1]),
     Mean.discharge = mean(Estimate.discharge[dischargeError==1]),
     SD.discharge = sd(Estimate.discharge[dischargeError==1], na.rm=T),
     Correct.discharge = 100*mean((LB95.discharge < Truth & UB95.discharge > Truth)[dischargeError==1])
)

save(sumtab, file = "Averaged_Estimates.rdata")

######################################################################3
#### Find scenarios that I want to examine
scenarios1 <- sumtab %>%
	filter(theta == 0.2 & alpha2 == -0.5 & alpha1 == 0.5 &
	     	trtR == -0.1 & trtD == -0.1 & trtD2 == 0.1)
scenarios2 <- sumtab %>%
	filter(theta == 0.2 & alpha1 == 0.5 & trtR == -0.1 & trtD == -0.1 )

######################################################################3
#### Tabulate Results (One Example Table)
scenarios1 %>% ungroup %>%
mutate(order = sapply(Parameter,
	          function(x) which(merged$Parameter[1:12] == x))) %>%
arrange(scenario, order) %>%
dplyr::select(scenario, Parameter, Truth:Correct.discharge) %>%
gt(rowname_col = "Parameter",
   groupname_col = "scenario")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
		     round(nrow(merged)/12/729),", n = ", 1500,")**")))%>%
cols_label(Mean = html("Competing Estimate"),
           Mean.death = html("Death Estimate"),
           Mean.discharge = html("Discharge Estimate"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           Correct.discharge = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
           SD.discharge = html("SE")
	) %>%
fmt_number(c(4,7,10)) %>%
fmt_number(c(5,8,11),pattern = "({x})")%>%
fmt_number(1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9,12),decimals = 1) %>%
fmt_missing(columns = 1:12, missing_text = "") %>%
cols_align(columns = 3, align = c("center")) %>%
cols_align(columns = c(5,8,11), align = c("left")) %>%
cols_align(columns = c(6,9,12), align = c("center")) #%>%
#as_latex %>% as.character %>% cat

######################################################################3
#### Tabulate Results (Across Alpha2, TrtD2)
scenarios2 %>% ungroup %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
dplyr::select(scenario, Parameter, Truth:Correct.discharge) %>%
select(-Mean.discharge:-Correct.discharge) %>%
gt(rowname_col = "Parameter",
   groupname_col = "scenario")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
		     round(nrow(merged)/12/729),", n = ", 1500,")**")))%>%
cols_label(Mean = html("Competing Estimate"),
           Mean.death = html("Death Estimate"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
) %>%
fmt_number(c(4,7)) %>%
fmt_number(c(5,8),pattern = "({x})")%>%
fmt_number(1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9),decimals = 1) %>%
fmt_missing(columns = 1:9, missing_text = "") %>%
cols_align(columns = 3, align = c("center")) %>%
cols_align(columns = c(5,8), align = c("left")) %>%
cols_align(columns = c(6,9), align = c("center")) #%>%
#as_latex() %>%as.character() %>%cat()

#### Tabulate A Different way (Across Alpha2, TrtD2)
scenarios2%>%
ungroup %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
select(-Mean.discharge:-Correct.discharge, -Mean:-Correct) %>%
mutate(cell = paste0(round(Mean.death, 2), " (",round(Correct.death,2),"%)")) %>%
	gt(rowname_col = "Parameter",
	   groupname_col = "scenario")%>%
	tab_stubhead(label = "Parameter")%>%
	tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
			     round(nrow(merged)/12/729),", n = ", 1500,")**")))%>%
	cols_label(Mean = html("Competing Estimate"),
	           Mean.death = html("Death Estimate"),
	           Correct = html("Correct %"),
	           Correct.death = html("Correct %"),
	           SD = html("SE"),
	           SD.death = html("SE"),
	) %>%
	fmt_number(c(4,7)) %>%
	fmt_number(c(5,8),pattern = "({x})")%>%
	fmt_number(1, pattern = "Scenario {x}")%>%
	fmt_number(c(6,9),decimals = 1) %>%
	fmt_missing(columns = 1:9, missing_text = "") %>%
	cols_align(columns = 3, align = c("center")) %>%
	cols_align(columns = c(5,8), align = c("left")) %>%
	cols_align(columns = c(6,9), align = c("center"))
