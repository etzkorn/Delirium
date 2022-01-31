
rm(list = ls())
library(tidyverse)
#library(gt)

results.files <- grep("Simulation_Results_", dir(), value = T)
simid.start <- str_extract(results.files, "\\d+") %>% as.numeric()

load("../simulation_results/Simulation_Results.rdata")

dim(b0)
colMeans(b0)

#results <-tibble()
#results2 <-tibble()
#results3<-tibble()

#### Loop Through Files and Extract Summaries
#for(i in results.files){
#load(i)

results <-
b0 %>% as_tibble() %>%
mutate(simid=simid,
       r = simid %/% 5000,
       Truth = map(Truth, ~as.matrix(.) %>% t() %>% as_tibble)) %>%
unnest(Truth) %>%
group_by(r) %>%
summarise(BetaR = mean(exp(V1)^2),
          betaR=betaR[1],
          EtaR = mean(exp(V2)^2),
          etaR=etaR[1],
          BetaD = mean(exp(V3)^2),
          betaD=betaD[1],
          EtaD = mean(exp(V4)^2),
          etaD=etaD[1],
          BetaD2 = mean(exp(V5)^2),
          betaD2=betaD2[1],
          EtaD2 = mean(exp(V6)^2),
          etaD2=etaD2[1],
          Theta = mean(exp(V7)^2),
          theta=theta[1],
          Alpha1 = mean(V8), alpha1=alpha1[1],
          Alpha2 = mean(V9), alpha2=alpha2[1],
          TRTR = mean(V10), trtR=trtR[1],
          TRTD = mean(V11), trtD=trtD[1],
          TRTD2 = mean(V12), trtD2=trtD2[1]) %>%
ungroup %>%
arrange(r)

results %>% select(Theta:trtD2)

# Death Joint Model
results2 <-
	tibble(simid = simid,
	       r = simid %/% 5000,
	       Parameter = map(deathJoint, ~.$Parameter),
	       Estimate.Death = map(deathJoint, ~.$Estimate),
	       Error.Death = factor(deathError,
	       	   labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) #%>%
	#bind_rows(results2)

# Discharge Joint Model
results3 <-
	tibble(simid = simid,
	       r = simid %/% 5000,
	       Parameter = map(dischargeJoint, ~.$Parameter),
	       Estimate.Discharge = map(dischargeJoint, ~.$Estimate),
	       Error.Discharge = factor(dischargeError,
	       	         labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) #%>%
	#bind_rows(results3)
#}

results <- results %>%
	arrange(r, simid) %>%
	distinct(r, simid, .keep_all = T) %>%
	unnest(c(Parameter, Truth, Estimate))%>%
	group_by(r, Parameter) %>%
	filter(!n()==1)%>%
	summarise(Truth = Truth[1],
	          Mean = mean(Estimate[error=="None"]),
	          SD = sd(Estimate[error=="None"], na.rm=T),
	)

results2 <- results2 %>%
	arrange(r, simid) %>%
	distinct(r, simid, .keep_all = T) %>%
	unnest(c(Parameter, Estimate.Death))%>%
	group_by(r,Parameter) %>%
	summarise(Mean.Death = mean(Estimate.Death[Error.Death=="None"]),
	          SD.Death = sd(Estimate.Death[Error.Death=="None"], na.rm=T),
	)

results3 <- results3 %>%
	arrange(r, simid) %>%
	distinct(r, simid, .keep_all = T) %>%
	unnest(c(Parameter, Estimate.Discharge))%>%
	group_by(r,Parameter) %>%
	summarise(Mean.Discharge = mean(Estimate.Discharge[Error.Discharge=="None"]),
	          SD.Discharge = sd(Estimate.Discharge[Error.Discharge=="None"], na.rm=T),
	)


###### Tabulate Results
results <-
results %>% ungroup %>%
left_join(results2, by = c("Parameter","r")) %>%
left_join(results3, by = c("Parameter","r")) %>%
mutate(r = paste("Scenario",r+1))

save(results, file = "Gathered_Results.rdata")
#
# gt(rowname_col = "Parameter",
#    groupname_col = "r")%>%
# tab_stubhead(label = "Parameter")%>%
# tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
# 		     nrow(5000),", n = ", 1500,")**")))%>%
# cols_label(Mean = html("Competing Estimate"),
#            Mean.Death = html("Death Estimate"),
#            Mean.Discharge = html("Discharge Estimate"),
#            SD = html("SE"),
#            SD.Death = html("SE"),
#            SD.Discharge = html("SE")
# 	) %>%
# 	fmt_number(c(5,7,9),pattern = "({x})")%>%
# 	fmt_number(c(4,6,8)) %>%
# 	fmt_missing(columns = 1:9, missing_text = "") %>%
# 	cols_align(columns = c(5,6,9), align = c("left"))
#
#
