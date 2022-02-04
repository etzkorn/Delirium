
# Note: At the time these models were run, there was a problem with how
# some parameters (alpha, sigma) from the initialization models were transformed.
# This has since been fixed in the local software, and the lines below that
# correct the previous mistake should be removed if models are fit using the updated software.

rm(list = ls())
library(tidyverse)
library(gt)
library(grDevices)

file.list <- grep("Scenario",dir("../simulation_results/",full.names = T),value = T)
merged <- tibble()

for(file in file.list){
load(file)

truth <- dplyr::select(results, scenario, simid, truth) %>%
	unnest(truth) %>%
	pivot_longer(names_to = "Parameter", values_to = "Truth", cols = betaR:trtD2) %>%
	mutate(Parameter = rep(results$summary.table[[1]]$Parameter, nrow(results)))

results1 <- results %>%
	unnest(truth) %>%
	dplyr::select(summary.table, simid, betaR:trtD2,  competingError=critCV) %>%
	unnest(summary.table) %>%
	dplyr::select(-Raw, -Raw.SE, -p, -H0)

# Death Joint Model
results2 <-results %>%
	dplyr::select(summary.table1, simid, deathError=istop1) %>%
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
	dplyr::select(-Raw, -Raw.SE, -p)

# Discharge Joint Model
results3 <- results %>%
	dplyr::select(summary.table2, simid, dischargeError=istop2) %>%
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
Parameter = gsub("1","2",Parameter)) %>%
	dplyr::select(-Raw, -Raw.SE, -p)

### Merge Results
merged <- results1 %>% ungroup %>%
left_join(results2, by = c("Parameter","simid"), suffix = c("",".death")) %>%
left_join(results3, by = c("Parameter","simid"), suffix = c("",".discharge")) %>%
left_join(truth, by = c("Parameter","simid")) %>%
bind_rows(merged)

rm(results, results1, results2, results3)
}

rm(file, file.list, truth)

######################################################################
### Summarize Results

sumtab <-
merged %>%
#mutate( old = simid <80001) %>%
group_by(scenario, Parameter,
         betaR, etaR, betaD, etaD, betaD2, etaD2, sigma, alpha1, alpha2,
         trtR, trtD, trtD2) %>%
summarise(
     Truth = Truth[1],
     #Mean = mean(Estimate[competingError==1]) - Truth,
     bias = mean(Estimate[competingError==1]) - Truth,
     SD = sd(Estimate[competingError==1], na.rm=T),
     Correct = 100*mean((LB95 < Truth & UB95 > Truth)[competingError==1]),
     #Mean.death = mean(Estimate.death[deathError==1])- Truth,
     bias.death = mean(Estimate.death[deathError==1])- Truth,
     SD.death = sd(Estimate.death[deathError==1], na.rm=T),
     Correct.death = 100*mean((LB95.death < Truth & UB95.death > Truth)[deathError==1]),
     #Mean.discharge = mean(Estimate.discharge[dischargeError==1])- Truth,
     bias.discharge = mean(Estimate.discharge[dischargeError==1])- Truth,
     SD.discharge = sd(Estimate.discharge[dischargeError==1], na.rm=T),
     Correct.discharge = 100*mean((LB95.discharge < Truth & UB95.discharge > Truth)[dischargeError==1]),
     n = sum(competingError==1),
     n.death = sum(deathError==1, na.rm=T),
     n.discharge = sum(dischargeError==1, na.rm=T)
)

save(sumtab, file = paste0("Averaged_Estimates",substr(date(),0,11),".rdata"))
#load("Averaged_EstimatesWed Feb  2 .rdata")

######################################################################3
#### Tabulate Results (One Example Table)
sumtab %>% ungroup %>%
filter(scenario==260) %>% # alpha2 = 0, betaD2 = 0
mutate(order = sapply(Parameter,
	          function(x) which(merged$Parameter[1:12] == x))) %>%
arrange(scenario, order) %>%
dplyr::select(scenario, Parameter, Truth:Correct.discharge) %>%
gt(rowname_col = "Parameter",
   groupname_col = "scenario")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
     round(nrow(merged)/12/length(unique(merged$scenario))),
     ", n = ", 1500,")**")))%>%
cols_label(#Mean = html("Competing Estimate"),
           #Mean.death = html("Death Estimate"),
           #Mean.discharge = html("Discharge Estimate"),
           bias = html("Competing Estimate"),
           bias.death = html("Death Estimate"),
           bias.discharge = html("Discharge Estimate"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           Correct.discharge = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
           SD.discharge = html("SE")
	) %>%
fmt_number(c(4,7,10),decimals = 4) %>%
fmt_number(c(5,8,11),pattern = "({x})",decimals = 4)%>%
fmt_number(1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9,12),decimals = 1) %>%
fmt_missing(columns = 1:12, missing_text = "") %>%
cols_align(columns = 3, align = c("center")) %>%
cols_align(columns = c(5,8,11), align = c("left")) %>%
cols_align(columns = c(6,9,12), align = c("center")) #%>%
as_latex %>% as.character %>% cat

######################################################################3
#### Tabulate Results (Across Alpha2, TrtD2)
sumtab %>% ungroup %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
dplyr::select(scenario, Parameter, trtD2, alpha2, bias:Correct.discharge) %>%
dplyr::select(-bias.discharge:-Correct.discharge) %>%
gt(rowname_col = "Parameter",
   groupname_col = "scenario")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
     round(nrow(merged)/12/length(unique(merged$scenario))),
     ", n = ", 1500,")**")))%>%
cols_label(bias = html("Competing Estimate"),
           bias.death = html("Death Estimate"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
) %>%
fmt_number(c(4,7)+1,decimals = 4) %>%
fmt_number(c(5,8)+1,pattern = "({x})",decimals = 4)%>%
fmt_number(1+1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9)+1,decimals = 1) %>%
fmt_missing(columns = (1:9)+1, missing_text = "") %>%
cols_align(columns = 3+1, align = c("center")) %>%
cols_align(columns = c(5,8)+1, align = c("left")) %>%
cols_align(columns = c(6,9)+1, align = c("center")) %>%
as_latex() %>%as.character() %>%cat()

#############################################################################
# Plot Recurrent Event Treatment Effect Bias with Bias confidence intervals
png("../simulation_results/NineScenario_Bias_TrtR.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
sumtab %>% filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 4)+
geom_point(aes(x = trtD2+alpha2/20, y = bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias-2*SD/sqrt(n), ymax = bias+2*SD/sqrt(n),
	      color = factor(alpha2), group = factor(alpha2)),
	  size = 1)+
	theme_bw(20)+
	theme(legend.position = "none")+
ylim(-0.02, 0.02)+
	scale_color_discrete(expression(alpha[2]))+
	scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
	xlab(expression(beta[2]))+
	ylab(expression(Bias~beta[r]))+
	ggtitle("Competing Death and Discharge"),

sumtab %>% filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias.death-2*SD.death/sqrt(n.death),
	      ymax = bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2), group = factor(alpha2)),
	  size = 1)+
theme_bw(20)+
theme(legend.position = c(0.5,0.85),
      legend.background = element_rect(color = "grey80"))+
ylim(-0.02, 0.02)+
scale_color_discrete(expression(alpha[2]))+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
xlab(expression(beta[2]))+
ylab(expression(Bias~beta[r]))+
ggtitle("Joint, Death"),
nrow=1)
dev.off()

#############################################################################
# Plot Death Treatment Effect Bias with Bias confidence intervals
png("../simulation_results/NineScenario_Bias_TrtD.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias-2*SD/sqrt(n),
	      ymax = bias+2*SD/sqrt(n),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
	scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(20)+
theme(legend.position = "none")+
ylim(-0.08, 0.04)+
scale_color_discrete(expression(alpha[2]))+
xlab(expression(beta[2]))+
ylab(expression(Bias~beta[1]))+
ggtitle("Competing Death and Discharge"),

sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias.death-2*SD.death/sqrt(n.death),
	      ymax = bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
	scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(20)+
theme(legend.position = c(0.35,0.25),
      legend.background = element_rect(color = "grey80"))+
ylim(-0.08, 0.04)+
scale_color_discrete(expression(alpha[2]))+
xlab(expression(beta[2]))+
ylab(expression(Bias~beta[1]))+
ggtitle("Joint Death"),
	nrow=1)
dev.off()


#############################################################################
# Plot Alpha1 Bias confidence intervals
png("../simulation_results/NineScenario_Bias_Alpha.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
	sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias-2*SD/sqrt(n),
	      ymax = bias+2*SD/sqrt(n),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(20)+
theme(legend.position = "none")+
ylim(-0.2, 0.6)+
scale_color_discrete(expression(alpha[2]))+
xlab(expression(beta[2]))+
ylab(expression(Bias~alpha[1]))+
ggtitle("Competing Death and Discharge"),

	sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
ggplot() +
geom_hline(aes(yintercept = 0), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = bias.death-2*SD.death/sqrt(n.death),
	      ymax = bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(20)+
theme(legend.position = c(0.35,0.25),
      legend.background = element_rect(color = "grey80"))+
ylim(-0.2, 0.6)+
scale_color_discrete(expression(alpha[2]))+
xlab(expression(beta[2]))+
ylab(expression(Bias~alpha[1]))+
ggtitle("Joint Death"),
	nrow=1)
dev.off()

#############################################################################
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

#####################################################################################
# Calculate p-values for bias
merged %>%
group_by(scenario, Parameter,
         betaR, etaR, betaD, etaD, betaD2, etaD2, sigma, alpha1, alpha2,
         trtR, trtD, trtD2) %>%
summarise(
	Truth = Truth[1],
	Mean = mean(Estimate[competingError==1]),
	SD = sd(Estimate[competingError==1], na.rm=T),
	n = sum(competingError==1),
	Correct = 100*mean((LB95 < Truth & UB95 > Truth)[competingError==1]),
	Mean.death = mean(Estimate.death[deathError==1]),
	SD.death = sd(Estimate.death[deathError==1], na.rm=T),
	Correct.death = 100*mean((LB95.death < Truth & UB95.death > Truth)[deathError==1]),
	n.death = sum(deathError==1),
	Mean.discharge = mean(Estimate.discharge[dischargeError==1]),
	SD.discharge = sd(Estimate.discharge[dischargeError==1], na.rm=T),
	Correct.discharge = 100*mean((LB95.discharge < Truth & UB95.discharge > Truth)[dischargeError==1]),
	n.discharge = sum(dischargeError==1)
) %>%
ungroup %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
dplyr::select(Parameter, Truth, trtD2, alpha2, Mean:SD, Mean.death:SD.death, n, n.death) %>%
mutate(t = (Mean- Truth)*sqrt(n)/SD,
       t.death = (Mean.death - Truth)*sqrt(n.death)/SD.death) %>%
arrange(Parameter, trtD2) %>%
dplyr::select(Parameter:alpha2, t, t.death) %>%
print.data.frame
