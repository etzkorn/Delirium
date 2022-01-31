
rm(list = ls())
library(tidyverse)
library(gt)
library(grDevices)

file = "../simulation_results/Simulation_Results80000.rdata"
load(file)

results <-
tibble(simid = simid,
       competingJoint = competingJoint,
       deathJoint = deathJoint,
       dischargeJoint = dischargeJoint,
       competingError = competingError,
       deathError = deathError,
       dischargeError = dischargeError) %>%
bind_cols(as.data.frame(do.call(Truth, what = "rbind")))

# True Values
truth <- dplyr::select(results, simid, betaR:trtD2) %>%
	pivot_longer(names_to = "Parameter", values_to = "Truth", cols = betaR:trtD2) %>%
	mutate(Parameter = rep(results$competingJoint[[1]]$Parameter, nrow(results)),
	       scenario = ((simid-1) %% 729)+1)

# Competing Joint Model
results1 <- results %>%
	select(competingJoint, simid,betaR:trtD2, competingError) %>%
	unnest(competingJoint)%>%
	select(-Raw, -Raw.SE, -p, -H0) %>%
	mutate(Estimate = ifelse(Parameter == "Sigma", Estimate^2, Estimate),
	       LB95 = ifelse(Parameter == "Sigma", LB95^2, LB95),
	       UB95 = ifelse(Parameter == "Sigma", UB95^2, UB95))

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

save(sumtab, file = "Averaged_Estimates_First80k.rdata")

######################################################################3
#### Find scenarios that I want to examine
scenarios1 <- sumtab %>%
	filter(theta == 0.5 & alpha2 == -0.5 & alpha1 == 0.5 &
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

# Here the confidence interval coverage for the joint model looks ok
# except for alpha1

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


######################################################################
# Plots
plottab <-
sumtab %>%
mutate(order = sapply(Parameter,
	          function(x) which(competingJoint[[1]]$Parameter == x)),
	       bias = Mean - Truth,
       bias.death = Mean.death - Truth,
       bias.discharge = Mean.discharge - Truth)

# Examine Calculations
plottab %>% ungroup %>%
select(Parameter, Truth, Mean, bias, Mean.death, bias.death) %>%
filter(Parameter == "Recurrent: trt")

###################################################
### Plot Bias as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_Bias_Boxplots.png",
    width = 600, height = 800)
gridExtra::grid.arrange(
plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = bias,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 0, linetype=3)+
geom_boxplot(alpha = 0.5, position="dodge") +
ylim(-0.02, 0.02)+
xlab("Alpha2")+
ylab("Bias")+
ggtitle("Competing Joint Model") +
scale_fill_discrete("Discharge\nTreatment\nEffect") +
theme_classic(20),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = bias.death,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 0, linetype=3)+
geom_boxplot(alpha = 0.5) +
ylim(-0.02, 0.02) +
xlab("Alpha2") +
ylab("Bias")+
ggtitle("Joint Death Model")+
scale_fill_discrete("Discharge\nTreatment\nEffect")+
theme_classic(20),
nrow = 2
)
dev.off()


###################################################
### Plot Correctness as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_Correctness_Boxplots.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = Correct,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 95, linetype=3)+
geom_boxplot(alpha = 0.5, position="dodge") +
ylim(80, 100)+
xlab("Alpha2")+
ylab("95% CI Coverage for Treatment\nEffect on Delirium")+
ggtitle("Competing Joint Model") +
scale_fill_discrete("Discharge\nTreatment\nEffect") +
theme_classic(20)+
theme(legend.position = c(.8,.2)),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = Correct.death,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 95, linetype=3)+
geom_boxplot(alpha = 0.5) +
ylim(80, 100) +
xlab("Alpha2") +
ggtitle("Joint Death Model")+
scale_fill_discrete("Discharge\nTreatment\nEffect")+
theme_classic(20)+
theme(legend.position = "none",
      #axis.title.x = element_blank(),
      axis.title.y = element_blank()),
nrow = 1
)
dev.off()


###################################################
### Model Bias of trtR as Function of all Parameters

plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	bind_cols(Truth2)%>%
	lm(formula = Mean ~ theta+alpha1+alpha2+trtR+trtD+trtD2) %>%
	summary

plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	bind_cols(Truth2)%>%
	lm(formula = Mean.death ~ theta+alpha1+alpha2+factor(trtR)+factor(trtD)+factor(trtD2)) %>%
	summary

### Examine Parameters Biasing trtR
plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	bind_cols(Truth2)%>%
	arrange(abs(bias.death)) %>%
	dplyr::select(Parameter, bias.death, theta : trtD2) %>%
	head(20) %>% arrange(bias.death)

###################################################
### Plot Bias of Alpha2 as Function of all Parameters

plottab %>%
	filter(Parameter=="Alpha, Terminal1") %>%
	bind_cols(Truth2)%>%
	lm(formula = Mean ~ factor(theta)+factor(alpha1)+factor(alpha2)+
	   	factor(trtR)+factor(trtD)+factor(trtD2)) %>%
	summary
# alpha1, alpha2

plottab %>%
	filter(Parameter=="Alpha, Terminal1") %>%
	bind_cols(Truth2)%>%
	lm(formula = MeanDeath ~ factor(theta)+factor(alpha1)+factor(alpha2)+
	   	factor(trtR)+factor(trtD)+factor(trtD2)) %>%
	summary
# alpha1, alpha2, theta, trtD2

###################################################
### Model Bias of Alpha2 as Function of all Parameters
gridExtra::grid.arrange(
	plottab %>%
		filter(Parameter=="Alpha, Terminal1") %>%
		bind_cols(Truth2) %>%
		ggplot(aes(x = bias,
		           fill = factor(theta),
		           y = factor(alpha2)))+
		geom_vline(xintercept = 0, linetype=3)+
		geom_boxplot(alpha = 0.5, position="dodge") +
		xlim(-0.5, 2)+
		ylab("Alpha2")+
		xlab("Bias")+
		ggtitle("Competing Joint Model") +
		scale_fill_discrete("Discharge\nTreatment\nEffect") +
		theme_classic(20),
	plottab %>%
		filter(Parameter=="Alpha, Terminal1") %>%
		bind_cols(Truth2) %>%
		ggplot(aes(x = bias.death,
		           fill = factor(theta),
		           y = factor(alpha2)))+
		geom_vline(xintercept = 0, linetype=3)+
		geom_boxplot(alpha = 0.5) +
		xlim(-0.5, 2) +
		ylab("Alpha2") +
		xlab("Bias")+
		ggtitle("Joint Death Model")+
		scale_fill_discrete("Discharge\nTreatment\nEffect")+
		theme_classic(20),
	nrow = 2
)





