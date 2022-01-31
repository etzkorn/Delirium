
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


# file<- c("../simulation_results/Simulation_Results80000.rdata")
# load(file)
# results <-
# 	tibble(simid = simid,
# 	       competingJoint = competingJoint,
# 	       deathJoint = deathJoint,
# 	       dischargeJoint = dischargeJoint,
# 	       competingError = competingError,
# 	       deathError = deathError,
# 	       dischargeError = dischargeError) %>%
# 	bind_cols(as.data.frame(do.call(Truth, what = "rbind"))) %>%
# filter(simid < 80001)


for(file in file.list){
load(file)
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
group_by(scenario, Parameter) %>%
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
Truth <- do.call(Truth, what = "rbind") %>% as.data.frame
Truth$simid <- simid
Truth$scenario <- simid

simids1 <- simid[Truth$theta == 0.2 & Truth$alpha2 == -0.5 & Truth$alpha1 == 0.5 &
	     	Truth$trtR == -0.1 & Truth$trtD == -0.1 & Truth$trtD2 == 0.1]
scenarios1 <- merged %>% filter(simid%in%simids1) %>% group_by(scenario) %>% summarise()  %>% unlist
simids2 <- simid[Truth$alpha1 == 0.5 & Truth$trtR == -0.1 & Truth$trtD == -0.1 & Truth$theta == 0.2]
scenarios2 <- merged %>% filter(simid%in%simids2) %>% group_by(scenario) %>% summarise()  %>% unlist

######################################################################3
#### Tabulate Results (One Example Table)
sumtab %>%
ungroup %>%
filter(scenario %in% scenarios1) %>%
mutate(order = sapply(Parameter,
	          function(x) which(merged$Parameter[1:12] == x))) %>%
arrange(scenario, order) %>%
dplyr::select(-order) %>%
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
cols_align(columns = c(6,9,12), align = c("center")) %>%
as_latex %>% as.character %>% cat

######################################################################3
#### Tabulate Results (Across Alpha2, TrtD2)
sumtab %>%
ungroup %>%
filter(scenario %in% scenarios2,
       Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
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
cols_align(columns = c(6,9), align = c("center")) %>%
as_latex() %>%
as.character() %>%
cat()

#### Tabulate A Different way (Across Alpha2, TrtD2)
sumtab %>%
ungroup %>%
filter(scenario %in% scenarios2,
       Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
select(-Mean.discharge:-Correct.discharge, -Mean:-Correct) %>%
mutate(cell = paste0(round(Mean.death, 2), " (",round(Correct.death,2),"%)"))
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

truth %>% select(-simid) %>%
distinct() %>%
pivot_wider(id_cols = c("scenario"),
	names_from = "Parameter", values_from = "Truth") %>%
arrange(scenario) %>%
right_join(sumtab, by = "scenario") %>%
filter(scenario %in% scenarios2,
       Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
mutate(`Alpha, Terminal2` =  factor(`Alpha, Terminal2`,
			levels = c(-0.5, 0, 0.5),
			labels = c("...longer time\nto discharge.",
			           "...similar time\nto discharge.",
			           "...shorter time\nto discharge."),
			ordered = T),
			`Terminal2: trt` =  factor( `Terminal2: trt` ,
					    levels = c(-0.1, 0, 0.1),
					    labels = c("...lengthens time\nto discharge.",
					               "...does not\nchange time\nto discharge.",
					               "...shortens time\nto discharge."),
					    ordered = T)) %>%
select(scenario, Parameter, Mean.death:Correct.death, `Terminal2: trt`, `Alpha, Terminal2`) %>%
mutate(lab1 = paste0("beta_r = ",
	         round(Mean.death, 2),
	         "(",round(Correct.death,1),")"),
       lab2 =paste0("beta_1 = ", round(Mean.death, 2),
       	 "(",round(Correct.death,1),")"),
       lab3 = paste0("alpha_1 = ", round(Mean.death, 2),
       	  "(",round(Correct.death,1),")"),
       lab = ifelse(Parameter == "Alpha, Terminal1", lab3,
		ifelse(Parameter == "Recurrent: trt", lab1, lab2))) %>%
select(-lab1:-lab3, -Mean.death:-Correct.death) %>%
#group_by(scenario, `Terminal2: trt`, `Alpha, Terminal2`) %>%
#summarise(lab = paste(lab,collapse = "")) %>%
#ungroup %>%
pivot_wider(names_from = `Alpha, Terminal2`, values_from = lab, id_cols = c("Terminal2: trt","Parameter")) %>%
mutate(`Terminal2: trt` = ifelse(Parameter == "Alpha, Terminal1",as.character(`Terminal2: trt`),"")) %>%
select(-Parameter) %>%
knitr::kable(format = "latex",escape = F)

### Plot Table
plottab1 <-
truth %>% select(-simid) %>%
distinct() %>%
pivot_wider(id_cols = c("scenario"),
	names_from = "Parameter", values_from = "Truth") %>%
arrange(scenario) %>%
right_join(sumtab, by = "scenario") %>%
filter(scenario %in% scenarios2,
       Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
mutate(`Alpha, Terminal2` =  factor(`Alpha, Terminal2`,
			levels = c(-0.5, 0, 0.5),
			labels = c("...longer time\nto discharge.",
			           "...similar time\nto discharge.",
			           "...shorter time\nto discharge."),
			ordered = T),
       `Terminal2: trt` =  factor( `Terminal2: trt` ,
       		     levels = c(-0.1, 0, 0.1),
       		     labels = c("...lengthens time\nto discharge.",
       		                "...does not\nchange time\nto discharge.",
       		                "...shortens time\nto discharge."),
       		     ordered = T)) %>%
select(-Mean.discharge:-Correct.discharge) %>%
mutate(lab1 = paste0("hat(beta)[r] == ",
	         round(Mean.death, 2),
	         "~(",round(Correct.death,1),")"),
       lab2 =paste0("hat(beta)[1] == ", round(Mean.death, 2),
       	 "~(",round(Correct.death,1),")"),
       lab3 = paste0("hat(alpha)[1] == ", round(Mean.death, 2),
       	  "~(",round(Correct.death,1),")"))

png("../simulation_results/ExampleTable9.png",
    width = 1200, height = 900)
ggplot() +
geom_text(data = filter(plottab1, Parameter == "Recurrent: trt"),
          aes(x = `Alpha, Terminal2`, y = `Terminal2: trt`,
              label = lab1),
          nudge_y = .2, parse = T, size = 12) +
geom_text(data = filter(plottab1, Parameter == "Terminal1: trt"),
          aes(x = `Alpha, Terminal2`, y = `Terminal2: trt`,
              label = lab2),
          nudge_y = 0, parse = T, size = 12) +
geom_text(data = filter(plottab1, Parameter == "Alpha, Terminal1"),
          aes(x = `Alpha, Terminal2`, y = `Terminal2: trt`,
              label = lab3),
          nudge_y = -.2, parse = T, size = 12) +
ylab("")+
scale_x_discrete("People with higher daily rate of delirium have...",
	     position = "top") +
annotate("text", x = -.1, y = 3.25, label = "Treatment...", size = 14)+
coord_cartesian(clip = "off", xlim = c(1,3)) +
theme_bw(30)+
theme(axis.title.x = element_text(hjust = 0, size = 38),
      panel.grid =  element_blank(),
      axis.text = element_text(size = 30)) +
labs(caption = bquote(beta[r]==-0.1~~~~beta[1]==-0.1~~~~alpha[1]==-0.5))
dev.off()

######################################################################3
plottab <-
sumtab %>%
mutate(order = sapply(Parameter,
	          function(x) which(competingJoint[[1]]$Parameter == x)),
       bias = Mean - Truth,
       bias.death = Mean.death - Truth,
       bias.discharge = Mean.discharge - Truth)

plottab %>%
filter(Parameter=="Alpha, Terminal1")%>%
ggplot(aes(x = bias, y = bias.death))+
geom_point()+
#facet_wrap("Parameter", scales = "free") +
geom_vline(xintercept = 0, col = "blue")+
geom_hline(yintercept = 0, col = "blue")+
coord_fixed(xlim = c(-0.5,1.75), ylim= c(-0.5,1.75))

###################################################
### Plot Bias as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_Bias_Boxplots.png",
    width = 600, height = 800)
gridExtra::grid.arrange(
plottab %>%
filter(Parameter=="Recurrent: trt") %>%
bind_cols(Truth2) %>%
ggplot(aes(x = bias,
           fill = factor(trtD2),
           y = factor(alpha2)))+
geom_vline(xintercept = 0, linetype=3)+
geom_boxplot(alpha = 0.5, position="dodge") +
xlim(-0.02, 0.02)+
ylab("Alpha2")+
xlab("Bias")+
ggtitle("Competing Joint Model") +
scale_fill_discrete("Discharge\nTreatment\nEffect") +
theme_classic(20),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
bind_cols(Truth2) %>%
ggplot(aes(x = bias.death,
           fill = factor(trtD2),
           y = factor(alpha2)))+
geom_vline(xintercept = 0, linetype=3)+
geom_boxplot(alpha = 0.5) +
xlim(-0.02, 0.02) +
ylab("Alpha2") +
xlab("Bias")+
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
bind_cols(Truth2) %>%
ggplot(aes(y = Correct,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 95, linetype=3)+
geom_boxplot(alpha = 0.5, position="dodge") +
ylim(50, 100)+
xlab("Alpha2")+
ylab("95% CI Coverage for Treatment\nEffect on Delirium")+
ggtitle("Competing Joint Model") +
scale_fill_discrete("Discharge\nTreatment\nEffect") +
theme_classic(20)+
theme(legend.position = c(.8,.2)),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
bind_cols(Truth2) %>%
ggplot(aes(y = Correct.death,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 95, linetype=3)+
geom_boxplot(alpha = 0.5) +
ylim(50, 100) +
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



