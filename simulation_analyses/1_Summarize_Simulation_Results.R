
rm(list = ls())
library(tidyverse)
library(gt)

load("../simulation_results/Simulation_Results80000.rdata")
#load("../simulation_results/Simulation_Results486.rdata")

# Competing Joint Model
results <- tibble(
	Truth = Truth,
	r = ((simid-1)%%729)+1,
	simid = simid,
	Parameter = map(competingJoint, ~.$Parameter),
	Estimate = map(competingJoint, ~.$Estimate),
	LB95 = map(competingJoint, ~.$LB95),
	UB95 = map(competingJoint, ~.$UB95),
	error = factor(competingError,
		   labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) %>%
arrange(simid) %>%
distinct(simid, .keep_all = T) %>%
unnest(c(Truth, Parameter, Estimate, LB95, UB95))

# Death Joint Model
results2 <-
tibble(
	r = ((simid-1)%%729)+1,
	simid = simid,
	Parameter = map(deathJoint, ~.$Parameter),
	Estimate = map(deathJoint, ~.$Estimate),
	LB95 = map(deathJoint, ~.$Estimate - 2*.$Estimate.SE),
	UB95 = map(deathJoint, ~.$Estimate + 2*.$Estimate.SE),
	error = factor(deathError,
		   labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) %>%
arrange(simid) %>%
distinct(simid, .keep_all = T) %>%
unnest(c(Parameter, Estimate, LB95, UB95))

# Discharge Joint Model
results3 <- tibble(
	r = ((simid-1)%%729)+1,
	simid = simid,
	Parameter = map(dischargeJoint, ~.$Parameter),
	Estimate = map(dischargeJoint, ~.$Estimate),
	LB95 = map(dischargeJoint, ~.$Estimate - 2*.$Estimate.SE),
	UB95 = map(dischargeJoint, ~.$Estimate + 2*.$Estimate.SE),
	error = factor(dischargeError,
		   labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) %>%
arrange(simid) %>%
distinct(simid, .keep_all = T) %>%
unnest(c(Parameter, Estimate, LB95, UB95))%>%
mutate(Parameter = gsub("1","2",Parameter))


### Merge Results

merged <- results %>% ungroup %>%
left_join(results2, by = c("Parameter","simid","r"), suffix = c("",".death")) %>%
left_join(results3, by = c("Parameter","simid"), suffix = c("",".discharge"))

sumtab <-
merged %>%
group_by(r, Parameter) %>%
mutate(Estimate = ifelse(Parameter == "Sigma", Estimate^2,Estimate))%>%
summarise(
          Truth = Truth[1],
          Mean = mean(Estimate[error=="None"]),
          SD = sd(Estimate[error=="None"], na.rm=T),
     Correct = 100*mean(LB95 < Truth & UB95 > Truth),
     Mean.death = mean(Estimate.death[error.death=="None"]),
     SD.death = sd(Estimate.death[error.death=="None"], na.rm=T),
     Correct.death = 100*mean(LB95.death < Truth & UB95.death > Truth),
     Mean.discharge = mean(Estimate.discharge[error.discharge=="None"]),
     SD.discharge = sd(Estimate.discharge[error.discharge=="None"], na.rm=T),
     Correct.discharge = 100*mean(LB95.discharge < Truth & UB95.discharge > Truth)
)%>%
mutate(r = paste("Scenario",r))

save(sumtab, file = "Averaged_Estimates.rdata")

######################################################################3
#### Tabulate Results
sumtab %>%
filter(r=="Scenario 1")%>%
mutate(order = sapply(Parameter,
	          function(x) which(competingJoint[[1]]$Parameter == x))) %>%
arrange(r, order) %>%
dplyr::select(-order)%>%
gt(rowname_col = "Parameter",
   groupname_col = "r")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
		     110,", n = ", 1500,")**")))%>%
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
	fmt_number(c(5,8,11),pattern = "({x})")%>%
	fmt_number(c(4,7,10)) %>%
	fmt_number(c(6,9,12),decimals = 1) %>%
	fmt_missing(columns = 1:12, missing_text = "") %>%
	cols_align(columns = c(5,8,11), align = c("left")) %>%
cols_align(columns = c(6,9,12), align = c("center"))

######################################################################3

plottab <-
sumtab %>%
mutate(order = sapply(Parameter,
	          function(x) which(competingJoint[[1]]$Parameter == x)),
       bias = Mean - Truth,
       bias.death = Mean.death - Truth,
       bias.discharge = Mean.discharge - Truth)

Truth2 <- do.call(Truth, what = "rbind") %>% as_tibble
Truth2 <- Truth2[(simid %/% 729)==1,]
# Truth2 <- Truth2 %>%
# mutate(alpha1 = factor(alpha1, levels = c(0, -0.5, 0.5),
# 	           labels  = as.character(c(0, -0.5, 0.5)), ordered = T),
#        alpha2 = factor(alpha2, levels = c(0, -0.5, 0.5),
#        	    labels  = as.character(c(0, -0.5, 0.5)), ordered = T),
#        trtD = factor(trtD, levels = c(0, -0.1, 0.1),
#        	  labels  = as.character(c(0, -0.1, 0.1)), ordered = T),
#        trtD2 = factor(trtD2, levels = c(0, -0.1, 0.1),
#        	   labels  = as.character(c(0, -0.1, 0.1)), ordered = T),
#        trtR = factor(trtR, levels = c(0, -0.1, 0.1),
#        	  labels  = as.character(c(0, -0.1, 0.1)), ordered = T))

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



