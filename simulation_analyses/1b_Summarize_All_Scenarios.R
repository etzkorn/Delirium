
rm(list = ls())
library(tidyverse)
library(gt)
library(grDevices)

load("../simulation_results/Simulation_Results_Fri_Jan_28.rdata")
load("../simulation_results/Simulation_Results_Mon_Jan_31_.rdata")
head(sumtab)

sumtab %>%
filter(Parameter == "Sigma") %>%
group_by(sigma) %>%
summarise(mean(competingError),
          mean(deathError),
          mean(dischargeError))
# sigma == 1 was likely way too high
# so we should not use results from those models

sumtab <- sumtab %>% filter(sigma <1)

######################################################################3
plottab <-
sumtab %>%
mutate(bias = Mean - Truth,
       bias.death = Mean.death - Truth,
       bias.discharge = Mean.discharge - Truth,
       mse = SD^2 + bias^2,
       mse.death = SD.death ^2 + bias.death^2,
       mse.discharge = SD.discharge ^2 + bias.discharge^2)

plottab %>%
filter(Parameter=="Alpha, Terminal1")%>%
ggplot(aes(x = bias, y = bias.death, color = factor(alpha1)))+
geom_point()+
#facet_wrap("Parameter", scales = "free") +
geom_vline(xintercept = 0, col = "blue")+
geom_hline(yintercept = 0, col = "blue")+
facet_wrap("alpha1", nrow = 2)#+
#coord_fixed(xlim = c(-1,27), ylim= c(-1,27))

###################################################
### Plot Bias as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_Bias_Boxplots.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = Mean,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = -0.25, linetype=3)+
geom_boxplot(alpha = 0.5, position="dodge") +
ylim(-0.27, -0.23)+
xlab(expression(alpha[2]))+
ylab(expression(Mean~Estimate~of~beta[r]))+
ggtitle("Competing Joint Model") +
theme_classic(20)+
	theme(legend.position = "none"),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = Mean.death,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = -0.25, linetype=3)+
geom_boxplot(alpha = 0.5) +
	ylim(-0.27, -0.23)+
xlab(expression(alpha[2])) +
ylab(expression(Mean~Estimate~of~beta[r]))+
ggtitle("Joint Death Model")+
scale_fill_discrete(expression(beta[2]))+
theme_classic(20)+
theme(legend.position = c(0.7,0.15)),
nrow = 1
)
dev.off()

###################################################
### Plot SE as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_SE_Boxplots.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	ggplot(aes(y = SD,
	           fill = factor(trtD2),
	           x = factor(alpha2)))+
	#geom_hline(yintercept = 0, linetype=3)+
	geom_boxplot(alpha = 0.5) +
	ylim(0.077, 0.11)+
	xlab(expression(alpha[2]))+
	ylab(expression(SE~of~beta[r]))+
	ggtitle("Competing Joint Model") +
	theme_classic(20)+
	theme(legend.position = "none"),
plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	ggplot(aes(y = SD.death,
	           fill = factor(trtD2),
	           x = factor(alpha2)))+
	#geom_hline(yintercept = 0, linetype=3)+
	geom_boxplot(alpha = 0.5) +
	ylim(0.077, 0.11)+
	xlab(expression(alpha[2])) +
	ylab(expression(SE~of~beta[r]))+
	ggtitle("Joint Death Model")+
	scale_fill_discrete(expression(beta[2]))+
	theme_classic(20)+
	theme(legend.position = c(0.7,0.15)),
nrow = 1
)
dev.off()


###################################################
### Plot MSE as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/TreatmentEffect_MSE_Boxplots.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
	plottab %>%
		filter(Parameter=="Recurrent: trt") %>%
		ggplot(aes(y = mse,
		           fill = factor(trtD2),
		           x = factor(alpha2)))+
		#geom_hline(yintercept = 0, linetype=3)+
		geom_boxplot(alpha = 0.5) +
		ylim(0.006, 0.012)+
		xlab(expression(alpha[2]))+
		ylab(expression(MSE~of~beta[r]))+
		ggtitle("Competing Joint Model") +
		theme_classic(20)+
		theme(legend.position = "none"),
	plottab %>%
		filter(Parameter=="Recurrent: trt") %>%
		ggplot(aes(y = mse.death,
		           fill = factor(trtD2),
		           x = factor(alpha2)))+
		#geom_hline(yintercept = 0, linetype=3)+
		geom_boxplot(alpha = 0.5) +
		ylim(0.006, 0.012)+
		xlab(expression(alpha[2])) +
		ylab(expression(MSE~of~beta[r]))+
		ggtitle("Joint Death Model")+
		scale_fill_discrete(expression(beta[2]))+
		theme_classic(20)+
		theme(legend.position = c(0.7,0.15)),
	nrow = 1
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
geom_boxplot(alpha = 0.5) +
ylim(90, 100)+
xlab(expression(alpha[2]))+
ylab(expression(beta[r]~CI~Coverage))+
ggtitle("Competing Joint Model") +
scale_fill_discrete(expression(beta[2])) +
theme_classic(20)+
theme(legend.position = c(.8,.2)),

plottab %>%
filter(Parameter=="Recurrent: trt") %>%
ggplot(aes(y = Correct.death,
           fill = factor(trtD2),
           x = factor(alpha2)))+
geom_hline(yintercept = 95, linetype=3)+
geom_boxplot(alpha = 0.5) +
ylim(90, 100) +
xlab(expression(alpha[2])) +
ggtitle("Joint Death Model")+
theme_classic(20)+
theme(legend.position = "none",
      axis.title.y = element_blank()),
nrow = 1
)
dev.off()



###################################################
### Plot Error Rates as Function of Alpha2, Treatment Effect on Discharge
png("../simulation_results/ErrorRate_Boxplots.png",
    width = 1200, height = 600)
gridExtra::grid.arrange(
plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	ggplot(aes(y = competingError,
	           fill = factor(trtD2),
	           x = factor(alpha2)))+
	#geom_hline(yintercept = 95, linetype=3)+
	geom_boxplot(alpha = 0.5, position="dodge") +
	ylim(0, 0.3)+
	xlab(expression(alpha[2]))+
	ylab("Proportion of Models Failed to Fit")+
	ggtitle("Competing Joint Model") +
	scale_fill_discrete(expression(beta[2])) +
	theme_classic(20)+
	theme(legend.position = c(.8,.8)),
	plottab %>%
	filter(Parameter=="Recurrent: trt") %>%
	ggplot(aes(y = deathError,
	           fill = factor(trtD2),
	           x = factor(alpha2)))+
	#geom_hline(yintercept = 95, linetype=3)+
	geom_boxplot(alpha = 0.5) +
	ylim(0, 0.3) +
	xlab(expression(alpha[2])) +
	ggtitle("Joint Death Model")+
	theme_classic(20)+
	theme(legend.position = "none",
	      #axis.title.x = element_blank(),
	      axis.title.y = element_blank()),
nrow = 1
)
dev.off()

###################################################
### Error Rates for Competing Joint Model
lm(data = plottab,
   competingError ~ factor(sigma) + factor(alpha2) + factor(trtD2) +
   	factor(trtR) + factor(trtD) + factor(alpha1)) %>%
summary

lm(data = plottab,
   competingError ~ factor(sigma)) %>%
	summary



