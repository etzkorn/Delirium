
# Friday May 20, 2022
# I have downloaded the output file "Simulation_Results_4_Fri_May_20_.rdata"
# after running fit_models.R and gather_results.R on the cluster.

# Note: At the time these models were run, there was a problem with how
# some parameters (alpha, sigma) from the initialization models were transformed.
# This has since been fixed in the local software, and the lines below that
# correct the previous mistake should be removed if models are fit using the updated software.

rm(list = ls())
library(tidyverse)
library(gt)
library(grDevices)

load(file = "../simulation_results/Simulation_Results_4_Fri_May_20_.rdata")

############################
# Error Rates
sumtab %>% ungroup %>%
group_by(alpha2, trtD2) %>%
summarise(competingError[1],
	   deathError[1],
	   dischargeError[2]) %>%
print.data.frame

######################################################################3
#### Tabulate Results (One Example Table)

sumtab <- sumtab%>%
mutate(bias = Mean - Truth, bias.death = Mean.death - Truth, bias.discharge = Mean.discharge - Truth)

sumtab %>% ungroup %>%
filter(alpha2==0, trtD2 == 0) %>% # alpha2 = 0, betaD2 = 0
mutate(order = c(4,5,1,7,8,9,10,11,12,6,2,3)) %>%
arrange(order) %>%
dplyr::select(Parameter, bias, SD:Correct, bias.death, SD.death:Correct.death, bias.discharge, SD.discharge:Correct.discharge) %>%
gt(rowname_col = "Parameter")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results (R = 5,000, n = 1,500)**")))%>%
cols_label(#Mean = html("Competing Estimate"),
           #Mean.death = html("Death Estimate"),
           #Mean.discharge = html("Discharge Estimate"),
           bias = html("Competing Bias"),
           bias.death = html("Joint (Death) Bias"),
           bias.discharge = html("Joint (Discharge) Bias"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           Correct.discharge = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
           SD.discharge = html("SE")
	) %>%
fmt_number(c(4,7,10)-2,decimals = 4) %>%
fmt_number(c(5,8,11)-2,pattern = "({x})",decimals = 4)%>%
fmt_number(1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9,12)-2,decimals = 1) %>%
fmt_missing(columns = 1:10, missing_text = "") %>%
#cols_align(columns = 2, align = c("center")) %>%
cols_align(columns = c(5,8,11)-2, align = c("left")) %>%
cols_align(columns = c(6,9,12)-2, align = c("center")) #%>%
as_latex %>% as.character %>% cat

######################################################################3
#### Tabulate Results  9 Scenarios (Across Alpha2, TrtD2)
sumtab %>% ungroup %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
dplyr::select(trtD2, alpha2, Parameter, bias:Correct.discharge) %>%
dplyr::select(-bias.discharge:-Correct.discharge) %>%
gt()%>%
#tab_stubhead(label = "Parameter")%>%
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
fmt_number(c(4,7),decimals = 4) %>%
fmt_number(c(5,8),pattern = "({x})",decimals = 4)%>%
#fmt_number(1+1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9),decimals = 1) %>%
fmt_missing(columns = (1:9), missing_text = "") %>%
cols_align(columns = 3, align = c("center")) %>%
cols_align(columns = c(5,8), align = c("left")) %>%
cols_align(columns = c(6,9), align = c("center")) %>%
as_latex() %>%as.character() %>%cat()

#############################################################################
# Plot Recurrent Event Treatment Effect Bias with Bias confidence intervals
png("../simulation_results/NineScenario_Bias.png",
    width = 1000, height = 1200)
gridExtra::grid.arrange(
sumtab %>% filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 4)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias-2*SD/sqrt(n), ymax = Truth+bias+2*SD/sqrt(n),
	      color = factor(alpha2), group = factor(alpha2)),
	  size = 1)+
	theme_bw(25)+
	theme(legend.position = "none",
	      plot.margin = margin(0,.25,.25,.5,"cm"),
	      axis.title.x = element_blank(),
	      axis.ticks.x = element_blank(),
	      axis.text.x = element_blank())+
	ylim(-0.27, -0.23)+
	scale_color_discrete(expression(alpha[d]))+
	scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
	xlab(expression(beta[d]))+
	ylab(expression(Average~Estimate~beta[r]))+
	ggtitle("Competing Joint Model (a)"),

sumtab %>% filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
	      ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2), group = factor(alpha2)),
	  size = 1)+
theme_bw(25)+
theme(legend.position = "none",
      legend.background = element_rect(color = "grey80"),
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())+
ylim(-0.27, -0.23)+
scale_color_discrete(expression(alpha[d]))+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~beta[r]))+
ggtitle("Joint, Death (b)"),

sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias-2*SD/sqrt(n),
	      ymax = Truth+bias+2*SD/sqrt(n),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
	scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank())+
	ylim(-0.35, -0.22)+
	scale_color_discrete(expression(alpha[d]))+
xlab(expression(beta[d]))+
	scale_y_continuous(expression(Average~Estimate~beta[m]),
		       breaks = c(-0.22,-0.25,-0.28, -0.31, -0.34),
		       limits = c(-0.35, -0.22))+
	ylab(expression(Average~Estimate~beta[m])),

sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
	      ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(25)+
theme(legend.position = c(0.35,0.25),
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      #title = element_blank(),
      legend.background = element_rect(color = "grey80"),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())+
scale_color_discrete(expression(alpha[d]))+
scale_y_continuous(expression(Average~Estimate~beta[m]),
	       breaks = c(-0.22,-0.25,-0.28, -0.31, -0.34),
	       limits = c(-0.35, -0.22))+
xlab(expression(beta[d])),

sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias-2*SD/sqrt(n),
	      ymax = Truth+bias+2*SD/sqrt(n),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0.1,.25,.25,1,"cm"))+
ylim(0.35, 1)+
scale_color_discrete(expression(alpha[d]))+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~alpha[m])),

sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(aes(x = trtD2+alpha2/20,
	      ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
	      ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
	      color = factor(alpha2),
	      group = factor(alpha2)),
	  size = 1)+
scale_x_continuous(breaks = c(-0.25, 0, 0.25))+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0,.25,.25,.5,"cm"),
      #title = element_blank(),
      legend.background = element_rect(color = "grey80"),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank())+
ylim(0.35, 1)+
scale_color_discrete(expression(alpha[2]))+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~alpha[m])),
nrow=3)
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
