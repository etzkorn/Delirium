
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

load(file = "../simulation_results/Simulation_Results_5_Wed_Jun__1_.rdata")

############################
# Error Rates
sumtab %>% ungroup %>%
group_by(etaD, alpha2, trtD2) %>%
summarise(competingError[1],
	   deathError[1],
	   dischargeError[2]) %>%
print.data.frame

######################################################################3
#### Tabulate Results (One Example Table)

sumtab <- sumtab%>%
mutate(bias = Mean - Truth, bias.death = Mean.death - Truth, bias.discharge = Mean.discharge - Truth)

sumtab %>% ungroup %>%
filter(alpha2==0, trtD2 == 0, etaD == 16) %>%
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
filter(etaD == 16) %>%
filter(Parameter %in% c("Alpha, Terminal1", "Recurrent: trt", "Terminal1: trt")) %>%
dplyr::select(Parameter, bias, SD:Correct, bias.death, SD.death:Correct.death, bias.discharge, SD.discharge:Correct.discharge) %>%
dplyr::select(-bias.discharge:-Correct.discharge) %>%
gt()%>%
#tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R = 5000, n = 1500)**")))%>%
cols_label(bias = html("Competing Estimate"),
           bias.death = html("Death Estimate"),
           Correct = html("Correct %"),
           Correct.death = html("Correct %"),
           SD = html("SE"),
           SD.death = html("SE"),
) %>%
fmt_number(c(4,7)-2,decimals = 4) %>%
fmt_number(c(5,8)-2,pattern = "({x})",decimals = 4)%>%
#fmt_number(1+1, pattern = "Scenario {x}")%>%
fmt_number(c(6,9)-2,decimals = 1) %>%
fmt_missing(columns = (1:7), missing_text = "") %>%
cols_align(columns = 3, align = c("center")) %>%
cols_align(columns = c(5,8)-2, align = c("left")) %>%
cols_align(columns = c(6,9)-2, align = c("center")) %>%
as_latex() %>%as.character() %>%cat()

#############################################################################
# Plot Recurrent Event Treatment Effect Bias with Bias confidence intervals

sumtab <-
	sumtab %>%
	mutate(n = 5000 - 5000*competingError,
		   n.death = 5000 - 5000*deathError,
		   n.discharge = 5000 - 5000*dischargeError)

{
png("../simulation_results/NineScenario_Bias.png",
    width = 1000, height = 1200)
gridExtra::grid.arrange(

#### Treatment Effect Delirium
sumtab %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>%
filter(Parameter == "Recurrent: trt") %>%
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
ylim(-0.54, -0.46)+
	scale_color_discrete(expression(alpha[d]))+
	scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
	xlab(expression(beta[d]))+
	ylab(expression(Average~Estimate~beta[r]))+
	ggtitle("Competing Joint Model (a)"),


sumtab %>% filter(Parameter == "Recurrent: trt") %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>% ggplot() +
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
ylim(-0.54, -0.46)+
scale_color_discrete(expression(alpha[d]))+
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~beta[r]))+
ggtitle("Joint, Death (b)"),


#### Treatment Effect Mortality
sumtab %>% filter(Parameter == "Terminal1: trt") %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>%ggplot() +
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
	scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank())+
scale_color_discrete(expression(alpha[d]))+
xlab(expression(beta[d]))+
	scale_y_continuous(expression(Average~Estimate~beta[m]),
		       #breaks = c(-0.22,-0.25,-0.28, -0.31, -0.34),
		       limits = c(-0.6, -0.4))+
	ylab(expression(Average~Estimate~beta[m])),



sumtab %>% filter(Parameter == "Terminal1: trt") %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>%
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
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
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
      axis.text.y = element_blank()
	  )+
scale_color_discrete(expression(alpha[d]))+
scale_y_continuous(expression(Average~Estimate~beta[m]),
				   limits = c(-0.6, -0.4))+
xlab(expression(beta[d])),

#### Alpha Mortality
sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>%
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
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0.1,.25,.25,1,"cm"),
	  axis.ticks.x = element_blank())+
ylim(0.75, 1.5)+
scale_color_discrete(expression(alpha[d]))+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~alpha[m])),


sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
filter(etaD == 16)%>%
mutate(n = 5000) %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/20, y = Truth+bias.death, color = factor(alpha2)),
           size = 4) +
#geom_line(aes(x = trtD2+alpha2/20,, y = bias.death, color = factor(alpha2), group = factor(alpha2))) +
geom_errorbar(
	aes(x = trtD2+alpha2/20,
		ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
		ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
		color = factor(alpha2),
		group = factor(alpha2)),
	size = 1)+
scale_x_continuous(
	expression(beta[d]),
	breaks = c(-0.5, 0, 0.5),
	minor_breaks = NULL
	)+
theme_bw(25)+
theme(
	legend.position = "none",
    plot.margin = margin(0,.25,.25,.5,"cm"),
    legend.background = element_rect(color = "grey80"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
	axis.ticks.x = element_blank()
	)+
ylim(0.75, 1.5)+
scale_color_discrete(expression(alpha[2]))+
ylab(expression(Average~Estimate~alpha[m])),
nrow=3)
dev.off()
}

#############################################################################
# Plot %Correct by alphaD, betaD


{
	png("../simulation_results/NineScenario_Correctness.png",
		width = 1000, height = 1200)
	gridExtra::grid.arrange(

		#### Treatment Effect Delirium
		sumtab %>%
			filter(etaD == 16)%>%
			filter(Parameter == "Recurrent: trt") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 4)+
			geom_point(
				aes(x = trtD2+alpha2/20,
					y = Correct,
					color = factor(alpha2)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.title.x = element_blank(),
				  axis.ticks.x = element_blank(),
				  axis.text.x = element_blank())+
			scale_y_continuous(
				expression(Correct~CIs~beta[r]),
				limits = c(88, 100), breaks = c(90, 95, 100))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			ggtitle("Competing Joint Model (a)"),


		sumtab %>%
			filter(Parameter == "Recurrent: trt") %>%
			filter(etaD == 16)%>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/20, y = Correct.death, color = factor(alpha2)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
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
			scale_y_continuous(limits = c(88, 100), breaks = c(90, 95, 100))+
			scale_color_discrete(expression(alpha[d]))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			ggtitle("Joint, Death (b)"),


	#### Treatment Effect Mortality
		sumtab %>% filter(Parameter == "Terminal1: trt") %>%
			filter(etaD == 16)%>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/20,
					y = Correct,
					color = factor(alpha2)),
					   size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1) +
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.title.x = element_blank(),
				  axis.ticks.x = element_blank(),
				  axis.text.x = element_blank())+
			scale_color_discrete(expression(alpha[d]))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			scale_y_continuous(
				expression(Correct~CIs~beta[m]),
				limits = c(88, 100), breaks = c(90, 95, 100)),



		sumtab %>% filter(Parameter == "Terminal1: trt") %>%
			filter(etaD == 16)%>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/20,
					y = Correct.death,
					color = factor(alpha2)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
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
				  axis.text.y = element_blank()
			)+
			scale_color_discrete(
				expression(alpha[d]))+
			scale_y_continuous(
				limits = c(88,100), breaks = c(90, 95, 100))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL),

	#### Alpha Mortality
		sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
			filter(etaD == 16)%>%
			ggplot() +
			geom_hline(
				aes(yintercept = 95),
				linetype = 2
				)+
			geom_point(
				aes(
					x = trtD2+alpha2/20,
					y = Correct,
					color = factor(alpha2)
					),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.ticks.x = element_blank())+
			scale_color_discrete(expression(alpha[d]))+
			scale_x_continuous(
				expression(beta[d]),
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			scale_y_continuous(
				expression(Correct~CIs~alpha[m]),
				limits = c(88,100), breaks = c(90, 95, 100)),


		sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
			filter(etaD == 16)%>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/20,
					y = Correct.death,
					color = factor(alpha2)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/20,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(
				legend.position = "none",
				plot.margin = margin(0,.25,.25,.5,"cm"),
				legend.background = element_rect(color = "grey80"),
				axis.title.y = element_blank(),
				axis.ticks.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.x = element_blank()
			)+
			scale_x_continuous(
				expression(beta[d]),
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL
			)+
			scale_color_discrete(expression(alpha[2]))+
			scale_y_continuous(
				limits = c(88,100), breaks = c(90, 95, 100)) +
		annotate("text", x = -.5, y = 89, hjust = 0,
				 size = 9, color = "#F8766D",
				 label = "`*`~Correctness~`<`~70~when~alpha[d]~'='~-1.",
				 parse = T),
		nrow=3)
	dev.off()
}

#############################################################################
# Plot Bias as function of death prevalence (etaM)

{
png("../simulation_results/TwentySevenScenario_Bias.png",
    width = 1000, height = 1200)
gridExtra::grid.arrange(

############################################
#### Treatment Effect Delirium
sumtab %>%
mutate(n = 5000) %>%
filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 4)+
geom_point(
	aes(x = trtD2+alpha2/8 + (etaD-29)/400,
		y = Truth+bias,
		color = factor(alpha2),
		shape = factor(etaD)),
           size = 4) +
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias-2*SD/sqrt(n),
#		ymax = Truth+bias+2*SD/sqrt(n),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1)+
theme_bw(25)+
theme(legend.position = "none",
	      plot.margin = margin(0,.25,.25,.5,"cm"),
	      axis.title.x = element_blank(),
	      axis.ticks.x = element_blank(),
	      axis.text.x = element_blank())+
ylim(-0.54, -0.46)+
scale_color_discrete(expression(alpha[d]))+
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~beta[r]))+
ggtitle("Competing Joint Model (a)"),


sumtab %>% filter(Parameter == "Recurrent: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(
	aes(
		x = trtD2+alpha2/8 + (etaD-29)/400,
		y = Truth+bias.death,
		color = factor(alpha2),
		shape = factor(etaD)),
    size = 4
	) +
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
#		ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1)+
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
ylim(-0.54, -0.46)+
scale_color_discrete(expression(alpha[d]))+
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~beta[r]))+
ggtitle("Joint, Death (b)"),

############################################
#### Treatment Effect Mortality
sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(
	aes(
		x = trtD2+alpha2/8 + (etaD-29)/400,
		y = Truth+bias,
		color = factor(alpha2),
		shape = factor(etaD)),
	size = 4)+
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias-2*SD/sqrt(n),
#		ymax = Truth+bias+2*SD/sqrt(n),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1) +
theme_bw(25)+
theme(legend.position = c(.5, .8),
	  #legend.justification = 1,
	  legend.box.just = "center",
	  legend.background = element_rect(color = "grey80"),
	  legend.spacing = unit(0,"cm"),
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank())+
scale_shape_discrete(
	expression(a[m]),
	guide = guide_legend(
      direction = "horizontal", label.hjust = 0))+
scale_color_discrete(
	expression(alpha[d]),
	guide = guide_legend(
      direction = "horizontal", label.hjust = 0))+
scale_x_continuous(
	expression(beta[d]),
	breaks = c(-0.5, 0, 0.5),
	minor_breaks = NULL)+
scale_y_continuous(
	expression(Average~Estimate~beta[m]),
	limits = c(-0.6, -0.4)),



sumtab %>% filter(Parameter == "Terminal1: trt") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(aes(x = trtD2+alpha2/8 + (etaD-29)/400,
			   y = Truth+bias.death, color = factor(alpha2),shape = factor(etaD)
			   ),
           size = 4) +
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
#		ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1)+
scale_x_continuous(breaks = c(-0.5, 0, 0.5),minor_breaks = NULL)+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0,.25,.25,.5,"cm"),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      #title = element_blank(),
      legend.background = element_rect(color = "grey80"),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
	  )+
scale_color_discrete(expression(alpha[d]))+
scale_y_continuous(expression(Average~Estimate~beta[m]),
				   limits = c(-0.6, -0.4))+
xlab(expression(beta[d])),

############################################
#### Alpha Mortality
sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
#filter(etaD == 16)%>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(
	aes(
		x = trtD2+alpha2/8 + (etaD-29)/400 ,
		y = Truth+bias,
		color = factor(alpha2),
		shape = factor(etaD)
		),
    size = 4) +
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias-2*SD/sqrt(n),
#		ymax = Truth+bias+2*SD/sqrt(n),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1)+
theme_bw(25)+
theme(legend.position = "none",
      plot.margin = margin(0.1,.25,.25,1,"cm"),
	  axis.ticks.x = element_blank())+
scale_x_continuous(
	expression(beta[d]),
	breaks = c(-0.5, 0, 0.5),
	minor_breaks = NULL)+
ylim(0.65, 2)+
xlab(expression(beta[d]))+
ylab(expression(Average~Estimate~alpha[m])),


sumtab %>%
filter(Parameter == "Alpha, Terminal1") %>%
ggplot() +
geom_hline(aes(yintercept = Truth), linetype = 2)+
geom_point(
	aes(
		x = trtD2+alpha2/8 + (etaD-29)/400,
		y = Truth+bias.death,
		color = factor(alpha2),
		shape = factor(etaD)
		),
	size = 4)+
#geom_errorbar(
#	aes(
#		x = trtD2+alpha2/8 + (etaD-29)/400,
#		ymin = Truth+bias.death-2*SD.death/sqrt(n.death),
#		ymax = Truth+bias.death+2*SD.death/sqrt(n.death),
#		color = factor(alpha2),
#		group = factor(alpha2)),
#	  size = 1)+
theme_bw(25)+
theme(
	legend.position = "none",
    plot.margin = margin(0,.25,.25,.5,"cm"),
    legend.background = element_rect(color = "grey80"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
	axis.ticks.x = element_blank()
	)+
scale_x_continuous(
	expression(beta[d]),
	breaks = c(-0.5, 0, 0.5),
	minor_breaks = NULL
	)+
ylim(0.65, 2)+
ylab(expression(Average~Estimate~alpha[m])),
nrow=3)
dev.off()
}

#############################################################################
# Plot %Correct by alphaD, betaD, etaM


{
	png("../simulation_results/TwentysevenScenario_Correctness.png",
		width = 1000, height = 1200)
	gridExtra::grid.arrange(

		#### Treatment Effect Delirium
		sumtab %>%
			filter(Parameter == "Recurrent: trt") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 4)+
			geom_point(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct,
					color = factor(alpha2),
					shape = factor(etaD)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.title.x = element_blank(),
				  axis.ticks.x = element_blank(),
				  axis.text.x = element_blank())+
			scale_y_continuous(
				expression(Correct~CIs~beta[r]),
				limits = c(88, 100), breaks = c(90, 95, 100))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			ggtitle("Competing Joint Model (a)"),


		sumtab %>%
			filter(Parameter == "Recurrent: trt") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(
					x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct.death,
					color = factor(alpha2),
					shape = factor(etaD)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
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
			scale_y_continuous(limits = c(88, 100), breaks = c(90, 95, 100))+
			scale_color_discrete(expression(alpha[d]))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			ggtitle("Joint, Death (b)"),


	#### Treatment Effect Mortality
		sumtab %>% filter(Parameter == "Terminal1: trt") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct,
					color = factor(alpha2),
					shape = factor(etaD)),
					   size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
					size = 1) +
			theme_bw(25)+
			theme(legend.position = c(.5, .3),
				  #legend.justification = 1,
				  legend.box.just = "center",
				  legend.background = element_rect(color = "grey80"),
				  legend.spacing = unit(0,"cm"),
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.title.x = element_blank(),
				  axis.ticks.x = element_blank(),
				  axis.text.x = element_blank())+
			scale_color_discrete(expression(alpha[d]),
				guide = guide_legend(
					direction = "horizontal"))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			scale_y_continuous(
				expression(Correct~CIs~beta[m]),
				limits = c(88, 100), breaks = c(90, 95, 100))+
			scale_shape_discrete(
				expression(a[m]),
				guide = guide_legend(
					direction = "horizontal")),



		sumtab %>% filter(Parameter == "Terminal1: trt") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct.death,
					color = factor(alpha2),
					shape = factor(etaD)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.title.x = element_blank(),
				  axis.ticks.x = element_blank(),
				  axis.text.x = element_blank(),
				  #title = element_blank(),
				  legend.background = element_rect(color = "grey80"),
				  axis.title.y = element_blank(),
				  axis.ticks.y = element_blank(),
				  axis.text.y = element_blank()
			)+
			scale_color_discrete(
				expression(alpha[d]))+
			scale_y_continuous(
				limits = c(88,100),
				breaks = c(90, 95, 100))+
			scale_x_continuous(
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL),

	#### Alpha Mortality
		sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
			ggplot() +
			geom_hline(
				aes(yintercept = 95),
				linetype = 2
				)+
			geom_point(
				aes(
					x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct,
					color = factor(alpha2),
					shape = factor(etaD)
					),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					ymin = Correct - 1.96*sqrt(Correct * (100-Correct)/n),
					ymax = Correct + 1.96*sqrt(Correct * (100-Correct)/n),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(legend.position = "none",
				  plot.margin = margin(0,.25,.25,.5,"cm"),
				  axis.ticks.x = element_blank())+
			scale_color_discrete(expression(alpha[d]))+
			scale_x_continuous(
				expression(beta[d]),
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL)+
			scale_y_continuous(
				expression(Correct~CIs~alpha[m]),
				limits = c(88,100), breaks = c(90, 95, 100)),


		sumtab %>% filter(Parameter == "Alpha, Terminal1") %>%
			ggplot() +
			geom_hline(aes(yintercept = 95), linetype = 2)+
			geom_point(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					y = Correct.death,
					color = factor(alpha2),
					shape = factor(etaD)),
				size = 4) +
			geom_errorbar(
				aes(x = trtD2+alpha2/8 + (etaD-29)/400 ,
					ymin = Correct.death - 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					ymax = Correct.death + 1.96*sqrt(Correct.death * (100-Correct.death)/n.death),
					color = factor(alpha2),
					group = factor(alpha2)),
				size = 1)+
			theme_bw(25)+
			theme(
				legend.position = "none",
				plot.margin = margin(0,.25,.25,.5,"cm"),
				legend.background = element_rect(color = "grey80"),
				axis.title.y = element_blank(),
				axis.ticks.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.x = element_blank()
			)+
			scale_x_continuous(
				expression(beta[d]),
				breaks = c(-0.5, 0, 0.5),
				minor_breaks = NULL
			)+
			scale_color_discrete(expression(alpha[2]))+
			scale_y_continuous(
				limits = c(88,100), breaks = c(90, 95, 100)) +
		annotate("text", x = -.5, y = 89, hjust = 0,
				 size = 9, color = "#F8766D",
				 label = "`*`~Correctness~`<`~70~when~alpha[d]~'='~-1.",
				 parse = T),
		nrow=3)
	dev.off()
}

