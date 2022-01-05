
rm(list = ls())
library(tidyverse)

load("../reduce_data/Simulation_Results.rdata")

table(competingError)
table(deathError)
table(dischargeError)




results <-
tibble(
	data = 1:5000,
	est = map(competingJoint, ~.$Estimate),
	error = factor(competingError,
		   labels = c("None","Maxit","Calculation"),levels = c(1,2,4))) %>%
	unnest(c(est))

#truth =
#	tibble(Parameter = mod.joint[[1]]$summary.table$Parameter,
#	       truth = par00)

results2 <-
	tibble(data = 1:5000,
	       par1 = map(deathJoint, ~.$Parameter),
	       est1 = map(deathJoint, ~.$Estimate),
	       par2 = map(dischargeJoint, ~.$Parameter),
	       est2 = map(dischargeJoint, ~.$Estimate)) %>%
	unnest(c(par1, est1, par2, est2))

tab_start <-
	results %>%mutate(Truth = rep(truth$truth,nsim)) %>%
	group_by(Parameter) %>%
	summarise(Truth = Truth[1],
	          Mean = mean(Estimate[!anyerror], trim = 0.005),
	          SD = mean((Estimate[!anyerror]-Mean)^2, na.rm=T, trim = 0.005)^.5,
	          Est.SD = mean(Estimate.SE[!anyerror], na.rm=T, trim = 0.005), .groups = "drop"
	) %>%
	pivot_longer(cols = Mean:Est.SD) %>%
	mutate(value = round(value, 3)) %>%
	pivot_wider(names_from = c(name), values_from = value)

gt(tab_start, rowname_col = "Parameter")%>%
	tab_stubhead(label = "Parameter")%>%
	tab_header(
		title = md(paste0("Competing Joint Model Simulation Results(R =",length(par00),", n = ", length(unique(data0[[1]]$id)),")")))%>%
	cols_label(
		Truth = html("True Value"),
		Mean = html("Avg. Estimate"),
		SD = html("SE"),
		Est.SD = html("Avg. Est. SE")
	)

truth =
	tibble(Parameter = mod.joint[[1]]$summary.table$Parameter,
	       truth = par00)

results2 <-
	tibble(data = rep(1:(nsim), 2), model = rep(1:2, each = nsim),
	       Parameter = c(map(mod.joint, ~.$initialization$summary.table1$Parameter),
	       	  map(mod.joint, ~.$initialization$summary.table2$Parameter)),
	       Estimate = c(map(mod.joint, ~.$initialization$summary.table1$Estimate),
	       	 map(mod.joint, ~.$initialization$summary.table2$Estimate)),
	       Error = c(map(mod.joint, ~.$initialization$joint1$istop),
	                 map(mod.joint, ~.$initialization$joint1$istop))) %>%
	unnest(c(Parameter, Estimate, Error)) %>%
	mutate(Truth = c(rep(par00[c(1:4,7:8,10:11)], nsim), rep(par00[c(1:2,5:7,9:10,12)], nsim)),
	       Error = Error!=1) %>%
	group_by(model, Parameter)  %>%
	summarise(Truth = Truth[1],
	          Mean = mean(Estimate[!Error]),
	          SD = mean((Estimate[!Error]-Mean)^2, na.rm=T)^.5, .groups = "drop"
	) %>%
	pivot_wider(id_cols = Parameter, values_from = c(Truth, Mean, SD), names_from = model)

tab_start %>%
	left_join(results2, by = "Parameter") %>%
	select(Parameter, Truth, Mean, SD, Mean_1, SD_1, Mean_2, SD_2) %>%
	gt(rowname_col = "Parameter")%>%
	tab_stubhead(label = "Parameter")%>%
	tab_header(
		title = md(paste0("**Competing Joint Model Simulation Results(R =",length(par00),", n = ", length(unique(data0[[1]]$id)),")**")))%>%
	cols_label(
		Mean = html("Competing Estimate"),
		Mean_1 = html("Death Estimate"),
		Mean_2 = html("Discharge Estimate"),
		SD = html("SE"),
		SD_1 = html("SE"),
		SD_2 = html("SE")
	) %>%
	fmt_number(c(4,6,8),pattern = "({x})")%>%
	fmt_number(c(3,5,7)) %>%
	fmt_missing(columns = 1:8, missing_text = "") %>%
	cols_align(columns = c(4,6,8), align = c("left"))


