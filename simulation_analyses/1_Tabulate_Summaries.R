
rm(list = ls())
library(tidyverse)
library(gt)

load(file = "../simulation_results/Gathered_Results.rdata")

results %>%
gt(rowname_col = "Parameter",
   groupname_col = "r")%>%
tab_stubhead(label = "Parameter")%>%
tab_header(title = md(paste0("**Competing Joint Model Simulation Results(R =",
		     5000,", n = ", 1500,")**")))%>%
cols_label(Mean = html("Competing Estimate"),
           Mean.Death = html("Death Estimate"),
           Mean.Discharge = html("Discharge Estimate"),
           SD = html("SE"),
           SD.Death = html("SE"),
           SD.Discharge = html("SE")
	) %>%
	fmt_number(c(5,7,9),pattern = "({x})")%>%
	fmt_number(c(4,6,8)) %>%
	fmt_missing(columns = 1:9, missing_text = "") %>%
	cols_align(columns = c(5,6,9), align = c("left"))


