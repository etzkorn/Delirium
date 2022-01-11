
rm(list = ls())
load("../simulation_results/Data_Simulation_Results80000.rdata")
head(summaries)

summaries<-summaries%>%
mutate(par = map(par, ~ as.matrix(.) %>% t() %>% as_tibble)) %>%
unnest(par)#%>%

table(summaries$theta)
table(summaries$alpha1)
table(summaries$alpha2)
table(summaries$trtR)
table(summaries$trtD)
table(summaries$trtD2)

summaries%>%
dplyr::select(betaR:trtD2)%>%
distinct
