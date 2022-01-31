
rm(list = ls())
load("../simulation_results/Simulation_Results_80003_155000.rdata")
head(results)

results <- select(results, simid, deaths:trtD2)

lm(data = results, deaths ~ factor(theta) + factor(alpha1) + factor(alpha2) +
   	 factor(trtR)+ factor(trtD)+ factor(trtD2)) %>% summary

results %>% filter(theta == 0.5, alpha2 ==0, alpha1 == 0, trtR == 0, trtD ==0)  %>%
pivot_longer(cols = deaths : events.pl) %>%
group_by(trtD2, name) %>%
summarise(m = mean(value)) %>%
pivot_wider(names_from = "name", values_from = "m")


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
