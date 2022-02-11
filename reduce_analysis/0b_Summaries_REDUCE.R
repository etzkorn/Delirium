
library(tidyverse)
df <- read_csv("../reduce_data/reduce data.csv")
head(df)

df <- df %>%
mutate(letter = substr(CRF_letter_number, 0,1)) %>%
mutate(study_arm = factor(study_arm,
		  labels = c("Placebo", "Haloperidol (2mg)", "Haloperidol (1mg)"))) %>%
filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
select(died, gender, age, (delirium.coma.days>0), apache,
       losic, number_days_survived_28days,
       study_arm, delirium_number_of_days, coma_number_of_days, matches(c("d.._coma","d.._delirium"))) %>%
mutate( id = 1:n(),
        apache = ifelse(apache < 0, NA,apache),
        discharged = (losic < number_days_survived_28days) | (losic == 28 & number_days_survived_28days == 28),
        died = losic == number_days_survived_28days & number_days_survived_28days < 28,
        censored = losic > 28,
        outcome = ifelse(died, "Death", ifelse(discharged, "Discharged", "Censored")))

# count delirium/coma days
df <-
df %>%
select(id, d01_delirium:d28_delirium, d01_coma:d28_coma) %>%
pivot_longer(cols = c(d01_delirium:d28_delirium,d01_coma:d28_coma)) %>%
separate(name, into = c("d", "state")) %>%
mutate(value = ifelse(value < 0, NA, value)) %>%
pivot_wider(id_cols = c("id", "d"),names_from = "state", values_from = "value") %>%
group_by(id) %>%
summarise(delirium.coma.days = sum(as.numeric(delirium==1|coma==1), na.rm = T)) %>%
left_join(df, by = "id")

########################################################################
# Summarize by Patients
df%>%
group_by(study_arm) %>%
summarise(size = paste0("(n=",n(),")"),
          `Any Delirium (overall)` = paste0(sum(delirium.coma.days>0),
          	         " (",100*round(mean(delirium.coma.days>0),3),"%)"),
          `Length of Stay`= paste0(round(mean(pmin(losic, 28)),2),
          		 " (",round(sd(pmin(losic, 28)),2),")"),
          Died = paste0(sum(died),
          	  " (",100*round(mean(died),3),"%)"),
          Discharged = paste0(sum(discharged ),
          	        " (",100*round(mean(discharged ),3),"%)"),
          Censored = paste0(sum(censored),
          	      " (",100*round(mean(censored),3),"%)"),

          `Delirium among Discharges` = paste0(sum((delirium.coma.days>0)[discharged]),
          		" (",100*round(mean((delirium.coma.days>0)[discharged]),3),"%)"),
          `Length of Stay for Discharges`= paste0(round(mean(pmin(losic, 28)[discharged]),2),
          			    " (",round(sd(pmin(losic, 28)[discharged]),2),")"),

          `Delirium among Deaths` = paste0(sum((delirium.coma.days>0)[died]),
          			 " (",100*round(mean((delirium.coma.days>0)[died]),3),"%)"),
          `Length of Stay for Mortalities`= paste0(round(mean(pmin(losic, 28)[died]),2),
          		 " (",round(sd(pmin(losic, 28)[died]),2),")"),

          `Delirium among Censored` = paste0(sum((delirium.coma.days>0)[censored]),
          		           " (",100*round(mean((delirium.coma.days>0)[censored]),3),"%)"),) %>%
t() #%>%
knitr::kable("latex")

mean((df$delirium.coma.days>0)[df$died])
mean((df$delirium.coma.days>0)[df$discharged])

########################################################################
# Summarize by Observation Days
df%>%
group_by(study_arm) %>%
summarise(
`Total Days` = paste0("(t = ",sum(pmin(losic, 28))," days)"),
`Delirium Days, t(%)` = paste0(sum(delirium.coma.days)," (",round(100*sum(delirium.coma.days)/sum(pmin(losic, 28)),1),"%)"),

`Discharges: Total Days` = paste0("(t = ",sum(pmin(losic[discharged], 28))," days)"),
`Discharges: Total Delirium Days` = paste0(sum(delirium.coma.days[discharged]),
			       " (",round(100*sum(delirium.coma.days[discharged])/sum(pmin(losic[discharged], 28)),1),"%)"),
`Deaths: Total Days` = paste0("(t = ",sum(pmin(losic[died], 28))," days)"),
`Deaths: Total Delirium Days` = paste0(sum(delirium.coma.days[died]),
		" (",round(100*sum(delirium.coma.days[died])/sum(pmin(losic[died],28)),1),"%)")) %>%
t() #%>%
knitr::kable("latex")

sum((df$delirium.coma.days)[df$died])/sum((df$losic)[df$died])
sum((df$delirium.coma.days)[df$discharged])/sum((df$losic)[df$discharged])


########################################################################
# Plot Prevalence by Apache Score
ggplot(df,aes(x=apache, y=(delirium.coma.days>0)))+ geom_jitter()+stat_smooth()

df <- read_csv("../reduce_analysis_output/reduce data.csv")
head(df)

###########################################################################################
### Plot daily prevalence of delirium
# Pivot to a long format.
# Retain only days observed in ICU
df1 <- df %>%
	pivot_longer(cols = c(d01_coma:d28_delirium)) %>%
	separate(col = name, into = c("day", "state"), sep = "_") %>%
	pivot_wider(id_cols = c(id, day, apache, study_arm, losic, number_days_survived_28days),
		names_from = state) %>%
	mutate(day = as.numeric(gsub("d","", day)),
	       discharge = as.numeric(day == 0 & number_days_survived_28days>losic),
	       death = as.numeric(day == 0 & number_days_survived_28days<=losic)) %>%
	filter(day<=losic)

dfsum <-
df1 %>%
filter(!is.na(coma * delirium)) %>%
arrange(id, day) %>%
group_by(id) %>%
mutate(anydel = cumsum(delirium)>0) %>%
ungroup %>%
group_by(day, study_arm) %>%
summarise(prev1 = sum(delirium)/sum(1-coma),
          prev2 = sum(delirium)/1492,
          cumprev = sum(anydel)/1492)

ggplot(dfsum) +
geom_line(aes(y = prev1, x = day, group = study_arm, color = factor(study_arm)))

ggplot(dfsum) +
geom_line(aes(y = prev2, x = day, group = study_arm, color = factor(study_arm)))

ggplot(dfsum) +
geom_line(aes(y = cumprev, x = day, group = study_arm, color = factor(study_arm)))


df %>%
mutate(study_los = pmin(losic, 28) + 1) %>%
group_by(outcome,study_los) %>%
summarise(del_rate = mean((delirium.coma.days+coma_days)/study_los),
          delirium_prev = mean((delirium.coma.days>0))) %>%
ungroup %>%
filter(study_los > 0) %>%
ggplot() +
geom_point(aes(x = study_los, y = del_rate)) +
geom_point(aes(x = study_los, y = coma_rate), color = "red") +
geom_line(aes(x = study_los, y = del_rate)) +
geom_line(aes(x = study_los, y = delirium_prev), col = "darkgreen") +
geom_line(aes(x = study_los, y = coma_rate), color = "red") +
stat_smooth(aes(x = study_los, y = del_rate), se = F) +
stat_smooth(aes(x = study_los, y = coma_rate), se = F, color = "black") +
facet_wrap("outcome")


df %>%
mutate(study_los = pmin(losic, 28) + 1) %>%
group_by(outcome,study_los) %>%
summarise(delirium_prev = mean((delirium.coma.days>0))) %>%
ungroup %>%
filter(study_los > 0) %>%
ggplot() +
geom_line(aes(x = study_los, y = delirium_prev)) +
geom_point(aes(x = study_los, y = delirium_prev)) +
#stat_smooth(aes(x = study_los, y = delirium_prev), se = F, color = "black") +
facet_wrap("outcome")
