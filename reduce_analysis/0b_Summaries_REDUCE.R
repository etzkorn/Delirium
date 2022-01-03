
library(tidyverse)
df <- read_csv("../reduce_data/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df <- df %>%
	mutate(letter = substr(CRF_letter_number, 0,1)) %>%
	mutate(study_arm = factor(study_arm,
			  labels = c("Placebo", "Haloperidol (2mg)", "Haloperidol (1mg)"))) %>%
	filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
	select(died, gender, age, delirium_yes_no, apache,
	       losic, number_days_survived_28days,
	       study_arm, matches(c("d.._coma","d.._delirium"))) %>%
	mutate( id = 1:n(),
	        apache = ifelse(apache < 0, NA,apache))

########################################################################
# Summarize Select Measures
df%>%
group_by(study_arm) %>%
summarise(size = paste0("(n=",n(),")"),
          `Any Delirium (verall)` = paste0(sum(delirium_yes_no),
          	         " (",100*round(mean(delirium_yes_no),3),"%)"),
          `Length of Stay`= paste0(round(mean(losic),2),
          		 " (",round(sd(losic),2),")"),
          Discharged = paste0(sum(losic < number_days_survived_28days),
          	        " (",100*round(mean(losic < number_days_survived_28days),3),"%)"),
          `Delirium among Discharges` = paste0(sum(delirium_yes_no[losic < number_days_survived_28days]),
          		" (",100*round(mean(delirium_yes_no[losic < number_days_survived_28days]),3),"%)"),
          `Length of Stay for Discharges`= paste0(round(mean(losic[losic < number_days_survived_28days]),2),
          			    " (",round(sd(losic[losic < number_days_survived_28days]),2),")"),
          Died = paste0(sum(losic >= number_days_survived_28days& number_days_survived_28days < 28),
          	  " (",100*round(mean(losic >= number_days_survived_28days & number_days_survived_28days < 28),3),"%)"),
          `Delirium among Deaths` = paste0(sum(delirium_yes_no[losic >= number_days_survived_28days& number_days_survived_28days < 28]),
          			 " (",100*round(mean(delirium_yes_no[losic >= number_days_survived_28days& number_days_survived_28days < 28]),3),"%)"),
          `Length of Stay for Mortalities`= paste0(round(mean(losic[losic >= number_days_survived_28days& number_days_survived_28days < 28]),2),
          		 " (",round(sd(losic[losic >= number_days_survived_28days& number_days_survived_28days < 28]),2),")")) %>%
t() %>%
knitr::kable("latex")


# Plot Prevalence by Apache Score
ggplot(df,aes(x=apache, y=delirium_yes_no))+ geom_jitter()+stat_smooth()

library(tidyverse)
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


