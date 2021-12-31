
library(tidyverse)
df <- read_csv("../reduce_analysis_output/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df <- df %>%
	mutate(letter = substr(CRF_letter_number, 0,1)) %>%
	filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
	select(died, gender, age, delirium_yes_no, apache,
	       losic, number_days_survived_28days,
	       study_arm, matches(c("d.._coma","d.._delirium"))) %>%
	mutate( id = 1:n(),
	        apache = ifelse(apache < 0, NA,apache))

# Summarize Select Measures
df%>%
group_by(study_arm) %>%
summarise(size = n(),
          survived = mean(losic < number_days_survived_28days),
          N.survived = sum(losic < number_days_survived_28days), #discharged from icu before 28 or death
          died = mean(losic >= number_days_survived_28days & number_days_survived_28days < 28),
          N.died = sum(losic >= number_days_survived_28days& number_days_survived_28days < 28), #died before discharge
          anyDelirium = mean(delirium_yes_no),
          N.anyDelirium = sum(delirium_yes_no),
          mean(gender), sum(gender),
          mean(age),
          sd(age),
          mean(apache, na.rm=T), sd(apache, na.rm=T),
          mean(losic), sd(losic),
          mean(number_days_survived_28days), sd(number_days_survived_28days)) %>% t()


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

df1 %>%
filter(!is.na(coma * delirium)) %>%
group_by(day, study_arm) %>%
summarise(prev1 = sum(delirium)/sum(1-coma),
          prev2 = sum(delirium)/1492) %>%
ggplot() +
geom_line(aes(y = prev1, x = day, group = study_arm, color = factor(study_arm)))+
geom_line(aes(y = prev2, x = day, group = study_arm, color = factor(study_arm)), linetype = 2)


