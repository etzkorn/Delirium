
library(tidyverse)
df <- read_csv("../reduce_analysis_output/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df <- df %>%
	mutate(letter = substr(CRF_letter_number, 0,1)) %>%
	filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
	select(died, gender, age, delirium_yes_no, apache, losic, number_days_survived_28days, study_arm, matches(c("d.._coma","d.._delirium"))) %>%
	mutate( id = 1:n(),
	        apache = ifelse(apache < 0, NA,apache))
df

df%>%
group_by(study_arm) %>%
summarise(n(),,
          mean(losic < number_days_survived_28days), sum(losic < number_days_survived_28days), #discharged from icu before 28 or death
          mean(losic >= number_days_survived_28days & number_days_survived_28days < 28),
          sum(losic >= number_days_survived_28days& number_days_survived_28days < 28), #died before discharge
          mean(delirium_yes_no), sum(delirium_yes_no),
          mean(gender), sum(gender),
          mean(age), sd(age),
          mean(apache, na.rm=T), sd(apache, na.rm=T),
          mean(losic), sd(losic),
          mean(number_days_survived_28days), sd(number_days_survived_28days)) %>% t()



ggplot(df,aes(x=apache, y=delirium_yes_no))+ geom_jitter()+stat_smooth()

