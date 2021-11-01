
library(tidyverse)
df <- read_csv("../reduce_analysis_output/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df1 <- df %>%
mutate(letter = substr(CRF_letter_number, 0,1)) %>%
filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
select(died, apache, study_arm, losic, number_days_survived_28days, matches(c("d.._coma","d.._delirium"))) %>%
mutate( id = 1:n()) %>%
mutate(d00_dead = died, `d-1_dead` = died) %>%
select(-died) %>%
pivot_longer(cols = c(d00_dead, `d-1_dead`, d01_coma:d28_delirium)) %>%
separate(col = name, into = c("day", "state"), sep = "_") %>%
pivot_wider(id_cols = c(id, day, apache, study_arm, losic, number_days_survived_28days), names_from = state) %>%
mutate(day = as.numeric(gsub("d","", day)),
       discharge = as.numeric(day == 0 & number_days_survived_28days>losic),
       death = as.numeric(day == 0 & number_days_survived_28days<=losic)) %>%
filter(day<=losic)%>%
# Count days, create death and discharge event indicators.
# Delete delirium episodes with concurrent coma.
# Use LOCF to impute.
group_by(id) %>%
mutate(delirium = ifelse(delirium == -99, NA, delirium),
       coma = ifelse(coma == -99, NA, coma),
       delirium = zoo::na.locf(ifelse(day == 0 | coma == 1, 0, delirium)),
       coma = zoo::na.locf(ifelse(day == 0 ,0, coma)),
       tstart = ifelse(day == 0, max(day) + 0.01 ,
                    ifelse(day == -1, 0, day - 0.99)),
       tstop = ifelse(tstart == 0 | death == 1 | discharge == 1, tstart + 0.01, tstart + 1),
       state = ifelse(delirium ==1, "Delirium",
                      ifelse(coma == 1, "Coma", "None"))) %>%
select(-dead) %>%
arrange(id, tstart) %>%
mutate(prev.state = c("None", state[-n()]),
       next.state = ifelse(death == 1, "Death",
                           ifelse(discharge == 1, "Discharge",
                                  c(state[-1],"None")))) %>%
select(-day)

# Re-Organize Data into subsequent intervals
df2 <- df1  %>%
mutate(interval = cumsum(state != prev.state)) %>% #filter(id == 226)
group_by(id, interval) %>%
summarise(tstart = min(tstart),
          tstop = max(tstop),
          state = state[1],
          prev.state = prev.state[1],
          next.state = next.state[n()],
          study_arm = study_arm[1],
          apache=apache[1]) %>%

# Remove coma episodes
mutate(tstart = ifelse(state == "Coma",tstop - 0.01, tstart),
       prev.state = ifelse(state == "Coma", "Coma", prev.state),
       prev.state = ifelse(state == "Delirium" & prev.state == "Coma", "None", prev.state),
       state = ifelse(state == "Coma", "None", state)) %>%

# Create indicators for events
mutate(delirium = as.numeric(next.state == "Delirium"),
       coma = as.numeric(next.state == "Coma"),
       death = as.numeric(next.state == "Death"),
       discharge = as.numeric(next.state == "Discharge")) %>%

# Remove intervals of active delirium
filter(state != "Delirium" ) %>%
select(id, tstart, tstop, apache, study_arm, delirium:discharge)


save(df2, file = "../reduce_analysis_output/processed_data.rdata")

df2 <- df2 %>%
filter(study_arm %in% c(0,2)) %>%
mutate(treatment = as.numeric(study_arm==2))

save(df2, file = "../reduce_analysis_output/processed_data_2mgonly.rdata")
