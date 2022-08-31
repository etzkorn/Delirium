
# This script processes the REDUCE data for the competing joint model
# where coma and delirium are combined into a single class of recurrent
# event.

# Output files are
# "../reduce_data/processed_data_coma_delirium.rdata"
# "../reduce_data/processed_data_coma_delirium_2mgonly.rdata"

library(tidyverse)
df <- read_csv("../reduce_data/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df1 <- df %>%
mutate(letter = substr(CRF_letter_number, 0,1)) %>%
filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
dplyr::select(died, apache, age, study_arm, losic, number_days_survived_28days, matches(c("d.._coma","d.._delirium"))) %>%
mutate( id = 1:n()) %>%
mutate(d00_dead = died, `d-1_dead` = died) %>%
dplyr::select(-died) %>%
pivot_longer(cols = c(d00_dead, `d-1_dead`, d01_coma:d28_delirium)) %>%
separate(col = name, into = c("day", "state"), sep = "_") %>%
pivot_wider(id_cols = c(id, day, apache, age, study_arm, losic, number_days_survived_28days), names_from = state) %>%
mutate(day = as.numeric(gsub("d","", day)),
       discharge = as.numeric(day == 0 & number_days_survived_28days>losic),
       death = as.numeric(day == 0 & number_days_survived_28days<=losic)) %>%
filter(day<=losic)%>%
# Group together delirium and coma
group_by(id) %>%
mutate(delirium = ifelse(delirium == -99, NA, delirium),
       coma = ifelse(coma == -99, NA, coma),
       del = ifelse(day == 0, 0, delirium == 1 | coma==1),
# Count days, create death and discharge event indicators.
# Delete delirium episodes with concurrent coma.
# Use LOCF to impute.
       del = zoo::na.locf(del),
       tstart = ifelse(day == 0, max(day) + 0.01 ,
                    ifelse(day == -1, 0, day - 0.99)),
       tstop = ifelse(tstart == 0 | death == 1 | discharge == 1, tstart + 0.01, tstart + 1),
       state = ifelse(del ==1, "Delirium, Coma","None")) %>%
dplyr::select(-dead) %>%
arrange(id, tstart) %>%
mutate(prev.state = c("None", state[-n()]),
       next.state = ifelse(death == 1, "Death",
                           ifelse(discharge == 1, "Discharge",
                                  c(state[-1],"None")))) %>%
dplyr::select(-day)

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
          apache=apache[1],
          age = age[1]) %>%

# Create indicators for events
mutate(del = as.numeric(next.state == "Delirium, Coma"),
       death = as.numeric(next.state == "Death"),
       discharge = as.numeric(next.state == "Discharge")) %>%

# Remove intervals of active delirium
filter(state != "Delirium, Coma" ) %>%
dplyr::select(id, tstart, tstop, apache, age, study_arm, del:discharge)


save(df2, file = "../reduce_data/processed_data_coma_delirium.rdata")

df2 <- df2 %>%
filter(study_arm %in% c(1,2)) %>%
mutate(treatment = as.numeric(study_arm==2))

save(df2, file = "../reduce_data/processed_data_coma_delirium_2mgonly.rdata")
