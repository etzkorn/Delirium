# This script processes the REDUCE data for the competing joint model
# where coma and delirium are treated as separate classes of recurrent
# events, coma is the event of interest.

# Output files are
# "../reduce_data/processed_data_coma.rdata"
# "../reduce_data/processed_data_coma_2mgonly.rdata"

library(tidyverse)
df <- read_csv("../reduce_data/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df1 <- df %>%
mutate(letter = substr(CRF_letter_number, 0,1)) %>%
filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
dplyr::select(
	died, apache, age, study_arm, losic,
	number_days_survived_28days,
	matches(c("d.._coma","d.._delirium"))) %>%
mutate(id = 1:n()) %>%
# We need two extra rows per person. The first will represent the
# first risk period before any delirium event. The second will represent
# the last risk period, when the patient will experience the terminal event.
# Set "day" to 0 and -1 for now.
mutate(d00_dead = died, `d-1_dead` = died) %>%
dplyr::select(-died) %>%
pivot_longer(cols = c(d00_dead, `d-1_dead`, d01_coma:d28_delirium)) %>%
separate(col = name, into = c("day", "state"), sep = "_") %>%
pivot_wider(
	id_cols = c(id, day, apache, age, study_arm, losic, number_days_survived_28days),
	names_from = state) %>%
# define discharge and death indicators based on 28 day survival and length of stay
# these indicators. Put the indicator only on "day 0".
mutate(
	day = as.numeric(gsub("d","", day)),
    discharge = as.numeric(day == 0 & number_days_survived_28days>losic),
    death = as.numeric(day == 0 & number_days_survived_28days<=losic)) %>%
# retain only days spent in ICU
filter(day<=losic)%>%

# create indicators for delirium and coma
group_by(id) %>%
mutate(delirium = ifelse(delirium == -99, NA, delirium),
       coma = ifelse(coma == -99, NA, coma),
# Delete delirium episodes with concurrent coma.
# Use LOCF to impute.
       delirium = zoo::na.locf(ifelse(day == 0 | coma == 1, 0, delirium)),
       coma = zoo::na.locf(ifelse(day == 0 ,0, coma)),
       delirium = ifelse(coma == 1, 0, delirium),
# Define start of risk period
# We convert the row where day = 0 to the row that will
# represent the terminal event, so we set tstart to the max day.
       tstart = ifelse(
       	  day == 0,
       	  max(day) + 0.01,
          ifelse(
          	  day == -1, 0, day - 0.99
          	  )
          ),
       tstop = ifelse(
       	  tstart == 0 | death == 1 | discharge == 1,
       	  tstart + 0.01,
       	  tstart + 1),
       state = ifelse(
       		coma == 1,
       		"Coma",
       		ifelse(delirium==1, "Delirium","None"))) %>%
dplyr::select(-dead) %>%
arrange(id, tstart) %>%
mutate(prev.state = c("None", state[-n()]),
       next.state = ifelse(
       	  death == 1, "Death",
       	  ifelse(discharge == 1, "Discharge",
       	  	   c(state[-1],"None")))) %>%
dplyr::select(-day)

#############################################################################################
# Handle transitions Coma >> Delirium and Delirium >> Coma
# we need to introduce a small window where the patient
# is at risk for both delirium and coma.
df2 <- df1
df2 <- ungroup(df2)
i = 1
while(i < nrow(df2)){
	if(df2$state[i] == "Coma" & df2$next.state[i] == "Delirium"){

		# this row will represent a new small at risk period
		# with state == "None"
		df2 <- df2 %>% add_row(df2[i,], .after = i)

		df2[i,"next.state"] <- "None"
		df2[i,"tstop"] <- df2[i,"tstop"] - 0.01

		df2[i+1,"tstart"] <- df2[i,"tstop"]
		df2[i+1,"state"] <- "None"
		df2[i+1,"prev.state"] <- "Coma"

		df2[i+2,"prev.state"] <- "None"

		i = i+1
	}
	if(df2$state[i] == "Delirium" & df2$next.state[i] == "Coma"){

		# this row will represent a new small at risk period
		# with state == "None"
		df2 <- df2 %>% add_row(df2[i,], .after = i)

		df2[i,"next.state"] <- "None"
		df2[i,"tstop"] <- df2[i,"tstop"] - 0.01

		df2[i+1,"tstart"] <- df2[i,"tstop"]
		df2[i+1,"state"] <- "None"
		df2[i+1,"prev.state"] <- "Delirium"

		df2[i+2,"prev.state"] <- "None"

		i = i+1
	}
	i = i+1
}

#############################################################################################
# Gather Subsequent Days with the same state into subsequent intervals
df3 <- df2  %>%
mutate(interval = cumsum(state != prev.state)) %>%
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
mutate(delirium = as.numeric(next.state == "Delirium"), # commented out for coma-only data
       coma = as.numeric(next.state == "Coma"),
       death = as.numeric(next.state == "Death"),
       discharge = as.numeric(next.state == "Discharge")) %>%

# Remove intervals of active coma, delirium
filter(state != "Coma"  & state != "Delirium") %>%
dplyr::select(id, tstart, tstop, apache, age, study_arm, delirium, coma:discharge)


save(df3, file = "../reduce_data/processed_data_coma_delirium_separate.rdata")

df3 <- df3 %>%
ungroup %>%
filter(study_arm %in% c(1,2)) %>%
mutate(treatment = as.numeric(study_arm==2),
	   id = dense_rank(id))

save(df3, file = "../reduce_data/processed_data_coma_delirium_separate_2mg_only.rdata")
