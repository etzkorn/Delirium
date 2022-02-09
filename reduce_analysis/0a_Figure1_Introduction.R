
library(tidyverse)
df <- read_csv("../reduce_data/reduce data.csv")
head(df)

# Pivot to a long format.
# Retain only days observed in ICU
df1 <- df %>%
	mutate(letter = substr(CRF_letter_number, 0,1)) %>%
	filter(!letter %in% c("L","M","N","Q","R","T","U")) %>%
	select(died, gender, losic, number_days_survived_28days, matches(c("d.._coma","d.._delirium"))) %>%
	mutate( id = 1:n()) %>%
	mutate(d00_dead = died, `d-1_dead` = died) %>%
	select(-died) %>%
	pivot_longer(cols = c(d00_dead, `d-1_dead`, d01_coma:d28_delirium)) %>%
	separate(col = name, into = c("day", "state"), sep = "_") %>%
	pivot_wider(id_cols = c(id, day, gender, losic, number_days_survived_28days), names_from = state) %>%
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
	       tstart = ifelse(day == 0, max(day) + 0.2 ,
	       	    ifelse(day == -1, 0, day - 0.8)),
	       tstop = ifelse(tstart == 0 | death == 1 | discharge == 1, tstart + 0.2, tstart + 1),
	       state = ifelse(delirium ==1, "Delirium",
	       	   ifelse(coma == 1, "Coma", "None"))) %>%
	select(-dead) %>%
	arrange(id, tstart) %>%
	mutate(prev.state = c("None", state[-n()]),
	       next.state = ifelse(death == 1, "Death",
	       	        ifelse(discharge == 1, "Discharge",
	       	               c(state[-1],"None"))))

### Plot original data type
id.sample <-
df1 %>% group_by(id) %>% filter(sum(delirium + coma)>1) %>%
select(id) %>% unlist() %>% unique %>% sample(5)

# df1 %>%
# ungroup %>%
# filter(id %in% id.sample) %>%
# mutate(id = dense_rank(id)) %>%
# group_by(id) %>%
# filter(day != -1) %>%
# mutate(state2 = ifelse(state == "Delirium", "+",
# 	           ifelse(state == "Coma", "C",
# 	                  ifelse(day != 0, ".",
# 	                         ifelse(death == 1, "Dead", "Discharged")))),
#        day = ifelse(day == 0, max(day)+1, day)) %>%
# ggplot() +
# geom_text(aes(x = day, y = factor(id), label = state2),
#           hjust = 0,nudge_x = -.12) +
# scale_x_continuous("Day in ICU", breaks = 0:28, limits = c(0,28),
# 	       minor_breaks = NULL) +
# scale_y_discrete("Participant ID") +
# theme_bw()

# Re-Organize Data into subsequent intervals
df2 <- df1  %>%
mutate(state = ifelse(state %in% c("Coma","Delirium"), "Delirium/Coma", state),
       prev.state = ifelse(prev.state %in% c("Coma","Delirium"), "Delirium/Coma", prev.state),
       next.state = ifelse(next.state %in% c("Coma","Delirium"), "Delirium/Coma", next.state),
       interval = cumsum(state != prev.state)) %>% #filter(id == 226)
group_by(id, interval) %>%
summarise(tstart = min(tstart),
          tstop = max(tstop),
          state = state[1],
          prev.state = prev.state[1],
          next.state = next.state[n()],
          gender = gender[1]) %>%
mutate(delirium = as.numeric(next.state == "Delirium"),
       coma = as.numeric(next.state == "Coma"),
       death = as.numeric(next.state == "Death"),
       discharge = as.numeric(next.state == "Discharge")) %>%
	# Remove intervals of active delirium
filter(state != "Delirium" )

### Plot original data type
set.seed(100)
id.sample <-
	df1 %>% group_by(id) %>% filter((sum(delirium+coma, na.rm=T)>1) & losic > 5) %>%
	select(id) %>% unlist() %>% unique %>% sample(3)
id.sample <-
	df1 %>% group_by(id) %>%
	filter((sum(delirium)>1)&
	       	losic >27&
	       	!any(coma==1)) %>%
	select(id) %>% unlist() %>% unique %>% sample(2) %>% c(id.sample)
{
png("../reduce_analysis_output/Figure1_Introduction_Version2.png",height = 800, width = 800)
gridExtra::grid.arrange(
df1 %>%
	ungroup %>%
	filter(id %in% id.sample) %>%
	mutate(id = dense_rank(id)) %>%
	group_by(id) %>%
	filter(day != -1) %>%
	mutate(state2 = ifelse(death == 1, "Dead",
		           ifelse(discharge == 1, "Discharged", state)) %>% factor,
	       state2 = relevel(state2, "Delirium"),
	       day = ifelse(day == 0, max(day)+1, day)) %>%
	filter(state2 !="None") %>%
	ggplot() +
	geom_point(aes(x = day-1, y = factor(id),
		   shape = state2, fill = state2), size = 4) +
	scale_x_continuous("Day in ICU", breaks = 0:28, limits = c(0,28),
		       minor_breaks = NULL) +
	scale_y_discrete("Participant ID") +
	scale_shape_manual("Status",
		       values = c(22,23,24,25))+
	scale_fill_manual("Status",
		      values = c("#d55e00",
		                 "#f0e442",
		                 "#0072b2",
		                 "#009e73"))+
	theme_bw(25) +
	theme(panel.grid.major.y = element_blank(),
	      axis.title.x = element_blank(),
	      legend.position = c(.86,.35),
	      legend.background = element_rect(color = "grey80")),

df2 %>%
filter(id %in% id.sample & !next.state %in% c("None")) %>%
ungroup %>%
mutate(id = dense_rank(id),
       next.state = factor(next.state),
       next.state = relevel(next.state, "Delirium/Coma")) %>% #print.data.frame
ggplot() +
theme_bw(25)+
geom_point(aes(x = tstop, y = factor(id),
	   fill = next.state,
	   shape = next.state), size = 4)+
geom_segment(aes(x = tstart, xend = tstop, y = factor(id), yend = factor(id)), size = 1) +
scale_shape_manual("Event",
	       values = c(23,24,25)) +
scale_fill_manual("Event",
	      values = c("#f0e442","#0072b2", "#009e73"))+
scale_x_continuous("Day in ICU", breaks = 0:28, limits = c(0,28.5),
		       position = "top",minor_breaks = NULL) +
scale_y_discrete("Participant ID") +
theme(legend.position = c(.86,.35),
      legend.background = element_rect(color = "grey80"),
      panel.grid.major.y = element_blank()),
nrow = 2)
dev.off()
}




