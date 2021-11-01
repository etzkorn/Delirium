library(tidyverse)

# here I am just constructing an example data set in long
# format
df <- expand.grid(id = 1:20, day = 1:28) %>% 
	as_tibble %>%
	group_by(id) %>%
	mutate(discharged =  rbinom(1,28, prob = .75),
	       delirium = rbinom(28,1, .25),
	       current.state = ifelse(day > discharged, "Discharged",
	       	           ifelse(delirium==1, "ICU: Delirious",
	       	                  "ICU: Non-Delirious")) %>% as.factor) %>%
	ungroup
head(df)

# now I am transforming to wide format, so I can
# demonstrate how to elongate.
df.wide <- df %>%
	select(id, discharged, day, current.state) %>%
	spread(key = day, value = current.state) %>%
	# I am adding a discharge rank variable to use in a plot
	mutate(discharge.rank = rank(discharged,ties.method = "first"))
head(df.wide)

# transform to long format
df.long <- df.wide %>%
	gather(key = "day", value = "current.state", -id, - discharged, - discharge.rank) %>%
	mutate(day = as.integer(day))
head(df.long)

# make lasagna plot
ggplot(df.long) +
geom_tile(aes(x = day, y = id, fill = current.state)) +
theme_classic()

# sometimes it can be nice to order the ids by some feature of the data
# like day of discharge, most days delirious, etc.
# Here, the function dense_rank and rank can be really helpful

ggplot(df.long) +
	geom_tile(aes(x = day, y = discharge.rank, fill = current.state)) +
	theme_classic()


