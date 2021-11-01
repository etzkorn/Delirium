par <- read_file(file = "../package_tests/multiv_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1] %>%
	matrix(ncol = 14, byrow = T) %>%
	as.data.frame

colnames(df) <- c("n","LL", "1_lnbetaR.5", "2_lnetaR.5",
	      "3_lnbetaD.5", "4_lnetaD.5",
	      "5_lnbetaD2.5", "6_lnetaD2.5",
	      "7_lntheta.5",
	      "8_alpha1","9_alpha2",
	      "10_trtR", "11_trtD","12_trtD2")
df <- df %>% as_tibble %>% gather("par","val",-n)

df %>% dplyr::filter(n < 12) %>%
	ggplot() +
	geom_line(aes(x = n, y = val)) +
	geom_point(aes(x = n, y = val)) +
	facet_wrap("par", scales = "free_y") +
	theme_bw()

#############################################################################
# SPLINES
library(tidyverse)
par <- read_file(file = "../package_tests/multiv_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1] %>%
	matrix(ncol = 38, byrow = T) %>%
	as.data.frame

colnames(df) <- c("n","LL", paste0("ln_sp",1:30),
	      "7_ln_theta",
	      "8_alpha1","9_alpha2",
	      "10_trtR", "11_trtD","12_trtD2")
df %>%
as_tibble %>%
dplyr::select(n, LL, `7_ln_theta`:`12_trtD2`) %>%
gather("par","val",-n) %>%
#dplyr::filter(n < 50) %>%
ggplot() +
geom_line(aes(x = n, y = val)) +
geom_point(aes(x = n, y = val)) +
facet_wrap("par", scales = "free_y") +
theme_bw()


df %>%
as_tibble %>%
dplyr::select(n, ln_sp1:ln_sp10) %>%
gather("par","val",-n) %>%
#dplyr::filter(n < 50) %>%
ggplot() +
geom_line(aes(x = n, y = val)) +
geom_point(aes(x = n, y = val)) +
facet_wrap("par", scales = "free_y") +
theme_bw()

df %>%
as_tibble %>%
dplyr::select(n, ln_sp11:ln_sp20) %>%
gather("par","val",-n) %>%
#dplyr::filter(n < 50) %>%
ggplot() +
geom_line(aes(x = n, y = val)) +
geom_point(aes(x = n, y = val)) +
facet_wrap("par", scales = "free_y") +
theme_bw()

df %>%
as_tibble %>%
dplyr::select(n, ln_sp21:ln_sp30) %>%
gather("par","val",-n) %>%
#dplyr::filter(n < 50) %>%
ggplot() +
geom_line(aes(x = n, y = val)) +
geom_point(aes(x = n, y = val)) +
facet_wrap("par", scales = "free_y") +
theme_bw()
