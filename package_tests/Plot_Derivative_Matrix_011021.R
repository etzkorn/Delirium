library(tidyverse)
#####################################################################################
### Plot Inverse Matrix for Multivariate Joint Model

a <- read_lines("../package_tests/multiv_model_derivative.dat") %>%
	str_split("\\s+") %>%
	map(as.numeric) %>% map(na.omit)
length(a)
a.diag <- map(a, ~.[79:90])
a <- map(a, ~.[-79:-90])

A <- rep(list(matrix(0, 12, 12)), length(a))

for(i in 1:length(a)){
gdata::upperTriangle(A[[i]],diag = T) <- a[[i]]
}

for(i in 1:length(A)){
plot.mat <- cbind(a.diag[[i]],A[[i]])
colnames(plot.mat) <- c("0_Score","1_betaR.5", "2_etaR.5",
		"3_betaD.5", "4_etaD.5",
		"5_betaD2.5", "6_etaD2.5",
		"7_theta.5",
		"8_alpha1","9_alpha2",
		"10_trtR", "11_trtD","12_trtD2")
plot.mat <- as_tibble(plot.mat) %>%
	mutate(rows = c("1_lnbetaR.5", "2_lnetaR.5",
		    "3_lnbetaD.5", "4_lnetaD.5",
		    "5_lnbetaD2.5", "6_lnetaD2.5",
		    "7_lntheta.5",
		    "8_alpha1","9_alpha2",
		    "10_trtR", "11_trtD","12_trtD2"))
plot.mat <- pivot_longer(plot.mat, names_to = "cols", cols = `0_Score`:`12_trtD2`)
print(
	ggplot(plot.mat) +
		geom_raster(aes(x = cols, y = rows, fill  = value)) +
		scale_fill_gradient2("Hessian", low = "blue", high = "red",mid = "white", midpoint = 0) +
		geom_vline(xintercept = 1.5) +
		theme_classic()
)
readline("Hit enter to see next matrix...")
}

library(tidyverse)
#####################################################################################
### Plot Inverse Matrix for Multivariate Joint Model SPLINES

a <- read_lines("../package_tests/multiv_model_derivative.dat") %>%
	str_split("\\s+") %>%
	map(as.numeric) %>% map(na.omit)
length(a)
a.diag <- map(a, ~.[(702-35):702])
a <- map(a, ~.[-(702-35):-702])

A <- rep(list(matrix(0, 36, 36)), length(a))

for(i in 1:length(a)){
	gdata::upperTriangle(A[[i]],diag = T) <- a[[i]]
}

for(i in 1:length(A)){
	plot.mat <- cbind(a.diag[[i]],A[[i]])
	colnames(plot.mat) <- c("0_Score", paste0("sp",1:30),
				      "7_lntheta.5",
				      "8_alpha1","9_alpha2",
				      "10_trtR", "11_trtD","12_trtD2")
	plot.mat <- as_tibble(plot.mat) %>%
		mutate(rows = c(paste0("sp",1:30),
			    "7_lntheta.5",
			    "8_alpha1","9_alpha2",
			    "10_trtR", "11_trtD","12_trtD2"))
	plot.mat <- pivot_longer(plot.mat, names_to = "cols", cols = `0_Score`:`12_trtD2`)
	print(
		ggplot(plot.mat) +
			geom_raster(aes(x = cols, y = rows, fill  = value)) +
			scale_fill_gradient2("Hessian", low = "blue", high = "red",mid = "white", midpoint = 0) +
			geom_vline(xintercept = 1.5) +
			theme_classic()
	)
	readline("Hit enter to see next matrix...")
}

#####################################################################################
### Plot Derivative Matrix for Joint Model

a <- read_lines("../package_tests/joint_model_Derivative.dat") %>% str_split("\\s+") %>%
	map(as.numeric) %>% map(na.omit)
a.diag <- map(a, ~.[37:44])
a <- map(a, ~.[-37:-44])

A <- rep(list(matrix(0, 8, 8)), length(a))

for(i in 1:length(a)){
	gdata::upperTriangle(A[[i]],diag = T) <- a[[i]]
}

for(i in 1:length(A)){
	plot.mat <- cbind(a.diag[[i]],A[[i]])
	colnames(plot.mat) <- c("0_Score","1_betaR.5", "2_etaR.5", "3_betaD.5", "4_etaD.5",
			"5_theta.5",
			"6_alpha1",
			"7_trtR", "8_trtD")
	plot.mat <- as_tibble(plot.mat) %>%
		mutate(rows = c("1_betaR.5", "2_etaR.5", "3_betaD.5", "4_etaD.5",
			    "5_theta.5",
			    "6_alpha1",
			    "7_trtR", "8_trtD"))
	plot.mat <- pivot_longer(plot.mat, names_to = "cols", cols = `0_Score`:`8_trtD`)
	print(
	ggplot(plot.mat) +
	geom_raster(aes(x = cols, y = rows, fill  = value)) +
	scale_fill_gradient2("Hessian", low = "blue", high = "red",mid = "white", midpoint = 0) +
	geom_vline(xintercept = 1.5) +
		theme_classic()
	)
	readline("Hit enter to see next matrix...")
}



