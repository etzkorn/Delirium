library(frailtypack);library(tidyverse);library(survival);library(purrr)
source("R/simulate_competing_data.R")
source("../../delirium/R/random_weibull.R")

######################################################
############## Example 1
set.seed(101010)
data0 <- simulate.competing.data(n = 100, 
		         par0 = c(betaR = 1, etaR = .1,
		                  betaD = 1, etaD = 1,
		                  betaD2 = 1, etaD2 = 1,
		                  theta = .1,
		                  alpha1 = 1, alpha2 = 1,
		                  trtR = 1, trtD = 0, trtD2 = 0)) 
file.remove("frailty_model_parameters.dat")
model0 <-
multivPenal(formula = Surv(t0, t, event) ~ trt + cluster(id) +
		terminal(terminal1) + terminal2(terminal2),
	formula.terminalEvent = ~trt,
	formula.terminalEvent2 = ~trt,
	data = data0,
	initialize = T,
	hazard = "Weibull", 
	maxit = 2000)
model0$critCV[1:2]

# Check Parameter Convergence
par <- read_file(file = "frailty_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1:-14] %>%
matrix(ncol = 13, byrow = T) %>%
as.data.frame 

colnames(df) <- c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	      "betaD2.5", "etaD2.5", "theta.5", 
	      "alpha1", "alpha2",
	      "trtR", "trtD", "trtD2")
df <- 
df %>%
as_tibble %>%
mutate(n = 1:n()) %>%
gather("par","val",-n)

nrow(df)/13

png("Simulation_Fit1.png",width = 1200, height = 1000)
ggplot(df) +
geom_line(aes(x = n, y = val)) +
facet_wrap("par", scales = "free_y") +
geom_hline(data = tibble(par = c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
		       "betaD2.5", "etaD2.5", "theta.5", 
		       "alpha1", "alpha2",
		       "trtR", "trtD", "trtD2"),
	           val = c(6000,1,.1^.5,1,1,1,1,.1^.5,1,1,1,0,0)),
          aes(yintercept = val), color = "red") +
theme_bw(30)
dev.off()

######################################################
############## Example 2
set.seed(202020)
data0 <- simulate.competing.data(n = 100) 
file.remove("frailty_model_parameters.dat")
model0 <-
	multivPenal(formula = Surv(t0, t, event) ~ trt + cluster(id) +
			terminal(terminal1) + terminal2(terminal2),
		formula.terminalEvent = ~trt,
		formula.terminalEvent2 = ~trt,
		data = data0,
		initialize = T,hazard = "Weibull", maxit = 350)
model0$critCV[1:2]
# Check Parameter Convergence

par <- read_file(file = "frailty_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1:-14] %>%
	matrix(ncol = 13, byrow = T) %>%
	as.data.frame 

colnames(df) <- c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	      "betaD2.5", "etaD2.5", "theta.5", 
	      "alpha1", "alpha2",
	      "trtR", "trtD", "trtD2")
df <- 
	df %>%
	as_tibble %>%
	mutate(n = 1:n()) %>%
	gather("par","val",-n)

nrow(df)/13

png("Simulation_Fit2.png",width = 1200, height = 1000)
ggplot(df) +
	geom_line(aes(x = n, y = val)) +
	facet_wrap("par", scales = "free_y") +
	geom_hline(data = tibble(par = c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
			         "betaD2.5", "etaD2.5", "theta.5", 
			         "alpha1", "alpha2",
			         "trtR", "trtD", "trtD2"),
			 val = c(6000,1,.1^.5,1,1,1,1,.1^.5,1,1,1,0,0)),
			 aes(yintercept = val), color = "red") +
	theme_bw(30)
dev.off()

######################################################
############## Example 3
set.seed(303030)
data0 <- simulate.competing.data(n = 1200)
data0 %>% group_by(id) %>% summarise(n = n() - 1) %>% select(n) %>%unlist %>% hist
data0 %>% filter(terminal1==1) %>% select(t) %>%unlist %>% hist
data0 %>% filter(terminal2==1) %>% select(t) %>%unlist %>% hist

file.remove("frailty_model_parameters.dat")
model0 <-
	multivPenal(formula = Surv(t0, t, event) ~ trt + cluster(id) +
			terminal(terminal1) + terminal2(terminal2),
		formula.terminalEvent = ~trt,
		formula.terminalEvent2 = ~trt,
		data = data0,
		initialize = T,
		hazard = "Weibull", 
		maxit = 350, 
		recurrentAG = F)
model0$critCV[1:2]

# Check Parameter Convergence
par <- read_file(file = "frailty_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1:-14] %>%
	matrix(ncol = 13, byrow = T) %>%
	as.data.frame 

colnames(df) <- c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	      "betaD2.5", "etaD2.5", "theta.5", 
	      "alpha1", "alpha2",
	      "trtR", "trtD", "trtD2")
df <- 
	df %>%
	as_tibble %>%
	mutate(n = 1:n()) %>%
	gather("par","val",-n)

nrow(df)/13

png("Simulation_Fit3.png",width = 1200, height = 1000)
ggplot(filter(df, n < 100)) +
	geom_line(aes(x = n, y = val)) +
	facet_wrap("par", scales = "free_y") +
	#geom_hline(data = tibble(par = c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	#		         "betaD2.5", "etaD2.5", "theta.5", 
	#		         "alpha1", "alpha2",
	#		         "trtR", "trtD", "trtD2"),
	#		 val = c(6000,1,.1^.5,1,1,1,1,.1^.5,1,1,1,0,0)),
	#		 aes(yintercept = val), color = "red") +
	theme_bw(30)
dev.off()

#############################################################
# Check Parameter Convergence for joint model
file.remove("joint_model_parameters.dat")
joint0<- frailtyPenal(Surv(g,event) ~ trt + cluster(id) + terminal(terminal1),
	          formula.terminalEvent =  ~ trt ,
	          data = data0,
	          recurrentAG=F,
	          hazard="Weibull",
	          init.B = c(0,0),
	          init.Theta = .05, 
	          init.Alpha = -.1)
file.remove("joint_model_parameters.dat")
joint0<- frailtyPenal(Surv(t0, t,event) ~ trt + cluster(id) + terminal(terminal1),
	          formula.terminalEvent =  ~ trt ,
	          data = data0,
	          recurrentAG=T,
	          hazard="Weibull",
	          init.B = c(0,0),
	          init.Theta = .05, 
	          init.Alpha = -.1)
par <- read_file(file = "joint_model_parameters.dat") %>%
	strsplit("\\s+") %>% unlist %>% as.numeric
df <- as.numeric(par)[-1] %>%
	matrix(ncol = 9, byrow = T) %>%
	as.data.frame 

colnames(df) <- c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	      "theta.5", "alpha1", "trtR", "trtD")
df <- 
	df %>%
	as_tibble %>%
	mutate(n = 1:n()) %>%
	gather("par","val",-n)

png("../Simulation_Fit3_Joint.png",width = 1200, height = 1000)
df %>% filter( n < 1000, n > 500) %>%
ggplot() +
	geom_line(aes(x = n, y = val)) +
	facet_wrap("par", scales = "free_y") +
	#geom_hline(data = tibble(par = c("LL", "betaR.5", "etaR.5", "betaD.5", "etaD.5", 
	#		         "theta.5", "alpha1","trtR", "trtD"),
	#		 val = c(6000,.1^.5,20^.5,1,20^.5,.05^.5,-.1,1,0)),
	#		 aes(yintercept = val), color = "red") +
	theme_bw(30)
dev.off()

######################################################
# Many Simulations (save for later)

  data <- map(rep(100, 10), simulate.competing.data)
models <- list()

for(i in 1:10){
models[[i]] <- multivPenal(formula = Surv(t0, t, event) ~ trt + cluster(id) +
		          	terminal(terminal1) + terminal2(terminal2),
		          formula.terminalEvent = ~trt,
		          formula.terminalEvent2 = ~trt,
		          data = data[[i]],
		          initialize = T,hazard = "Weibull")
#save(data,models,file = "../../Simulations_081620.rdata")
cat(i," ,")
}

load(file = "../../Simulations_081620.rdata")
length(models)
models[[12]]

data[[13]]
table(table(data[[13]]$id))

map(models, ~.$critCV) %>% do.call(what = "rbind")
