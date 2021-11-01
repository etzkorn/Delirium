library(tidyverse);library(frailtypack)
data(dataMultiv)

 data <- dataMultiv
 colnames(data) <- tolower(colnames(data))
 data <- data %>%
 	filter(indicmeta != 1) %>%
 	group_by(patient) %>%
 	mutate(time0 = c(0,time1[-n()]),
 	       timegap = time1 - time0,
 	       discharge = 1-indicdeath-indicrec) %>%
 	dplyr::select(-indicmeta, -obs)
flips <- rbinom(nrow(data),1, 0.3)

sum(data$discharge); sum(data$indicdeath)
data$discharge <- ifelse(flips==1&data$indicdeath==1,1,data$discharge)
data$indicdeath <- ifelse(flips==1&data$indicdeath==1,0,data$indicdeath)
sum(data$discharge); sum(data$indicdeath)

################################################
# Weibull Calendar Times Models

file.remove("joint_model_parameters.dat")
joint0<- frailtyPenal(Surv(time0, time1,indicrec) ~ v1 + cluster(patient) + terminal(indicdeath),
	          formula.terminalEvent =  ~ v1,
	          data = data,
	          recurrentAG=T,
	          hazard="Weibull")
joint0$istop
### SUCCESS

file.remove("frailty_model_parameters.dat")
multiv0<- multivPenal(Surv(time0, time1,indicrec) ~ v1 + cluster(patient) + terminal(indicdeath)+ terminal2(discharge),
	          formula.terminalEvent =  ~ v1,
	          formula.terminalEvent2 =  ~ v1,
	          data = data,
	          recurrentAG=T,
	          hazard="Weibull")
multiv0$critCV
### FAILS

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

png("Simulation_Fit4.png",width = 1200, height = 1000)
ggplot(filter(df, n < 500)) +
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

# Check Parameter Convergence
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

nrow(df)/9

png("Simulation_Joint_Fit4.png",width = 1200, height = 1000)
ggplot(filter(df, n < 500)) +
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



################################################
# Weibull Gap Times Models

joint0<- frailtyPenal(Surv(timegap,indicmeta) ~ v1 + cluster(patient) + terminal(indicdeath),
	          formula.terminalEvent =  ~ v1,
	          data = dataMultiv,
	          recurrentAG=F,
	          hazard="Weibull")
### DOES NOT CONVERGE

joint1<- frailtyPenal(Surv(timegap,indicmeta) ~ v1 + cluster(patient) + terminal(indicdeath),
	          formula.terminalEvent =  ~ v1,
	          data = dataMultiv,
	          recurrentAG=F,
	          hazard="Splines",n.knots = 10, kappa = c(.1,.1))
### SUCCESSFUL CONVERGENCE

joint0<- frailtyPenal(Surv(time0, time1,indicmeta) ~ v1 + cluster(patient) + terminal(indicdeath),
	          formula.terminalEvent =  ~ v1,
	          data = dataMultiv,
	          recurrentAG=T,
	          hazard="Weibull")

model$loglik
model$critCV
# test
# formula = Surv(time0, time1, indicrec) ~ cluster(patient)+v1+v2+terminal(indicdeath)+terminal2(discharge)
# formula.Event2 = Surv(time0, time1,indicmeta) ~ v1+v2+v3 + cluster(patient)
# formula.terminalEvent=~v1
# formula.terminalEvent2=~v1
# hazard="Weibull"

initialize = TRUE
recurrentAG = FALSE
maxit = 350
print.times = TRUE
