
#########################
## Date: 2021/01/13
##  simulate icu discharge, time to death seperately, from weibull
##  simulate delirium and coma simultaneously from weibull distribution
##  frailty parameter, weilbull parameter are from fitting frailty model to REDUCE trial
#########################


library(rstudioapi)
library(survival)
library(ggplot2);library(gridExtra)
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
source("Function_V10.R")

out.path <- paste0("output/V10")
dir.create(out.path,showWarnings = FALSE)
dir.create(paste0(out.path,"/testA"))

#### A.- format data and fit frailty model - ####
data <- read.csv("Reduce_example.csv")[,-1]
100*table(data$d1)/nrow(data) # 51%   2.65%
100*table(data$d2)/nrow(data) # coma 31.89% delirium 10.49% 
100*table(data$d3)/nrow(data) # 21.46%, 14%

100*table(data$death)/nrow(data)

coma <- find.x(data,cov.vec=paste0("d",1:28),perfer=1)
delirium <- find.x(data,cov.vec=paste0("d",1:28),perfer=2)

## overall % of event
100*table(coma)/nrow(data);100*table(delirium)/nrow(data)
# 58.57%; 34.2% 

# fit coxph in R
# table(!is.na(data$number_days_survived),data$died)
# data$died[which(data$died==-96)] <- NA
data$CRF_letter_number <- data$losic.1 <- data$loshos <- data$died <- NULL
# data$died <- ifelse(!data$died %in% c(1,0),NA,data$died)
# colnames(data)[4] <- "number_days_survived"
# for generalizating purpose, change day with coma and delirium to delirium 
for(i in 1:28){
  data[,paste0("d",i)] <- ifelse(data[,paste0("d",i)]==3,2,data[,paste0("d",i)])
}
data = data[data$study_arm !="1-mg Haloperidol" & data$number_days_survived_28days>0,] # 

# fit coxph in R
# table(!is.na(data$number_days_survived),data$died)
# data$died[which(data$died==-96)] <- NA
fit <- coxph(Surv(data$number_days_survived,data$death)~1)
bhest <- basehaz(fit)


# reformat data and fit onset
# term event= 1- icu discharge; 2 - death
# event = 1 - coma; 2 - delirium
frailty.dat <- data_format(data,censor.day=28,death.after.icu = 1)
frailty.dat2 <- data_format(data,censor.day=28,death.after.icu = 0)
write.csv(frailty.dat,"frailty_dat.csv")
write.csv(frailty.dat2,"frailty_dat2.csv")

# onset <- fit.model.onset(frailty.dat)

frailty.duration <- data_duration_format(frailty.dat2)
# duration <- fit.model.duration(frailty.duration)

# write.csv(onset,"model_result_onset.csv",row.names = FALSE)
# write.csv(duration,"model_result_duration.csv",row.names = FALSE)


#### B.- simulate data - ####
onset <- read.csv(paste0(out.path,"/model_result_onset.csv"))
duration <- read.csv(paste0(out.path,"/model_result_duration.csv"))

seed <- 0
n.iteration <- 1000
# library(rstudioapi)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(survival)
library(cmprsk)
library(zoo)
library(boot)
library(frailtypack)

# data <- read.csv("Data/reduce data.csv")
# data$letter <- substr(data$CRF_letter_number,1,1)
# data <- data[which(!data$letter %in% c("L","M","N","Q","R","T","U")),]

# % coma on 1st: 52.7%
# % delirium on 1st: 3%
# coma incidence: 61.5%
# delirium incidence: 36%

# duration.lambda <- read.csv("duratin_lambda.csv")
# duration.lambda$coma.lambda <- seq(1,0.433,by=-0.021)
# duration.lambda$delirium.lambda <- seq(1.5,1.122,by=-0.014)

assess.after.icu=0
##### set-up ## 
n=500 # number of observation to simulate
seed <- 1234 # set.seed

#### hazard of discharge & death ####
# assume hazard of death = haz.death0*w*exp(betax)
# assume hazard of discharge = haz.discharge0*w^alpha*exp(betax)
# assume frailty term z ~ gamma(shape=1/theta.z, scale=theta.z)

# alpha.di=0.01
# alpha.m=1
 alpha.di = onset[which(onset$index=="M.2A"),"alpha"]
# alpha.di=  -3
 alpha.m = onset[which(onset$index=="M.2B"),"alpha"]
 alpha.m = -alpha.m
# alpha.m= -alpha.m
# haz.discharge <- 0.075;haz.discharge.hosp <- 0.075;
# haz.death <- 0.0075
dist.z <- "gamma"
# alpha.c <-  -4.330756
# alpha.de <- 3
alpha.c <- alpha.de <- 1

#### hazard of coma & discharge ####
# assume coma/delirium follow weibull distribution
# assume hazard of coma = haz.coma0*v.c*t^(v.c-1)*z*exp(betax)
# assume hazard of discharge = haz.delirium0*v.d*t^(v.d-1)*z*exp(betax)

# par.delirium=c(lambda0.d=4,v.d=0.3);par.coma=c(lambda0.c=5,v.c=0.5) # baseline hazard

## duration of coma / delirium

# #### A - Fit the Weibull model for the terminal event - icu discharge ####
# frailty.dat2$term.event.icu <- frailty.dat2$term.event
# frailty.dat2$term.event.icu <- ifelse(frailty.dat2$term.event.icu==2,0,frailty.dat2$term.event.icu)
# dat.term2 <- frailty.dat2 %>% group_by(id) %>% summarize(maxtime=max(t.stop))
# new.dat2 <- left_join(frailty.dat2,dat.term2,by="id") %>% filter(t.stop==maxtime)
# 
# new.dat2$t.stop <- ifelse(new.dat2$term.event.icu==0,30,new.dat2$t.stop)
# 
# # dat.icu$t.stop <- ifelse(dat.icu$t.stop==0,0.1,icu.dat$t.stop)
# fitA <- survreg(Surv(t.stop,term.event.icu)~trt,data=new.dat2,dist="weibull")
# # shape
# 1/fitA$scale # 0.7956739
# # scale
# exp(coef(fitA)) # 12.7831381 
# 
# fitA <- frailtyPenal(Surv(t.stop,term.event.icu)~trt,data=new.dat2,hazard="Weibull")
# #  thetaA <- fitA$theta
# shapeA <- fitA$shape.weib[1] # 0.7956742
# scaleA <- fitA$scale.weib[1] #  12.77367
# 
# #### B-  it the Weibull model for the terminal event - death as terminating event ####
# frailty.dat$term.event.death <- frailty.dat$term.event
# frailty.dat$term.event.death <- ifelse(frailty.dat$term.event.death==1,0,frailty.dat$term.event.death)
# frailty.dat$term.event.death <- ifelse(frailty.dat$term.event.death==2,1,frailty.dat$term.event.death)
# dat.term <- frailty.dat %>% group_by(id) %>% summarize(maxtime=max(t.stop))
# new.dat <- left_join(frailty.dat,dat.term,by="id") %>% filter(t.stop==maxtime)
# new.dat$t.stop <- ifelse(new.dat$term.event.death==0,30,new.dat$t.stop)
# 
# ## Fit the Weibull model for the terminal event
# fitB <- survreg(Surv(t.stop,term.event.death)~trt,data=new.dat,dist="weibull")
# # survreg:  see ?survreg shape:
# 1/fitB$scale #  0.736046
# # scale, 
# exp(coef(fitB)) # 372.5430031
# fitB <- frailtyPenal(Surv(t.stop,term.event.death)~trt,data=new.dat,hazard="Weibull")
# #  thetaB <- fitB$theta
# shapeB <- fitB$shape.weib[1] #  0.7358903
# scaleB <- fitB$scale.weib[1]#  374.3304


# # A) trt other terminal event as 30days -frailty pack
# onset[which(onset$index=="M.B"),"shape.terminal"] <-  0.6446709
# onset[which(onset$index=="M.B"),"scale.terminal"] <- 776.2161
# 
# onset[which(onset$index=="M.A"),"shape.terminal"] <- 0.7956742
# onset[which(onset$index=="M.A"),"scale.terminal"] <- 12.77367
# 12.11*gamma(1+1/0.85)


#### treatment effect ####
beta.m <- 0 # treatment effect on mortality
beta.di <- 0 # treatment effect on discharge
beta.coma <- 0 # treatment effect on coma
beta.coma.duration <- 0 # treatment effect on coma duration
beta.delirium <- 0 # treatment effect on delirium 
beta.delirium.duration <- 0 # treatment effect on delirium duration

## try
theta.z <- 1
# theta.z <- onset[which(onset$index=="M2A"),"shape.event"]

v.d <- onset[which(onset$index=="M.2"),"shape.event"]
s.d <- onset[which(onset$index=="M.2"),"scale.event"]
lambda0.d <- 1/(s.d^v.d)
v.c <- onset[which(onset$index=="M.1"),"shape.event"]
s.c <- onset[which(onset$index=="M.1"),"scale.event"]
lambda0.c <- 1/(s.c^v.c)

par.delirium=c(lambda0.d,v.d) #v - shape; lambada - 1/scale^shape
par.coma=c(lambda0.c,v.c)

v.icu <- onset[which(onset$index=="M.A"),"shape.terminal"]
s.icu <- onset[which(onset$index=="M.A"),"scale.terminal"]
lambda0.icu <- 1/(s.icu^v.icu)
par.icu=c(lambda0.icu,v.icu) #v - shape; lambada - 1/scale^shape

v.m <- onset[which(onset$index=="M.B0"),"shape.terminal"]
s.m <- onset[which(onset$index=="M.B0"),"scale.terminal"]
lambda0.m <- 1/(s.m^v.m)
par.death=c(lambda0.m,v.m)


v.m2 <- onset[which(onset$index=="M.B2"),"shape.terminal"]
s.m2 <- onset[which(onset$index=="M.B2"),"scale.terminal"]
lambda0.m2 <- 1/(s.m2^v.m2)
par.death2=c(lambda0.m2,v.m2)

nu.d.D <- duration[which(duration$model=="delirium duration"),"shape"]
nu.d.S <- duration[which(duration$model=="delirium duration"),"scale"]
lambda0.d.D <- 1/(nu.d.S^nu.d.D)

nu.c.D <- duration[which(duration$model=="coma duration"),"shape"]
nu.c.S <- duration[which(duration$model=="coma duration"),"scale"]
lambda0.c.D <- 1/(nu.c.S^nu.c.D)
par.delirium.duration=c(lambda0.d.D,nu.d.D)
par.coma.duration=c(lambda0.c.D,nu.c.D)
# par.delirium.duration=c(lambda0.d.D=1/(2.5*1),nu.d.D=1)
# par.coma.duration=c(lambda0.c.D=1/(1.5*0.5),nu.c.D=0.5)



#### simulate control group ####
cntl.dat <- simulate10(n,seed,
                       dist.z,theta.z,
                       alpha.di,alpha.m, # paramter used to generate frailty term for hazard of death & discharge
                       alpha.c,alpha.de,
                       beta.m=0, # treatment effect on mortality, NULL is for control group
                       beta.di=0, # treatment effect on discharge, NULL is for control group
                       beta.coma=0,# treatment effect on coma, NULL is for control group
                       beta.coma.duration=0,# treatment effect on coma duration, NULL is for control group
                       beta.delirium=0,# treatment effect on delirium , NULL is for control group
                       beta.delirium.duration=0 , # treatment effect on delirium duration, NULL is for control group
                       par.icu=par.icu,
                       par.death=par.death,
                       par.delirium=par.delirium,par.coma=par.coma, # baseline hazard
                       par.delirium.duration,par.coma.duration,
                       #                      duration.lambda,coma.duration.theta,delirium.duration.theta,# paratemer for generating duration from negative binomial, theta = mu^2/(var-mu)
                       assess.after.icu
)

trt.dat <- simulate10(n,seed+1,
                      dist.z,theta.z,
                      alpha.di,alpha.m, # paramter used to generate frailty term for hazard of death & discharge
                      alpha.c,alpha.de,
                      beta.m, # treatment effect on mortality, NULL is for control group
                      beta.di, # treatment effect on discharge, NULL is for control group
                      beta.coma,# treatment effect on coma, NULL is for control group
                      beta.coma.duration,# treatment effect on coma duration, NULL is for control group
                      beta.delirium,# treatment effect on delirium , NULL is for control group
                      beta.delirium.duration , # treatment effect on delirium duration, NULL is for control group
                      par.icu=par.icu,
                      par.death=par.death,
                      par.delirium=par.delirium,par.coma=par.coma, # baseline hazard
                      par.delirium.duration,par.coma.duration,
                      #                     duration.lambda,coma.duration.theta,delirium.duration.theta, # paratemer for generating duration from negative binomial, theta = mu^2/(var-mu)
                      assess.after.icu
)


cntl.dat <- simulate10(n,seed,
                      dist.z,theta.z,
                      alpha.di,alpha.m, # paramter used to generate frailty term for hazard of death & discharge
                      alpha.c,alpha.de,
                      beta.m=0, # treatment effect on mortality, NULL is for control group
                      beta.di=0, # treatment effect on discharge, NULL is for control group
                      beta.coma=0,# treatment effect on coma, NULL is for control group
                      beta.coma.duration=0,# treatment effect on coma duration, NULL is for control group
                      beta.delirium=0,# treatment effect on delirium , NULL is for control group
                      beta.delirium.duration=0 , # treatment effect on delirium duration, NULL is for control group
                      par.icu=par.icu,
                      par.death=par.death,
                      par.delirium=par.delirium,par.coma=par.coma, # baseline hazard
                      par.delirium.duration,par.coma.duration,
#                      duration.lambda,coma.duration.theta,delirium.duration.theta,# paratemer for generating duration from negative binomial, theta = mu^2/(var-mu)
                      assess.after.icu
)

trt.dat <- simulate11(n,seed+1,
                     dist.z,theta.z,
                     alpha.di,alpha.m, # paramter used to generate frailty term for hazard of death & discharge
                     alpha.c,alpha.de,
                     beta.m, # treatment effect on mortality, NULL is for control group
                     beta.di, # treatment effect on discharge, NULL is for control group
                     beta.coma,# treatment effect on coma, NULL is for control group
                     beta.coma.duration,# treatment effect on coma duration, NULL is for control group
                     beta.delirium,# treatment effect on delirium , NULL is for control group
                     beta.delirium.duration , # treatment effect on delirium duration, NULL is for control group
                     par.icu=par.icu,
                     par.death=par.death,par.death2=par.death2,
                     par.delirium=par.delirium,par.coma=par.coma, # baseline hazard
                     par.delirium.duration,par.coma.duration,
#                     duration.lambda,coma.duration.theta,delirium.duration.theta, # paratemer for generating duration from negative binomial, theta = mu^2/(var-mu)
                     assess.after.icu
)

cntl <- cntl.dat$sim.dat; trt <- trt.dat$sim.dat

cntl$grp <- 0;trt$grp <- 1; trt$id <- n+trt$id; sim.dat <- rbind(cntl,trt)
print(beta.m)
print(summary(sim.dat$surv.time))
print(table(sim.dat$died))
## % of event on 1st day
table(sim.dat$d1)
table(sim.dat$d2)
table(sim.dat$d3)

sim.dat$coma <- find.x(data=sim.dat,cov.vec=paste0("d",1:28),perfer=1)
sim.dat$delirium <- find.x(data=sim.dat,cov.vec=paste0("d",1:28),perfer=2)

## overall % of event
100*table(sim.dat$coma)/nrow(sim.dat);100*table(sim.dat$delirium)/nrow(sim.dat)

### get % which 1st delirium occur after ICU discharge ###
sim.dat$delirium.on.set <- ifelse(sim.dat$delirium.on.set <= sim.dat$surv.time,sim.dat$delirium.on.set,NA)
sim.dat$del_after_icu <- ifelse(sim.dat$delirium.on.set > sim.dat$icu.los,1,0)

table(!is.na(sim.dat$delirium.on.set),sim.dat$grp) # delirium ever by grp
table(sim.dat$del_after_icu,sim.dat$grp) # 1st delirium onset after ICU discharge by grp

#### plot lasagma plot ####
sim.dat$time <- ifelse(sim.dat$icu.los<=sim.dat$surv.time,sim.dat$icu.los,sim.dat$number_days_survived_28days)
sim.dat$check <- as.numeric(sim.dat$icu.los<sim.dat$surv.time & sim.dat$died)
sim.dat$check.diff <- ifelse(sim.dat$check==1,sim.dat$number_days_survived_28days,0)
sim.dat$censor

# order the patients: 1: patient who discharge then died within 28days
#                     2: patient who died within ICU: (small number of days survived comes first)
#                     3: patient who discharge: (more ICU stay comes first)
sim.dat$order <- 1000*sim.dat$check.diff+100*sim.dat$died+10*(28-sim.dat$number_days_survived_28days)*sim.dat$died + sim.dat$time + sim.dat$id/10000
df.dat <- sim.dat[order(sim.dat$order,decreasing=T),]


# ## get icu and hosp discharge 
sim.dat$check <- as.numeric(sim.dat$icu.los<sim.dat$number_days_survived_28days & sim.dat$died)
sim.dat$check.diff <- ifelse(sim.dat$check==1,sim.dat$number_days_survived_28days,0)
sim.dat$time <- ifelse(sim.dat$icu.los<=sim.dat$number_days_survived_28days,sim.dat$icu.los,sim.dat$number_days_survived_28days)
# sim.dat$time2 <- ifelse(sim.dat$hosp.los<=sim.dat$number_days_survived_28days,sim.dat$hosp.los,sim.dat$number_days_survived_28days)
# sim.dat$order <- 10000*sim.dat$check.diff+1000*sim.dat$died+100*(28-sim.dat$number_days_survived_28days)*sim.dat$died + 10*sim.dat$time +sim.dat$time2+ sim.dat$id/10000
sim.dat$order <- 10000*sim.dat$check.diff+1000*sim.dat$died+100*(28-sim.dat$number_days_survived_28days)*sim.dat$died + 10*sim.dat$time+ sim.dat$id/10000

sim.dat <- sim.dat[order(sim.dat$order,decreasing=T),]
df.long <- reshape(sim.dat,
                   idvar=c("id","order"),
                   varying=c(paste0("d",1:28)),
                   v.names="status",
                   direction="long")
df.long <- df.long[order(df.long$order,decreasing=T),]
df.long$order.id <- rep(c(length(unique(df.long$order)):1),each=28)
# cols <- c("discharge"="grey","hosp" = "grey","ICU"="lightyellow", "delirium" = "red", "coma" = "yellow","both"="red","death" = "green")
cols <- c("-2"="grey","-1" = "grey","0"="lightyellow", "2" = "red", "1" = "yellow","3"="red","4" = "green")
# status <- factor(status,levels=c(-2,-1,0,1,2,3,4),labels=c("hosp.discharge","icu.discharge","ICU","coma","delirium","delirium","death"))
df.long$status <- as.character(df.long$status)

pdf(paste0(out.path,"/lasagna_simdata_from_reg.pdf"))
p <- ggplot(df.long) +
  geom_tile(aes(x = time, y = order.id, fill = status)) + ylab("") +
  scale_fill_manual(values = cols,na.value="white") +
  theme_classic()
print(p)
dev.off()

# simulated data
tmp <- sim.dat[,c("id","grp","icu.los","number_days_survived_28days","died",
                  paste0("d",1:28))]
colnames(tmp)[2:5] <- c("study_arm","losic","number_days_survived","death")
for(day in 1:28){
  tmp[,paste0("d",day)] <- ifelse(tmp[,paste0("d",day)]%in% c(-1,4),NA,tmp[,paste0("d",day)])
}
frailty.sim <- data_format(df.dat=tmp,death.after.icu = 1)
frailty.sim2 <- data_format(df.dat=tmp,death.after.icu = 0)
duration.sim <-  data_duration_format(frailty.sim2)

frailty.duration$duration <- frailty.duration$t.stop - frailty.duration$t.start
duration.sim$duration <- duration.sim$t.stop - duration.sim$t.start
### compare the parameter distribution with REDUCE file ####
set.seed(seed)
aGamma <- 1/theta.z
z      <- rgamma(n,shape=aGamma, scale=1/aGamma)
summary(z)


# hazard of death
haz.m <- haz.death*z^alpha.m
summary(haz.m)

summary(frailty.dat$t.stop[which(frailty.dat$term.event==2)])
T.m <- rexp(n,haz.m)
summary(T.m)

# hazard of icu discharge
par.icu=c(lambda0.icu,v.icu)
haz.di <- lambda0.icu*v.icu
summary(haz.di)


frailty.dat2$term.event.icu <- frailty.dat2$term.event
frailty.dat2$term.event.icu <- ifelse(frailty.dat2$term.event.icu==2,0,frailty.dat2$term.event.icu)
dat.term2 <- frailty.dat2 %>% group_by(id) %>% summarize(maxtime=max(t.stop))
new.dat2 <- left_join(frailty.dat2,dat.term2,by="id") %>% filter(t.stop==maxtime)
new.dat2$t.stop.icu <- ifelse(new.dat2$term.event.icu==0,30,new.dat2$t.stop)
tmp <- new.dat2[which(new.dat2$term.event.icu==1),]

frailty.sim2$term.event.icu <- frailty.sim2$term.event
frailty.sim2$term.event.icu <- ifelse(frailty.sim2$term.event.icu==2,0,frailty.sim2$term.event.icu)
sim.term2 <- frailty.sim2 %>% group_by(id) %>% summarize(maxtime=max(t.stop))
new.sim2 <- left_join(frailty.sim2,sim.term2,by="id") %>% filter(t.stop==maxtime)
new.sim2$t.stop.icu <- ifelse(new.sim2$term.event.icu==0,30,new.sim2$t.stop)
tmp.sim <- new.sim2[which(new.sim2$term.event.icu==1),]

T.icu <- rweibull(nrow(tmp),shape=v.icu,scale=s.icu)
T.icu <- ifelse(T.icu>28,28.1,T.icu)
T.icu <- data.frame(id=1:length(T.icu),T.icu)
sim.dat$sim.T <- ifelse(sim.dat$icu.los>28,28.1,sim.dat$icu.los)
tbl1 <- tableGrob(t(summary(tmp$t.stop)),rows=NULL)
tbl2 <- tableGrob(t(round(summary(T.icu$T.icu,2))),rows=NULL)
tbl3 <- tableGrob(t(round(summary(tmp.sim$t.stop),3)),rows=NULL)

p1 <- ggplot(tmp,aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("icu.los from REDUCE data")
p2 <- ggplot(T.icu,aes(T.icu)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.icu)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(tmp.sim,aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("icu.los from simulated data using parameter")

tbl4 <- tableGrob(t(summary(tmp$t.stop[which(tmp$t.stop<=28)])),rows=NULL)
tbl5 <- tableGrob(t(summary(round(T.icu$T.icu[which(T.icu$T.icu<=28)],2))),rows=NULL)
tbl6 <- tableGrob(t(round(summary(tmp.sim$t.stop[which(tmp.sim$t.stop<=28)]),3)),rows=NULL)

p4 <- ggplot(tmp[which(tmp$t.stop<=28),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("icu.los from REDUCE data")
p5 <- ggplot(T.icu[which(T.icu$T.icu<=28),],aes(T.icu)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.icu)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p6 <- ggplot(tmp.sim[which(tmp.sim$t.stop<=28),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle(paste0("icu.los from simulated data using parameter:scale:",round(v.icu,2),";shape:",round(s.icu,2)))

pdf(paste0(out.path,"/ICU_discharge_diagnosis.pdf"))
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p4,tbl4,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p5,tbl5,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p6,tbl6,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()

#### death ####
# discharge from ICU then died
table(sim.dat$surv.time<=28) #  174 
check <- sim.dat[which(sim.dat$surv.time<=28),]
table(check$icu.los< check$surv.time,useNA="ifany") # 97 died after ICU discharge
# 77 died in ICU


frailty.dat$term.event.death <- ifelse(frailty.dat$term.event==1,0,frailty.dat$term.event)
frailty.dat$term.event.death <- ifelse(frailty.dat$term.event.death==2,1,frailty.dat$term.event.death)
dat.term <- frailty.dat %>% group_by(id) %>% summarize(maxtime=max(t.stop))
new.dat <- left_join(frailty.dat,dat.term,by="id") %>% filter(t.stop==maxtime)
tmp <- new.dat[which(new.dat$term.event.death==1),]

frailty.sim$term.event.death <- ifelse(frailty.sim$term.event==1,0,frailty.sim$term.event)
frailty.sim$term.event.death <- ifelse(frailty.sim$term.event.death==2,1,frailty.sim$term.event.death)
sim.term <- frailty.sim %>% group_by(id) %>% summarize(maxtime=max(t.stop))
new.sim <- left_join(frailty.sim,sim.term,by="id") %>% filter(t.stop==maxtime)
tmp.sim <- new.sim[which(new.sim$term.event.death==1),]


T.death <- rweibull(nrow(d),shape=v.m,scale=s.m)
T.death <- data.frame(id=1:length(T.death),T.death)
T.death <- T.death[which(T.death$T.death<=28),]

tbl1 <- tableGrob(t(summary(tmp$t.stop)),rows=NULL)
tbl2 <- tableGrob(t(round(summary(T.death$T.death),2)),rows=NULL)
tbl3 <- tableGrob(t(round(summary(tmp.sim$t.stop),2)),rows=NULL)


p1 <- ggplot(tmp,aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("time to death from REDUCE data")
p2 <- ggplot(T.death,aes(T.death)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.death)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(tmp.sim,aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle(paste0("time to death from simulated data using parameter:scale:",round(v.m,2),";shape:",round(s.m,2)))

# tbl4 <- tableGrob(t(summary(frailty.dat$t.stop[which(frailty.dat$term.event==2 & frailty.dat$t.stop<28)])),rows=NULL)
# tbl5 <- tableGrob(t(summary(round(T.death$T.death[which(T.death$T.death<28)],2))),rows=NULL)
# tbl6 <- tableGrob(t(summary(frailty.sim$t.stop[which(frailty.sim$term.event==2 & frailty.sim$t.stop<28)])),rows=NULL)
# 
# p4 <- ggplot(frailty.dat[which(frailty.dat$term.event==1& frailty.dat$t.stop<28),],aes(t.stop)) +
#   geom_histogram(color="black",fill="white",binwidth = 1) +
#   geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
#   geom_density(alpha=0.2,fill="#FF6666")+
#   ggtitle("time to death from REDUCE data")
# p5 <- ggplot(T.death[which(T.death$T.death<28),],aes(T.death)) +
#   geom_histogram(color="black",fill="white",binwidth = 1) +
#   geom_vline(aes(xintercept=mean(T.death)),color="blue",linetype="dashed") +
#   geom_density(alpha=0.2,fill="#FF6666")+
#   ggtitle("draw from weibull dist using parameter")
# p6 <- ggplot(frailty.sim[which(frailty.sim$term.event==2 & frailty.sim$t.stop<28),],aes(t.stop)) +
#   geom_histogram(color="black",fill="white",binwidth = 1) +
#   geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
#   geom_density(alpha=0.2,fill="#FF6666")+
#   ggtitle("time to death from simulated data using parameter")

pdf(paste0(out.path,"/death_diagnosis.pdf"))
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
# grid.arrange(p4,tbl4,nrow=2,as.table=TRUE,heights=c(4,1))
# grid.arrange(p5,tbl5,nrow=2,as.table=TRUE,heights=c(4,1))
# grid.arrange(p6,tbl6,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()


#### hazard of coma onset ####
par.coma=c(lambda0.c,v.c)

T.coma <- rweibull(1000,shape=onset[which(onset$index=="M.1"),"shape.event"],scale=onset[which(onset$index=="M.1"),"scale.event"])
T.coma <- ifelse(T.coma>=28,28,T.coma)
T.coma <- as.data.frame(T.coma)
sim.dat$sim.T <- ifelse(sim.dat$coma.on.set>=28,28,sim.dat$coma.on.set)
tbl1 <- tableGrob(t(summary(frailty.dat$t.stop[which(frailty.dat$event==1)])),rows=NULL)
tbl2 <- tableGrob(t(summary(round(T.coma$T.coma,2))),rows=NULL)
tbl3 <-  tableGrob(t(summary(frailty.sim$t.stop[which(frailty.sim$event==1)])),rows=NULL)

p1 <- ggplot(frailty.dat[which(frailty.dat$event==1),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("Time to 1st coma from REDUCE data")
p2 <- ggplot(T.coma,aes(T.coma)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.coma,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(frailty.sim[which(frailty.sim$event==1),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("1st coma on set from simulated data using parameter")

pdf("output/Time_to_1stComa_diagnosis.pdf")
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()

# hazard for delirium onset
par.delirium=c(lambda0.d,v.d) #v - shape; lambada - 1/scale^shape

T.delirium <- rweibull(1000,shape=onset[which(onset$index=="M.2"),"shape.event"],scale=onset[which(onset$index=="M.2"),"scale.event"])
T.delirium <- ifelse(T.delirium>=28,28,T.delirium)
T.delirium <- as.data.frame(T.delirium)
sim.dat$sim.T <- ifelse(sim.dat$delirium.on.set>=28,28,sim.dat$delirium.on.set)
tbl1 <- tableGrob(t(summary(frailty.dat$t.stop[which(frailty.dat$event==2)])),rows=NULL)
tbl2 <- tableGrob(t(summary(round(T.delirium$T.delirium,2))),rows=NULL)
tbl3 <- tableGrob(t(summary(frailty.sim$t.stop[which(frailty.sim$event==2)])),rows=NULL)

p1 <- ggplot(frailty.dat[which(frailty.dat$event==2),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("Time to 1st delirium from REDUCE data")
p2 <- ggplot(T.delirium,aes(T.delirium)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.delirium,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(frailty.sim[which(frailty.sim$event==2),],aes(t.stop)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(t.stop,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("1st delirium on set from simulated data using parameter")

pdf("output/Time_to_1stDelirium_diagnosis.pdf")
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()


# Coma duration
T.delirium <- rweibull(1000,shape=duration[which(duration$index=="M1"),"shape"],scale=duration[which(duration$index=="M1"),"scale"])
# T.delirium <- ifelse(T.delirium>=28,28,T.delirium)
T.delirium <- as.data.frame(T.delirium)
# sim.dat$sim.T <- ifelse(sim.dat$delirium.on.set>=28,28,sim.dat$delirium.on.set)

tbl1 <- tableGrob(t(summary(frailty.duration$duration[which(frailty.duration$event==1)])),rows=NULL)
tbl2 <- tableGrob(t(summary(round(T.delirium$T.delirium,2))),rows=NULL)
tbl3 <- tableGrob(t(summary(duration.sim$duration[which(duration.sim$event==1)])),rows=NULL)

p1 <- ggplot(frailty.duration[which(frailty.duration$event==1),],aes(duration)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(duration)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("coma duration from REDUCE data")
p2 <- ggplot(T.delirium,aes(T.delirium)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.delirium,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(duration.sim[which(duration.sim$event==1),],aes(duration)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(duration,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("coma duration from simulated data using parameter")

pdf("output/Coma_duration_diagnosis.pdf")
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()


# delirium duration
T.delirium <- rweibull(1000,shape=duration[which(duration$index=="M2"),"shape"],scale=duration[which(duration$index=="M2"),"scale"])
# T.delirium <- ifelse(T.delirium>=28,28,T.delirium)
T.delirium <- as.data.frame(T.delirium)
# sim.dat$sim.T <- ifelse(sim.dat$delirium.on.set>=28,28,sim.dat$delirium.on.set)

tbl1 <- tableGrob(t(summary(frailty.duration$duration[which(frailty.duration$event==2)])),rows=NULL)
tbl2 <- tableGrob(t(summary(round(T.delirium$T.delirium,2))),rows=NULL)
tbl3 <- tableGrob(t(summary(duration.sim$duration[which(duration.sim$event==2)])),rows=NULL)

p1 <- ggplot(frailty.duration[which(frailty.duration$event==2),],aes(duration)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(duration)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("delirium duration from REDUCE data")
p2 <- ggplot(T.delirium,aes(T.delirium)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(T.delirium,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("draw from weibull dist using parameter")
p3 <- ggplot(duration.sim[which(duration.sim$event==2),],aes(duration)) +
  geom_histogram(color="black",fill="white",binwidth = 1) +
  geom_vline(aes(xintercept=mean(duration,na.rm=T)),color="blue",linetype="dashed") +
  geom_density(alpha=0.2,fill="#FF6666")+
  ggtitle("delirium duration from simulated data using parameter")

pdf("output/Delirium_duration_diagnosis.pdf")
grid.arrange(p1,tbl1,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p2,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
grid.arrange(p3,tbl3,nrow=2,as.table=TRUE,heights=c(4,1))
dev.off()



# t=1
lambda0.c*v.c




