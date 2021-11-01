

## take daily status data (wide format) and tranform to by event with event start time and end time (long format)
# status code: 1 - coma; 2 - delirum
# column name: "id","study_arm","losic","number_days_survived","death"
#    followed by status of the day: dX.

# return frailty data: "id","t.start","t.stop","event","term.event","trt"
#        event: 1 - coma, 2 - delirium, 
#               3 - for last record only, when last day is still in coma; 4- for last record only, when last day is still in delirium
#               NA - for last record only, when missing daily status before ICU discharge date or death.
#        terminal event: 1 - icu discharge; 2 - death

## simulate: V8 - fill in the status grid along with the simulation
##           V9 - simulate all onset time and duration tie first, then fill in the status grid


data_duration_format <- function(frailty.dat){
  require(data.table)
  cols = c("t.start","event")
  anscols=paste0(cols,"_next")
  dat.tmp <- data.table(frailty.dat)
  dat.tmp[, (anscols) := shift(.SD, 1, NA, "lead"), .SDcols=cols,by=id]
  
  dat.tmp$t.stop2 <- ifelse(!is.na(dat.tmp$event_next),dat.tmp$t.start_next,dat.tmp$t.stop+1)
  dat.tmp2 <- dat.tmp[which(dat.tmp$event %in% c(1,2)),]
  dat.tmp2$t.start <- dat.tmp2$t.stop
  dat.tmp2$t.stop <- dat.tmp2$t.stop2
  dat.tmp2$t.start_next <- dat.tmp2$event_next <- dat.tmp2$t.stop2 <- NULL
  
  return(dat.tmp2)
}

data_format <- function(df.dat=data,censor.day=28,death.after.icu=1){
  # death.after.icu =1, treat patient died after icu discharge as terminal event died.
  # death.after.icu =0, treat patient died after icu discharge as icu discharge.
  
  stopifnot(c("id","study_arm","losic","number_days_survived","death") %in% colnames(df.dat))
  #  censor.day <- strsplit(colnames(df.dat)[ncol(df.dat)],"d")[[1]][2]
  #  censor.day <- as.numeric(censor.day)
  
  frailty.dat <- vector()
  
  
  df.dat$term.event.e <-ifelse(df.dat$death==1,2,ifelse(df.dat$losic<=censor.day,1,0))
  if(death.after.icu==0){
    df.dat$term.event.e <- ifelse(!is.na(df.dat$number_days_survived) & df.dat$number_days_survived>df.dat$losic & df.dat$losic<=28,1,df.dat$term.event.e)
  }
  df.dat$number_days_survived <- ifelse(df.dat$death==0,censor.day,df.dat$number_days_survived)
  
  for(id in df.dat$id){
    
    # initialize
    x<-t(df.dat[which(df.dat$id==id),paste0("d",1:censor.day)])
    #  x <- ifelse(x %in% c(1,3),1,x)
    status <- data.frame(day=1:censor.day,x); colnames(status)[2] <- "status"
    trt <- df.dat[which(df.dat$id==id),"study_arm"]
    icu.los <- df.dat[which(df.dat$id==id),"losic"]; alive.day <-  df.dat[which(df.dat$id==id),"number_days_survived"]
    if(death.after.icu==1){
      t.end <- ifelse(df.dat[which(df.dat$id==id),"death"]==0,icu.los,alive.day) 
    }else{
      t.end <- ifelse(df.dat[which(df.dat$id==id),"death"]==0,icu.los,min(icu.los,alive.day))
    }
    t.end <- ifelse(t.end>censor.day,censor.day,t.end)
    term.event.e <- df.dat$term.event.e[which(df.dat$id==id)]
    
    if(t.end==0){
      Row <- cbind(0,0.1,0,term.event.e)
    }else{
      status <- status[c(1:t.end),] 
      
      t.start=status[1,"day"]-1;status0 <- 0;t.stop=status[1,"day"]-1;term.event=0
      j=1
      Row <- vector()
      while(j < nrow(status)){
        status1 <- status[j,"status"]
        if(status1 %in% c(0)){
          t.stop=status[j,"day"]
          status0 <- status1
          j<- j+1
        }else if(status1 %in% c(NA)){
          if(status0 %in% c(0)){
            row <- c(t.start,t.stop,status0,0)
            Row <- rbind(Row,row) 
          }
          t.start=status[j,"day"]
          t.stop=status[j,"day"]
          status0 <- status1
          j<- j+1
        }else if(status1 %in% c(1)){ # coma
          if(!status0 %in% status1){
            row <- c(t.start,t.stop,1,term.event)
            Row <- rbind(Row,row)
            t.start=status[j,"day"];t.stop=status[j,"day"];term.event=0;status0 <- status1
            j<- j+1
          }else{
            t.start=status[j,"day"];t.stop=status[j,"day"]
            status0 <- status1
            j<- j+1
          }
          
        }else if(status1 %in% c(2)){ # delirium
          if(!status0 %in% status1){
            row <- c(t.start,t.stop,2,term.event)
            Row <- rbind(Row,row)
            t.start=status[j,"day"];t.stop=status[j,"day"];term.event=0;status0 <- status1
            j<- j+1
          }else{
            t.start=status[j,"day"];t.stop=status[j,"day"]
            status0 <- status1
            j<- j+1
          }
        }else{
          if(status0 %in% c(0)){
            row <- c(t.start,t.stop,0,term.event)
            Row <- rbind(Row,row)
            t.start=status[j,"day"];t.stop=status[j,"day"];term.event=0;status0 <- status1
            j<- j+1
          }else{
            t.stop=status[j,"day"]
            status0 <- status1
            j<- j+1
          }
        }
      }
      
      status1 <- status[nrow(status),"status"]
      if(status1 %in% c(0)){
        row <- c(t.start,status[nrow(status),"day"],0,term.event.e)
        Row <- rbind(Row,row)
      }else if(status1 %in% c(1)){ # coma
        if(!status0 %in% status1){
          row <- c(t.start,t.stop,1,0)
          t.start <- t.stop <- status[j,"day"]
          row2 <- c(t.start,t.stop,0,term.event.e)
          Row <- rbind(Row,row,row2)
        }else{ #  
          t.start=status[j,"day"];t.stop=status[j,"day"]
          row <- c(t.start,t.stop,3,term.event.e)
          Row <- rbind(Row,row)
        }
        
      }else if(status1 %in% c(2)){ # delirium
        if(!status0 %in% status1){
          row <- c(t.start,t.stop,2,0)
          t.start <- t.stop <- status[j,"day"]
          row2 <- c(t.start,t.stop,0,term.event.e)
          Row <- rbind(Row,row,row2)
        }else{
          t.start=status[j,"day"];t.stop=status[j,"day"]
          row <- c(t.start,t.stop,4,term.event.e)
          Row <- rbind(Row,row)
        }
      }else if(is.na(status1)){
        if(status0 %in% c(1,2)){
          t.start=status[j,"day"];t.stop=status[j,"day"]
          row <- c(t.start,t.stop,status1,term.event.e)
          Row <- rbind(Row,row) 
        }else if(status0 %in% c(0)){
          #        t.stop=t.stop-1
          row <- c(t.start,t.stop,status0,0)
          Row <- rbind(Row,row)
          t.start=status[j,"day"];t.stop=status[j,"day"]
          row <- c(t.start,t.stop,status1,term.event.e)
          Row <- rbind(Row,row)
        }else{
          t.start=status[j,"day"];t.stop=status[j,"day"]
          row <- c(t.start,t.stop,status1,term.event.e)
          Row <- rbind(Row,row)
        }
      }
    }
    
    
    dat.tmp<- cbind(id,Row,trt)
    frailty.dat   <- rbind(frailty.dat ,dat.tmp)
  }
  
  frailty.dat <- as.data.frame(frailty.dat)
  colnames(frailty.dat) <- c("id","t.start","t.stop","event","term.event","trt")
  frailty.dat[which(frailty.dat$t.start==frailty.dat$t.stop),"t.stop"] <- frailty.dat[which(frailty.dat$t.start==frailty.dat$t.stop),"t.stop"]+0.1
  
  return(frailty.dat)
}




fit.model.onset <- function(frailty.dat,frailty.dat2,out.path){
  require(frailtypack)
  require(tidyr)
  require(tidyverse)
  dir.create("output",showWarnings = FALSE)
  # frailty.dat[which(frailty.dat$event %in% c(3,4)),"event"] <- NA
  
  dat.tmp <- frailty.dat
  dat.tmp[which(dat.tmp$event %in% c(3,4)),"event"] <- 0
  dat.tmp[which(is.na(dat.tmp$event)),"event"] <- 0
  
  
  dat.tmp$coma.event <- dat.tmp$event
  dat.tmp$delirium.event <- dat.tmp$event
  
  dat.tmp[which(dat.tmp$coma.event==2),"coma.event"] <- 0
  
  dat.tmp[which(dat.tmp$delirium.event==1),"delirium.event"] <- 0
  dat.tmp[which(dat.tmp$delirium.event==2),"delirium.event"] <- 1
  
  dat.icu <- frailty.dat2
  dat.icu$term.event.icu <- dat.icu$term.event
  dat.icu$term.event.icu <- ifelse(dat.icu$term.event.icu==2,0,dat.icu$term.event.icu)

  dat.tmp$term.event.death <- dat.tmp$term.event
  dat.tmp$term.event.death <- ifelse(dat.tmp$term.event.death==1,0,dat.tmp$term.event.death)
  dat.tmp$term.event.death <- ifelse(dat.tmp$term.event.death==2,1,dat.tmp$term.event.death)
  # 
  #####  model for coma ####
  ### Simplifying the problem by first fitting the recurrent model
  ### ignoring the terminal event
  dat.tmp2 <- dat.tmp[!is.na(dat.tmp$coma.event),]
  fit.1 <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id),
                        data=dat.tmp2,recurrentAG=TRUE,hazard="Weibull")
  # theta of frailty term) (variance of gamma frailty parameter) : 
  theta.1 <- fit.1$theta
  shape.1 <- fit.1$shape.weib[1]
  scale.1 <- fit.1$scale.weib[1]
  
  tmp <- dat.tmp[which(dat.tmp$event==1),]
#  set.seed(2143)
  x.1 <- rweibull(n=nrow(tmp),shape=shape.1,scale=scale.1) # mean: scale*gamma(1+1/shape)
  smry.10 <- summary(tmp$t.stop)
  smry.11 <- summary(x.1)
  max <- ceiling(max(smry.10["Max."],smry.11["Max."])/2)*2
  
  pdf("output/Hist_coma_onset.pdf")
  par(mfcol=c(2,1))
  hist(tmp$t.stop,breaks=seq(0,max,by=2),main=paste0("Reduce data: Histogram of coma onset"))
  # hist(x)
  hist(x.1,breaks=seq(0,max,by=2),main=paste0("Simulate data: shape=",shape.1,",scale=",scale.1))
  dev.off()
  
  ### model for delirium ####
  ### Simplifying the problem by first fitting the recurrent model
  ### ignoring the terminal event
  fit.2 <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id),
                        data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Weibull")
  theta.2 <- fit.2$theta
  shape.2 <- fit.2$shape.weib[1]
  scale.2 <- fit.2$scale.weib[1]
  
  tmp <- dat.tmp[which(dat.tmp$event==1),]
  
  # gamma(1+0.96)*7.56/gamma(1+0.5)
#  set.seed(2143)
  x.2 <- rweibull(n=nrow(tmp),shape=shape.2,scale=scale.2) # mean: scale*gamma(1+1/shape)
  smry.20 <- summary(tmp$t.stop)
  smry.21 <- summary(x.2)
  max <- ceiling(max(smry.20["Max."],smry.21["Max."])/2)*2
  
  pdf("output/Hist_delirium_onset.pdf")
  par(mfcol=c(2,1))
  hist(tmp$t.stop,breaks=seq(0,max,by=2),main="Reduce data: Histogram of delirium onset")
  hist(x.2,breaks=seq(0,max,by=2),main=paste0("Simulate data: shape=",shape.2,",scale=",scale.2))
  dev.off()
  
  #### A - Fit the Weibull model for the terminal event - icu discharge ####
  dat.term <- dat.icu %>% group_by(id) %>% summarize(maxtime=max(t.stop))
  new.dat <- left_join(dat.icu,dat.term,by="id") %>% filter(t.stop==maxtime)
  fitA <- frailtyPenal(Surv(t.stop,term.event.icu)~trt,data=new.dat,hazard="Weibull")
#  thetaA <- fitA$theta
  shapeA <- fitA$shape.weib[1]
  scaleA <- fitA$scale.weib[1]
  
  #### B-  it the Weibull model for the terminal event - death as terminating event ####
  ## Fit the Weibull model for the terminal event
  dat.term <- dat.tmp %>% group_by(id) %>% summarize(maxtime=max(t.stop))
  new.dat <- left_join(dat.tmp,dat.term,by="id") %>% filter(t.stop==maxtime)
  fitB <- frailtyPenal(Surv(t.stop,term.event.death)~trt,data=new.dat,hazard="Weibull")
#  thetaB <- fitB$theta
  shapeB <- fitB$shape.weib[1]
  scaleB <- fitB$scale.weib[1]
  
  #####  model for delirium ####
  ### Simplifying the problem by first fitting the recurrent model
 
  ## Note the theta parameter in this model is very small, so it looks 
  ## like there may not be much information about the variance parameter
  ## for the random effect / frailty distribution
  ## this is likely why we were getting convergence problems!
  
  if(abs(theta.1)>0.1){
    fit1A <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                       data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Weibull")
    theta.1A <- fit1A$theta
    alpha.1A <- fit1A$alpha
    shape.1A <- fit1A$shape.weib
    scale.1A <- fit1A$scale.weib

    fit1B <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                         data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Weibull")

    theta.1B <- fit2B$theta
    alpha.1B <- fit2B$alpha
    shape.1B <- fit2B$shape.weib
    scale.1B <- fit2B$scale.weib
    
    fit1A.smooth <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                                 data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Splines",
                                 n.knots=7,kappa=c(100,100))
    
    alpha.1A.s <- fit1A.smooth$alpha
    
    
    fit1A.piecewise <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                                    data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Piecewise-per",
                                    nb.int = c(4,4))
    alpha.1A.p <- fit1A.piecewise$alpha
    
    fit1B.smooth <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                                 data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Splines",
                                 n.knots=7,kappa=c(100,100))
    
    alpha.1B.s <- fit1B.smooth$alpha
    
    fit1B.piecewise <- frailtyPenal(Surv(t.start,t.stop,coma.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                                    data=dat.tmp[!is.na(dat.tmp$coma.event),],recurrentAG=TRUE,hazard="Piecewise-per",
                                    nb.int = c(4,4))
    alpha.1B.p <- fit1A.piecewise$alpha
    
  }else{
    theta.1A <- alpha.1A <- shape.1A <- scale.1A <- NULL
    theta.1B <- alpha.1B <- shape.1B <- scale.1B <- NULL
    alpha.1A.s <- alpha.1A.p <-NA
    alpha.1B.s <- alpha.1B.p <-NA
  }
  
  if(abs(theta.2)>0.1){
    fit2A <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                         data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Weibull")

    theta.2A <- fit2A$theta
    alpha.2A <- fit2A$alpha
    shape.2A <- fit2A$shape.weib
    scale.2A <- fit2A$scale.weib

    fit2B <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                          data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Weibull")

    theta.2B <- fit2B$theta
    alpha.2B <- fit2B$alpha
    shape.2B <- fit2B$shape.weib
    scale.2B <- fit2B$scale.weib
    
    fit2A.smooth <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                                 data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Splines",
                                 n.knots=7,kappa=c(100,100))
    
    alpha.2A.s <- fit2A.smooth$alpha
    
    fit2A.piecewise <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.icu),formula.terminalEvent=~trt,
                                    data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Piecewise-per",
                                    nb.int = c(4,4))
    alpha.2A.p <- fit2A.piecewise$alpha
    
    
    fit2B.smooth <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                                 data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Splines",
                                 n.knots=7,kappa=c(100,100))
  
    
    alpha.2B.s <- fit2B.smooth$alpha
    
    fit2B.piecewise <- frailtyPenal(Surv(t.start,t.stop,delirium.event)~trt+cluster(id)+terminal(term.event.death),formula.terminalEvent=~trt,
                                    data=dat.tmp[!is.na(dat.tmp$delirium.event),],recurrentAG=TRUE,hazard="Piecewise-per",
                                    nb.int = c(4,4))
    alpha.2B.p <- fit2B.piecewise$alpha
    
    
  }else{
    theta.2A <- alpha.2A <- shape.2A <- scale.2A <- NULL
    theta.2B <- alpha.2B <- shape.2B <- scale.2B <- NULL
    alpha.2A.s <- alpha.2A.p <-NA
    alpha.2B.s <- alpha.2B.p <-NA
  }
  
  onset.out <- data.frame(index=c("M.1","M.2","M.A","M.B",
                                  "M.1A","M.1B","M.1A.s","M.1B.s","M.1A.p","M.1B.p",
                                  "M.2A","M.2B","M.2A.s","M.2B.s","M.2A.p","M.2B.p"),
                          model=c("ignore terminal, fit recurrent for coma","ignore terminal, fit recurrent for delirium",
                                  "ignore recurrent,fit weibull with ICU discharge as terminal event","ignore recurrent,fit weibull with death as terminal event",
                                  "joint:coma,ICU discharge","joint:coma,death",
                                  "joint (spline):coma,ICU discharge","joint (spline):coma,death",
                                  "joint (piecewise):coma,ICU discharge","joint (piecewise):coma,death",
                                  "joint:delirium,ICU discharge","joint:delirium,death",
                                  "joint (spline):delirium,ICU discharge","joint (spline):delirium,death",
                                  "joint (piecewise):delirium,ICU discharge","joint (piecewise):delirium,death"),
                          theta =c(theta.1,theta.2,NA,NA,
                                   NA,NA,NA,NA,NA,NA,
                                   NA,NA,NA,NA,NA,NA),
                          alpha = c(NA,NA,NA,NA,
                                    alpha.1A,alpha.1B,alpha.1A.s,alpha.1B.s,alpha.1A.p,alpha.1B.p,
                                    alpha.2A,alpha.2B,alpha.2A.s,alpha.2B.s,alpha.2A.p,alpha.2B.p),
                          shape.event =c(shape.1,shape.2,NA,NA,
                                         NA,NA,NA,NA,NA,NA,
                                         NA,NA,NA,NA,NA,NA),
                          scale.event =c(scale.1,scale.2,NA,NA,
                                         NA,NA,NA,NA,NA,NA,
                                         NA,NA,NA,NA,NA,NA),
                          shape.terminal =c(NA,NA,shapeA,shapeB,
                                            NA,NA,NA,NA,NA,NA,
                                            NA,NA,NA,NA,NA,NA),
                          scale.terminal =c(NA,NA,scaleA,scaleB,
                                            NA,NA,NA,NA,NA,NA,
                                            NA,NA,NA,NA,NA,NA))

  return(onset.out)
}
  

fit.model.duration <- function(frailty.duration,out.path){
  require(frailtypack)
  dir.create("output",showWarnings = FALSE)
  ###### model coma duration #####
  dat.tmp <- frailty.duration[which(frailty.duration$event==1),]
  dat.tmp$time <- dat.tmp$t.stop - dat.tmp$t.start
  
  ### ignoring the terminal event, model coma duration
  fit0C <- frailtyPenal(Surv(t.start,t.stop,event)~trt+cluster(id),
                        data=dat.tmp,recurrentAG=TRUE,hazard="Weibull")
  theta.1 <- fit0C$theta
  shape.1 <- fit0C$shape.weib[1]
  scale.1 <- fit0C$scale.weib[1]

  # gamma(1+0.96)*7.56/gamma(1+0.5)
#  set.seed(2143)
  x.2 <- rweibull(n=nrow(dat.tmp),shape=shape.1,scale=scale.1) # mean: scale*gamma(1+1/shape)
  x.2 <- as.data.frame(x.2)
  smry.20 <- round(summary(dat.tmp$time),2)
  smry.21 <- round(summary(x.2$x.2),2)
  max <- ceiling(max(smry.20["Max."],smry.21["Max."])/2)*2
  y.max <- ceiling(max(table(cut(dat.tmp$time,breaks=seq(0,max))), table(cut(x.2$x.2,breaks=seq(0,max))))/5)*5
  
  tbl1 <- tableGrob(t(smry.20),rows=NULL)
  tbl2 <- tableGrob(t(smry.21),rows=NULL)
  
  p1 <- ggplot(dat.tmp,aes(time)) +
    geom_histogram(color="black",fill="white",binwidth =1) +
    geom_vline(aes(xintercept=mean(time)),color="blue",linetype="dashed") +
    xlim(-1,max)+ylim(0,y.max)+
    geom_density(alpha=0.2,fill="#FF6666")+
    ggtitle("coma duration from REDUCE data")
  p2 <- ggplot(x.2,aes(x.2)) +
    geom_histogram(color="black",fill="white",binwidth = 1) +
    geom_vline(aes(xintercept=mean(x.2,na.rm=T)),color="blue",linetype="dashed") +
    xlim(-1,max)+ylim(0,y.max)+
    geom_density(alpha=0.2,fill="#FF6666")+
    ggtitle(paste0("draw from weibull dist using parameter,shape=",round(shape.1,2),",scale=",round(scale.1,2)))
  
  pdf(paste0(out.path,"/Hist_coma_duration.pdf"),width=12)
  grid.arrange(p1,p2,tbl1,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
  dev.off()
  
  ###### model delirium duration #####
  dat.tmp <- frailty.duration[which(frailty.duration$event==2),]
  dat.tmp$event <- 1
  dat.tmp$time <- dat.tmp$t.stop - dat.tmp$t.start
  
  ### ignoring the terminal event
  # fit2C <- survreg(Surv(t.stop-t.start,event)~trt,data=dat.tmp,dist="weibull")
  # 1/fit2C$scale 
  # # scale
  # exp(coef(fit2C)) 
  
  fit2C <- frailtyPenal(Surv(t.start,t.stop,event)~trt+cluster(id),
                        data=dat.tmp,recurrentAG=TRUE,hazard="Weibull")
  theta.2 <- fit2C$theta
  shape.2 <- fit2C$shape.weib[1]
  scale.2 <- fit2C$scale.weib[1]
  ## Note the theta parameter in this model is very small, so it looks 
  ## like there may not be much information about the variance parameter
  ## for the random effect / frailty distribution
  ## this is likely why we were getting convergence problems!
  
  
  # pdf("Data/Hist_delirium_onset.pdf")
  # hist(tmp$t.stop,main="Reduce data: Histogram of delirium onset")
  # dev.off()
  # gamma(1+0.96)*7.56/gamma(1+0.5)
#  set.seed(2143)
  x.2 <- rweibull(n=nrow(dat.tmp),shape=shape.2,scale=scale.2) # mean: scale*gamma(1+1/shape)
  x.2 <- as.data.frame(x.2)
  smry.20 <- round(summary(dat.tmp$time),2)
  smry.21 <- round(summary(x.2$x.2),2)
  max <- ceiling(max(smry.20["Max."],smry.21["Max."])/2)*2
  y.max <- ceiling(max(table(cut(dat.tmp$time,breaks=seq(0,max))), table(cut(x.2$x.2,breaks=seq(0,max))))/5)*5

  tbl1 <- tableGrob(t(smry.20),rows=NULL)
  tbl2 <- tableGrob(t(smry.21),rows=NULL)
  
  p1 <- ggplot(dat.tmp,aes(time)) +
    geom_histogram(color="black",fill="white",binwidth =1) +
    geom_vline(aes(xintercept=mean(time)),color="blue",linetype="dashed") +
    xlim(-1,max)+ylim(0,y.max)+
    geom_density(alpha=0.2,fill="#FF6666")+
    ggtitle("delirium duration from REDUCE data")
  p2 <- ggplot(x.2,aes(x.2)) +
    geom_histogram(color="black",fill="white",binwidth = 1) +
    geom_vline(aes(xintercept=mean(x.2,na.rm=T)),color="blue",linetype="dashed") +
    xlim(-1,max)+ylim(0,y.max)+
    geom_density(alpha=0.2,fill="#FF6666")+
    ggtitle(paste0("draw from weibull dist using parameter,shape=",round(shape.1,2),",scale=",round(scale.1,2)))
  
  pdf(paste0(out.path,"/Hist_delirium_duration.pdf"),width=12)
  grid.arrange(p1,p2,tbl1,tbl2,nrow=2,as.table=TRUE,heights=c(4,1))
  dev.off()
  
  duration.out <- data.frame(index=c("M1","M2"),
                             model=c("coma duration",
                                  "delirium duration"),
                          theta =c(theta.1,theta.2),
                          shape =c(shape.1,shape.2),
                          scale =c(scale.1,scale.2))
  
  return(duration.out)
}


gen.lp <- function(n,x){
  if(is.null(x)){
    beta.x <- rep(0,n)
  }else{
    beta.x <- rep(x,n)
  }
  return(beta.x)
}


## simulate icu discharge from weibull, time to death from exponential
## coma and delirium from weibull (time to next event)
## simulate coma, delirium duration from weibull

###  get event status and simulate duration of event
get.status10 <- function(t.c,t.d,U,U2,z,t.start,
                        duration.lambda,
                        beta.coma.duration.x,lambda0.c.D,nu.c.D,
                        beta.delirium.duration.x,lambda0.d.D,nu.d.D,
                        seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(t.c<=t.d){
    event=1 # coma
    t.end = t.c
    
    
    # simulate time to event free
    Y.c <- (-1)*log(U)*exp((-1)*beta.coma.duration.x)*(z)
    duration.tmp <- ((lambda0.c.D)^(-1)*Y.c)^(1/nu.c.D)
#    duration <- ceiling(duration.tmp)
    
    
  }else{
    event=2 # delirium
    t.end=t.d
    
    # simulate time to first delirium
    Y.d <- (-1)*log(U2)*exp((-1)*beta.delirium.duration.x)*(z)
    duration.tmp <- ((lambda0.d.D)^(-1)*Y.d)^(1/nu.d.D)
#    duration <- duration.tmp
  }
  return(c(t.start,t.end,event,duration.tmp))
}


simulate10 <- function(n=500,seed=1234,
                      dist.z="lognormal",theta.z=1,
                      alpha.di=-0.01,alpha.m=1, # paramter used to generate frailty term for hazard of death & discharge
                      alpha.c=1.1,alpha.de=1.1,
                      beta.m = 0, # treatment effect on mortality, NULL is for control group
                      beta.di = 0, # treatment effect on discharge, NULL is for control group
                      beta.coma = 0,# treatment effect on coma, NULL is for control group
                      beta.coma.duration = 0,# treatment effect on coma duration, NULL is for control group
                      beta.delirium = 0,# treatment effect on delirium , NULL is for control group
                      beta.delirium.duration = 0, # treatment effect on delirium duration, NULL is for control group
                      par.icu=c(lambda0.icu=1/(13^1.2),v.icu=1.2),
                      par.death=c(lambda0.m=1/(13^1.2),v.m=1.2),
                      par.delirium=c(lambda0.d=5.2,nu.d=0.82),
                      par.coma=c(lambda0.c=2.81,nu.c=1), # baseline hazard
                      par.delirium.duration=c(lambda0.d.D=5.2,nu.d.D=0.82),par.coma.duration=c(lambda0.c.D=2.81,nu.c.D=1),
                      assess.after.icu=1
){
  
  require(MASS)
  beta.m.x <- gen.lp(n,beta.m)
  beta.di.x <- gen.lp(n,beta.di)
  beta.coma.x <- gen.lp(n,beta.coma)
  beta.coma.duration.x <- ifelse(is.null(beta.coma.duration),0,beta.coma.duration)
  beta.delirium.x <- gen.lp(n,beta.delirium)
  beta.delirium.duration.x <- ifelse(is.null(beta.delirium.duration),0,beta.delirium.duration)
  
  set.seed(seed)
  U.mat1 <- matrix(runif(n),nrow=n,ncol=28)
  U.mat2 <- matrix(runif(n),nrow=n,ncol=28)
  U.mat3 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat4 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat5 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat6 <- matrix(runif(n*28),nrow=n,ncol=28)
  #  print(seed)
  # generating the frailty variable z
  dist.z <- match.arg(dist.z, choices=c("gamma", "lognormal"))
  if(length(theta.z)!=1){stop("theta.z has wrong dimension")}
  if(theta.z < 0) {stop("theta.z must be non-negative")}
  if(theta.z!=0){                                       # if theta.z=0 then frailty=1 for all
    if(dist.z=="gamma"){                              # gamma-frailty
      aGamma <- 1/theta.z
      z      <- rgamma(n,shape=aGamma, scale=theta.z)
    } else {                                          # lognormal frailty
      mu.z    <- log(1/sqrt(theta.z + 1))
      sigma.z <- sqrt(log(theta.z+1))
      z       <- exp(rnorm(n, mean = mu.z, sd=sigma.z))
    }
  }
  
  
  lambda0.icu <- par.icu[1]
  nu.icu     <- par.icu[2]
  
  lambda0.m <- par.death[1]
  nu.m     <- par.death[2]
  
  icu.time.sim <- death.time.sim <- vector()
  for(i in 1:n){
    U <- U.mat1[i]
    
    # simulate time to icu discharge
    Y.icu <- (-1)*log(U)*exp((-1)*beta.di.x[i])*(z[i]^alpha.di)^(-1)
    t.icu <- ((lambda0.icu)^(-1)*Y.icu)^(1/nu.icu)
    icu.time.sim <- c(icu.time.sim,t.icu)
    
    U <- U.mat2[i]
    # # simulate time to death
   Y.m <- (-1)*log(U)*exp((-1)*beta.m.x[i])*(z[i]^alpha.m)^(-1)
   t.m <- ((lambda0.m)^(-1)*Y.m)^(1/nu.m)
   death.time.sim <- c(death.time.sim,t.m)
  }
  
  # hazard for death - assume exponetial dist
  #  haz.m <- haz.death*(z^alpha.m)*exp(beta.m.x)
  #  death.time.sim <- rexp(n,haz.m)
  # print(summary(haz.m))
  #  print(summary(death.time.sim))
  # summary(death.time.sim)
  
  
  # coma & delirium
  if(length(par.coma)!=2){stop("par.rec has wrong dimension")}
  if(length(par.delirium)!=2){stop("par.rec has wrong dimension")}
  lambda0.c <- par.coma[1]
  nu.c     <- par.coma[2]
  lambda0.d <- par.delirium[1]
  nu.d     <- par.delirium[2]
  lambda0.c.D <- par.coma.duration[1]
  nu.c.D     <- par.coma.duration[2]
  lambda0.d.D <- par.delirium.duration[1]
  nu.d.D     <- par.delirium.duration[2]
  
  
  # simulate coma & delirium status and duration
  status.matrix <- vector()
  PT.DAT <- vector()
  #  print(U.mat[100,1])
  for(i in 1:n){
    #    print(i)
    status.c <- status.d <- duration<- 0
    U <- U.mat3[i,1]
    U2 <- U.mat4[i,1]
    
    # simulate time to first coma
#    Y.c <- (-1)*log(U)*exp((-1)*beta.coma.x[i])
    Y.c <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
    t.c <- ((lambda0.c)^(-1)*Y.c)^(1/nu.c)
    
    
    # simulate time to first delirium
    Y.d <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
    t.d <- ((lambda0.d)^(-1)*Y.d)^(1/nu.d)
    
    pt.dat <- vector()
    if(min(t.c,t.d)<28){
      update <- get.status10(t.c=t.c,t.d=t.d,U= U.mat5[i,1],U2= U.mat6[i,1],z=z[i],t.start=0,
                            duration.lambda,
                            beta.coma.duration.x,lambda0.c.D,nu.c.D,
                            beta.delirium.duration.x,lambda0.d.D,nu.d.D,
                            seed=NULL)
      
      st.day <- update[2];status=update[3];duration=update[4]
      
      pt.dat <- rbind(pt.dat,update)
  
    }else{
      # if the time to first event is larger than 28days, no event
      
      update <- c(0,27.1,0,0)
      pt.dat <- rbind(pt.dat,update)
      st.day <- min(t.c,t.d)
      duration=0
    }
    

    
    t.start <- st.day+duration
    
    # recursive step: simulation of N subsequent event times
    while (t.start < 28) {
      
      U  <- U.mat3[i,ceiling(t.start)]
      U2  <- U.mat4[i,ceiling(t.start)]
      if(status==1){
        # if previous event is coma:
        #      time not at risk for coma is duration of previous event + 1day
        #      time not at risk for delirium is duration of previous event
        
#        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])
        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
        t1.c <- t.start+1
        t.c <- (t1.c + ((Y.c+lambda0.c*(t1.c)^(nu.c))/lambda0.c)^(1/nu.c)-(t1.c))
        # delirium
        Y.d  <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
        t1.d <- t.start
        t.d <- (t1.d + ((Y.d+lambda0.d*(t1.d)^(nu.d))/lambda0.d)^(1/nu.d)-(t1.d))
      }else if(status==2){
        # if previous event is delirium:
        #      time not at risk for coma is duration of previous event + 1 day
        #      time not at risk for delirium is duration of previous event + 1 day
        
        # coma
        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
        t1.c <- t.start+1
        t.c <- (t1.c + ((Y.c+lambda0.c*(t1.c)^(nu.c))/lambda0.c)^(1/nu.c)-(t1.c))
        # delirium
        Y.d  <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
        t1.d <- t.start+1
        t.d <- (t1.d + ((Y.d+lambda0.d*(t1.d)^(nu.d))/lambda0.d)^(1/nu.d)-(t1.d))
      }
      
      if(min(t.c,t.d) < 28){
        update <- get.status10(t.c=t.c,t.d=t.d,U= U.mat5[i,1],U2= U.mat6[i,1],z=z[i],t.start,
                              duration.lambda,
                              beta.coma.duration.x,lambda0.c.D,nu.c.D,
                              beta.delirium.duration.x,lambda0.d.D,nu.d.D,
                              seed=NULL)
        
        st.day <- update[2];status=update[3];duration=update[4]
        pt.dat <- rbind(pt.dat,update)
        t.start <- st.day+duration
        
        # if((st.day+duration-1)<= 28){
        #   pt.status[st.day:(st.day+duration-1),"status"] <- rep(status,duration)
        # }else{
        #   pt.status[st.day:28,"status"] <- rep(status,(28-st.day)+1)
        # }

      }else{
        
        update <- c(t.start,27.1,0,0)
        pt.dat <- rbind(pt.dat,update)
        t.start=29
      }
    }
    
    
    pt.dat <- as.data.frame(pt.dat)
    colnames(pt.dat) <- c("t.start","t.end","event","duration")
    pt.dat$duration.end <- pt.dat$t.end+pt.dat$duration
    pt.dat$duration.end <- ifelse(pt.dat$duration.end>=28,28,pt.dat$duration.end)
    pt.dat$id <- i
    PT.DAT <- rbind(PT.DAT,pt.dat)
    
    pt.status <- data.frame(time=1:28,status=0)
    for(j in 1:nrow(pt.dat)){
      pt.status[ceiling(pt.dat$t.end[j]):ceiling(pt.dat$duration.end[j]),"status"] <- pt.dat$event[j]
    }
    status.matrix <- rbind(status.matrix,pt.status$status)
  }
  
    

  print(table(status.matrix[,1]))
  sim.dat <- data.frame(id=1:n,icu.los=icu.time.sim,surv.time=death.time.sim,status.matrix)
  print(table(sim.dat$d1))
  
  colnames(sim.dat)[4:31] <- paste0("d",1:28)
  sim.dat$died <-  ifelse(sim.dat$surv.time<28,1,0)
  #  print(table(sim.dat$died))
  sim.dat$icu.los <- ceiling(sim.dat$icu.los)
  sim.dat$surv.time <- ceiling(sim.dat$surv.time)
  sim.dat$number_days_survived_28days <- ifelse(sim.dat$surv.time<=28,sim.dat$surv.time,28)
  
  #  sim.dat$hosp.los <- round(sim.dat$hosp.los,0)
  
  # get 1st coma onset day
  sim.dat$coma.on.set <- apply(status.matrix,1,function(x) grep("^1",x)[1])
  # get 1st delirium on set day
  sim.dat$delirium.on.set <- apply(status.matrix,1,function(x) grep("^2",x)[1])
  

  for(day in c(1:28)){
    ## Situation: no delirium assessment after ICU, truncate at icu discharge & death
    status <- sim.dat[,paste0("d",day)]
    icu <- (day > sim.dat$icu.los) & (day <= sim.dat$number_days_survived_28days)
    #      hosp <- (day > sim.dat$icu.los) & (day >= sim.dat$hosp.los) & (day <= sim.dat$number_days_survived_28days)
    # & (df$died==0 | (df$died==1 & df$number_days_survived_28days==28))
    dead <- (day > sim.dat$number_days_survived_28days) & sim.dat$died==1
    
    # code icu discharge
    status <- ifelse(icu==TRUE,-1,status)
    #      status <- ifelse(hosp==TRUE,-2,status)
    status <- ifelse(dead==TRUE,4,status)
    #    status <- factor(status,levels=c(-2,-1,0,1,2,3,4),labels=c("hosp.discharge","icu.discharge","ICU","coma","delirium","delirium","death"))
    streval <- paste0("sim.dat$d",day," <- status")
    eval(parse(text=streval))
  }

  return(list(sim.dat=sim.dat,PT.DAT=PT.DAT))
}



find.x <- function(data,cov.vec,perfer=1){
  if(length(perfer)!=1 & length(perfer)!=length(cov.vec)){stop("perfer has wrong dimension.")}
  re <- data[,cov.vec]
  no.miss <- apply(re,1,function(x) sum(!is.na(x)))
  if(length(perfer)==1){
    re$perfer <- apply(re,1,function(x) sum(x==perfer,na.rm=T))
    re$perfer <- ifelse(no.miss==0,NA,ifelse(re$perfer>=1,1,re$perfer))
  }else{
    for(i in 1:length(cov.vec)){
      re[,cov.vec[i]] <- ifelse(re[,cov.vec[i]]==perfer[i],1,0)
    }
    re$perfer <- apply(re,1,function(x) sum(x==perfer,na.rm=T))
    re$perfer <- ifelse(no.miss==0,NA,ifelse(re$perfer>=1,1,re$perfer))
  }
  return(re$perfer)
}



simulate11 <- function(n=500,seed=1234,
                       dist.z="lognormal",theta.z=1,
                       alpha.di=-0.01,alpha.m=1, # paramter used to generate frailty term for hazard of death & discharge
                       alpha.c=1.1,alpha.de=1.1,
                       beta.m = 0, # treatment effect on mortality, NULL is for control group
                       beta.di = 0, # treatment effect on discharge, NULL is for control group
                       beta.coma = 0,# treatment effect on coma, NULL is for control group
                       beta.coma.duration = 0,# treatment effect on coma duration, NULL is for control group
                       beta.delirium = 0,# treatment effect on delirium , NULL is for control group
                       beta.delirium.duration = 0, # treatment effect on delirium duration, NULL is for control group
                       par.icu=c(lambda0.icu=1/(13^1.2),v.icu=1.2), 
                       par.death=c(lambda0.m=1/(13^1.2),v.m=1.2),
                       par.death2=c(lambda0.m2=1/(13^1.2),v.m2=1.2),
                       par.delirium=c(lambda0.d=5.2,nu.d=0.82),par.coma=c(lambda0.c=2.81,nu.c=1), # baseline hazard
                       par.delirium.duration=c(lambda0.d.D=5.2,nu.d.D=0.82),par.coma.duration=c(lambda0.c.D=2.81,nu.c.D=1),
                       assess.after.icu=1
){
  
  require(MASS)
  beta.m.x <- gen.lp(n,beta.m)
  beta.di.x <- gen.lp(n,beta.di)
  beta.coma.x <- gen.lp(n,beta.coma)
  beta.coma.duration.x <- ifelse(is.null(beta.coma.duration),0,beta.coma.duration)
  beta.delirium.x <- gen.lp(n,beta.delirium)
  beta.delirium.duration.x <- ifelse(is.null(beta.delirium.duration),0,beta.delirium.duration)
  
  set.seed(seed)
  U.mat1 <- matrix(runif(n),nrow=n,ncol=28)
  U.mat2 <- matrix(runif(n),nrow=n,ncol=28)
  U.mat2.2 <- matrix(runif(n),nrow=n,ncol=28)
  U.mat3 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat4 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat5 <- matrix(runif(n*28),nrow=n,ncol=28)
  U.mat6 <- matrix(runif(n*28),nrow=n,ncol=28)
  #  print(seed)
  # generating the frailty variable z
  dist.z <- match.arg(dist.z, choices=c("gamma", "lognormal"))
  if(length(theta.z)!=1){stop("theta.z has wrong dimension")}
  if(theta.z < 0) {stop("theta.z must be non-negative")}
  if(theta.z!=0){                                       # if theta.z=0 then frailty=1 for all
    if(dist.z=="gamma"){                              # gamma-frailty
      aGamma <- 1/theta.z
      z      <- rgamma(n,shape=aGamma, scale=theta.z)
    } else {                                          # lognormal frailty
      mu.z    <- log(1/sqrt(theta.z + 1))
      sigma.z <- sqrt(log(theta.z+1))
      z       <- exp(rnorm(n, mean = mu.z, sd=sigma.z))
    }
  }
  
  
  lambda0.icu <- par.icu[1]
  nu.icu     <- par.icu[2]
  
  lambda0.m <- par.death[1]
  nu.m     <- par.death[2]
  
  lambda0.m2 <- par.death2[1]
  nu.m2     <- par.death2[2]
  
  icu.time.sim <- death.time.sim <- vector()
  for(i in 1:n){
    U <- U.mat1[i]
    
    # simulate time to icu discharge
    Y.icu <- (-1)*log(U)*exp((-1)*beta.di.x[i])*(z[i]^alpha.di)^(-1)
    t.icu <- ((lambda0.icu)^(-1)*Y.icu)^(1/nu.icu)
    icu.time.sim <- c(icu.time.sim,t.icu)
    
    U <- U.mat2[i]
    # # simulate time to death
    Y.m <- (-1)*log(U)*exp((-1)*beta.m.x[i])*(z[i]^alpha.m)^(-1)
    t.m <- ((lambda0.m)^(-1)*Y.m)^(1/nu.m)
    
    if(t.icu < t.m){
      U <- U.mat2.2[i]
      # # simulate time to death
      Y.m <- (-1)*log(U)*exp((-1)*beta.m.x[i])*(z[i]^alpha.m)^(-1)
      t.m <- ((lambda0.m)^(-1)*Y.m)^(1/nu.m)
      t.m <- t.icu+t.m
    }
    death.time.sim <- c(death.time.sim,t.m)
  }
  
  # hazard for death - assume exponetial dist
  #  haz.m <- haz.death*(z^alpha.m)*exp(beta.m.x)
  #  death.time.sim <- rexp(n,haz.m)
  # print(summary(haz.m))
  #  print(summary(death.time.sim))
  # summary(death.time.sim)
  
  
  # coma & delirium
  if(length(par.coma)!=2){stop("par.rec has wrong dimension")}
  if(length(par.delirium)!=2){stop("par.rec has wrong dimension")}
  lambda0.c <- par.coma[1]
  nu.c     <- par.coma[2]
  lambda0.d <- par.delirium[1]
  nu.d     <- par.delirium[2]
  lambda0.c.D <- par.coma.duration[1]
  nu.c.D     <- par.coma.duration[2]
  lambda0.d.D <- par.delirium.duration[1]
  nu.d.D     <- par.delirium.duration[2]
  
  
  # simulate coma & delirium status and duration
  status.matrix <- vector()
  PT.DAT <- vector()
  #  print(U.mat[100,1])
  for(i in 1:n){
    #    print(i)
    status.c <- status.d <- duration<- 0
    U <- U.mat3[i,1]
    U2 <- U.mat4[i,1]
    
    # simulate time to first coma
    #    Y.c <- (-1)*log(U)*exp((-1)*beta.coma.x[i])
    Y.c <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
    t.c <- ((lambda0.c)^(-1)*Y.c)^(1/nu.c)
    
    
    # simulate time to first delirium
    Y.d <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
    t.d <- ((lambda0.d)^(-1)*Y.d)^(1/nu.d)
    
    pt.dat <- vector()
    if(min(t.c,t.d)<28){
      update <- get.status10(t.c=t.c,t.d=t.d,U= U.mat5[i,1],U2= U.mat6[i,1],z=z[i],t.start=0,
                             duration.lambda,
                             beta.coma.duration.x,lambda0.c.D,nu.c.D,
                             beta.delirium.duration.x,lambda0.d.D,nu.d.D,
                             seed=NULL)
      
      st.day <- update[2];status=update[3];duration=update[4]
      
      pt.dat <- rbind(pt.dat,update)
      
    }else{
      # if the time to first event is larger than 28days, no event
      
      update <- c(0,27.1,0,0)
      pt.dat <- rbind(pt.dat,update)
      st.day <- min(t.c,t.d)
      duration=0
    }
    
    
    
    t.start <- st.day+duration
    
    # recursive step: simulation of N subsequent event times
    while (t.start < 28) {
      
      U  <- U.mat3[i,ceiling(t.start)]
      U2  <- U.mat4[i,ceiling(t.start)]
      if(status==1){
        # if previous event is coma:
        #      time not at risk for coma is duration of previous event + 1day
        #      time not at risk for delirium is duration of previous event
        
        #        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])
        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
        t1.c <- t.start+1
        t.c <- (t1.c + ((Y.c+lambda0.c*(t1.c)^(nu.c))/lambda0.c)^(1/nu.c)-(t1.c))
        # delirium
        Y.d  <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
        t1.d <- t.start
        t.d <- (t1.d + ((Y.d+lambda0.d*(t1.d)^(nu.d))/lambda0.d)^(1/nu.d)-(t1.d))
      }else if(status==2){
        # if previous event is delirium:
        #      time not at risk for coma is duration of previous event + 1 day
        #      time not at risk for delirium is duration of previous event + 1 day
        
        # coma
        Y.c  <- (-1)*log(U)*exp((-1)*beta.coma.x[i])*(z[i]^alpha.c)^(-1)
        t1.c <- t.start+1
        t.c <- (t1.c + ((Y.c+lambda0.c*(t1.c)^(nu.c))/lambda0.c)^(1/nu.c)-(t1.c))
        # delirium
        Y.d  <- (-1)*log(U2)*exp((-1)*beta.delirium.x[i])*(z[i]^alpha.de)^(-1)
        t1.d <- t.start+1
        t.d <- (t1.d + ((Y.d+lambda0.d*(t1.d)^(nu.d))/lambda0.d)^(1/nu.d)-(t1.d))
      }
      
      if(min(t.c,t.d) < 28){
        update <- get.status10(t.c=t.c,t.d=t.d,U= U.mat5[i,1],U2= U.mat6[i,1],z=z[i],t.start,
                               duration.lambda,
                               beta.coma.duration.x,lambda0.c.D,nu.c.D,
                               beta.delirium.duration.x,lambda0.d.D,nu.d.D,
                               seed=NULL)
        
        st.day <- update[2];status=update[3];duration=update[4]
        pt.dat <- rbind(pt.dat,update)
        t.start <- st.day+duration
        
        # if((st.day+duration-1)<= 28){
        #   pt.status[st.day:(st.day+duration-1),"status"] <- rep(status,duration)
        # }else{
        #   pt.status[st.day:28,"status"] <- rep(status,(28-st.day)+1)
        # }
        
      }else{
        
        update <- c(t.start,27.1,0,0)
        pt.dat <- rbind(pt.dat,update)
        t.start=29
      }
    }
    
    
    pt.dat <- as.data.frame(pt.dat)
    colnames(pt.dat) <- c("t.start","t.end","event","duration")
    pt.dat$duration.end <- pt.dat$t.end+pt.dat$duration
    pt.dat$duration.end <- ifelse(pt.dat$duration.end>=28,28,pt.dat$duration.end)
    pt.dat$id <- i
    PT.DAT <- rbind(PT.DAT,pt.dat)
    
    pt.status <- data.frame(time=1:28,status=0)
    for(j in 1:nrow(pt.dat)){
      pt.status[ceiling(pt.dat$t.end[j]):ceiling(pt.dat$duration.end[j]),"status"] <- pt.dat$event[j]
    }
    status.matrix <- rbind(status.matrix,pt.status$status)
  }
  
  
  
  print(table(status.matrix[,1]))
  sim.dat <- data.frame(id=1:n,icu.los=icu.time.sim,surv.time=death.time.sim,status.matrix)
  print(table(sim.dat$d1))
  
  colnames(sim.dat)[4:31] <- paste0("d",1:28)
  sim.dat$died <-  ifelse(sim.dat$surv.time<28,1,0)
  #  print(table(sim.dat$died))
  sim.dat$icu.los <- ceiling(sim.dat$icu.los)
  sim.dat$surv.time <- ceiling(sim.dat$surv.time)
  sim.dat$number_days_survived_28days <- ifelse(sim.dat$surv.time<=28,sim.dat$surv.time,28)
  
  #  sim.dat$hosp.los <- round(sim.dat$hosp.los,0)
  
  # get 1st coma onset day
  sim.dat$coma.on.set <- apply(status.matrix,1,function(x) grep("^1",x)[1])
  # get 1st delirium on set day
  sim.dat$delirium.on.set <- apply(status.matrix,1,function(x) grep("^2",x)[1])
  
  
  for(day in c(1:28)){
    ## Situation: no delirium assessment after ICU, truncate at icu discharge & death
    status <- sim.dat[,paste0("d",day)]
    icu <- (day > sim.dat$icu.los) & (day <= sim.dat$number_days_survived_28days)
    #      hosp <- (day > sim.dat$icu.los) & (day >= sim.dat$hosp.los) & (day <= sim.dat$number_days_survived_28days)
    # & (df$died==0 | (df$died==1 & df$number_days_survived_28days==28))
    dead <- (day > sim.dat$number_days_survived_28days) & sim.dat$died==1
    
    # code icu discharge
    status <- ifelse(icu==TRUE,-1,status)
    #      status <- ifelse(hosp==TRUE,-2,status)
    status <- ifelse(dead==TRUE,4,status)
    #    status <- factor(status,levels=c(-2,-1,0,1,2,3,4),labels=c("hosp.discharge","icu.discharge","ICU","coma","delirium","delirium","death"))
    streval <- paste0("sim.dat$d",day," <- status")
    eval(parse(text=streval))
  }
  
  return(list(sim.dat=sim.dat,PT.DAT=PT.DAT))
}
