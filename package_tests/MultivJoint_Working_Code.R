library(tidyverse);library(frailtypack)
data(dataMultiv)


multiv0<-  multivPenal(Surv(TIMEGAP,INDICREC)~
	           	cluster(PATIENT)+
	           	event2(INDICMETA)+
	           	terminal(INDICDEATH),
	           formula.Event2=~1,
	           formula.terminalEvent=~1,
	           data=dataMultiv,hazard="Weibull")
multiv0
print(multiv0)
