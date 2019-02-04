library(survival)
library(survsim)
dist.ev=c("weibull","llogistic","weibull")
anc.ev=c(0.8,0.9,0.820)
beta0.ev=c(3.56,5.94,5.78)
beta=(c(-0.04,-0.02,-0.01))
x=list(c("normal",26,4.5),c("unif",50,75),c("bern",0.25))
data=mult.ev.sim(n=100,foltime=30,dist.ev,anc.ev,beta0.ev,dist.cens="weibull",anc.cens=1,beta0.cens=5.2,z=("unif",0.6,1.4),beta,x,nsit=3)
library(survival)
library(survsim)
sim.data <- mult.ev.sim(n=1000, foltime=3600, dist.ev=c('llogistic','weibull',
                                                        'weibull','weibull'),anc.ev=c(0.69978200185280, 0.79691659193027,
                                                                                      0.82218416457321, 0.85817155198598),beta0.ev=c(5.84298525742252, 5.94362650803287,
                                                                                                                                     5.78182528904637, 5.46865223339755),,anc.cens=1.17783687569519,
                        beta0.cens=7.39773677281100,z=list(c("unif", 0.8,1.2)), beta=list(c(-0.4,-0.5,-0.6,-0.7),

summary(sim.data) 
coxph(Surv(time,status)~x+x.1,data=sim.data)
coxph(Surv(time,status)~x+x.1+strata(ev.num),data=sim.data)