require(survival)
require(prodlim)
dat<-read.csv("Furneux_2008.csv",header=T)
km<-survfit(Surv(Time/30.4,Death)~1,data=dat)
print(km)

# Let's do a quick survival analysis using all variables
colnames(dat)
dat$RT<-as.character(dat$RT)
dat$RT[dat$RT==""]<-NA
dat$RT<-factor(dat$RT)

m1<-coxph(Surv(Time/30.4,Death)~RT,data=dat)
summary(m1)
km0 <- prodlim(Hist(Time/30.4,Death)~RT,data=dat)
plot(km0,confint=T,percent=F,atrisk.at=seq(0,24,3),background.horizontal=NULL,
     axis1.at=seq(0,24,3),xlab="Time (Months)",legend=F,logrank=T,
     atrisk.title="No. at Risk",ylab="Survival Fraction",xlim=c(0,24),col=c(1:2),
     marktime=T)
m2<-coxph(Surv(Time/30.4,Death)~RT+log10(CellCycle),data=dat)
summary(m2)
# does it imporve our fit
anova(m1,m2)
# finally:
m3<-coxph(Surv(Time/30.4,Death)~RT*log10(CellCycle),data=dat)
anova(m2,m3)
# and everyone cheers...
# (Note we should look at a calibration plot though to see how well we
# actually describe the data)
# we can plot out the interaction - let's pick a time-point
# of interest say 8 months as that was the median OS
res1<-survfit(m3,newdata = data.frame(CellCycle=seq(7,20,by=1),
                                      RT=rep("Yes",times=14)))
s1<-summary(res1,times=6)

plot(seq(7,20,by=1),s1$surv,type="l",ylim=c(0,1),
     xlab="Cell Cycle (days)",ylab="6-month Survival Probability")
lines(seq(7,20,by=1),s1$lower,lty=2)
lines(seq(7,20,by=1),s1$upper,lty=2)

res2<-survfit(m3,newdata = data.frame(CellCycle=seq(7,20,by=1),
                                      RT=rep("No",times=14)))
s2<-summary(res2,times=6)
lines(seq(7,20,by=1),s2$surv,col=2)
lines(seq(7,20,by=1),s2$lower,col=2,lty=2)
lines(seq(7,20,by=1),s2$upper,col=2,lty=2)

# lets do a calibration plot too
require(rms)
m4<-cph(Surv(Time,Death)~RT*log10(CellCycle),data=dat,
        x=T,y=T,surv = T,time.inc=6*30)
c4<-calibrate(m4,cmethod="KM",u=6*30,m=25)
plot(c4)
