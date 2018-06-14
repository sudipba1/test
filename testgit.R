library(activity)
setwd("C:/Users/HP/Dropbox/Blender_processing/25_5_18")
source("traprate_code.r")
source("speedCode Apr17.r")
source("detectfunc code.r")

setwd("C:/Users/HP/Dropbox/Blender_processing/25_5_18") #set working directory
#data

det.dat <-read.csv("CTR_firstdetection_data.csv")
sp <-"Chital"
i <-det.dat$species==sp
amodN0 <-fitdf("angle", data=det.dat[i,])
amodN1 <-fitdf("angle", data=det.dat[i,], order=1)
amodN2 <-fitdf("angle", data=det.dat[i,], order=2)
amodH0 <-fitdf("angle","hazard", data=det.dat[i,])
amodH1 <-fitdf("angle","hazard", data=det.dat[i,], order=1)
amodH2 <-fitdf("angle","hazard", data=det.dat[i,], order=2)
AICtab(amodN0@model, amodN1@model, amodN2@model, amodH0@model, amodH1@model, amodH2@model)
(angle.est <- edest(amodH1))
plot(amodH1, col=2)

rmodN0 <-fitdf("radius", data=det.dat[i,])
rmodN1 <-fitdf("radius", data=det.dat[i,], order=1)
rmodN2 <-fitdf("radius", data=det.dat[i,], order=2)
rmodH0 <-fitdf("radius","hazard", data=det.dat[i,])
rmodH1 <-fitdf("radius","hazard", data=det.dat[i,], order=1)
rmodH2 <-fitdf("radius","hazard", data=det.dat[i,], order=2)
AICtab(rmodN0@model, rmodN1@model, rmodN2@model, rmodH0@model, rmodH1@model, rmodH2@model)
plot(rmodH1, col=2)
plot(rmodH0, col=2)

(radius.est <- edest(rmodH1))

par(mfrow=c(1,2))
plot(rmodH1)
plot(rmodH0)

par(mfrow=c(2,2))
plot(rmodN2, col=2, ylim=c(0,0.5), main="HNormal")
plot(rmodH1, col=2, ylim=c(0,0.5), main="Hazard")


rmod <-fitdf("radius","hazard", data=subset(det.dat, species==sp & radius<12))
plot(rmod)
edest(rmod)

#Activity

act.dat<-read.csv("CTR_activity_data.csv")
summary(act.dat)

j<-act.dat$species==sp

act.est<-fitact(act.dat$time[j], reps=10)
act.est

plot(act.est, lcol=2)




#speed
spd.dat<-read.csv("CTR_speed_data.csv")
summary(spd.dat)

spd.est <-fit.spd(spd.dat$speed~1, subset(spd.dat, species==sp), pdf="all")
spd.est$AICtab
plot(spd.est$models$lnorm)
(speed <- predict.sbm(spd.est$models$lnorm))


#4 Density
trap.dat <-read.csv("CTR_traprate_data_1.csv")
View(trap.dat)
nrow(trap.dat)

param <-list(r=radius.est[1]/1000,
             theta=angle.est[1],
             v=speed$speed*60^2*24/1000,
             p=act.est@act[1]
)

paramSE <-list(r=radius.est[2]/1000,
               theta=angle.est[2],
               v=speed$se*60^2*24/1000,
               p=act.est@act[2]
)


k<-trap.dat$species==sp
with(subset(trap.dat, species==sp), hist(P/T, breaks=seq(0,12,0.1)))
bootTRD(trap.dat$P[k], trap.dat$T[k], param, paramSE)
with(trap.dat, sum(P))
with(trap.dat, sum(T))
