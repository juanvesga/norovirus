
## Plot model fits

nsim <- 300
posteriors<-processed_chains$trajectories$state
# Sample
sims <- posteriors[,sample(ncol(posteriors), nsim), ]

data<-data_all

## Community incidence (IID2)
t<-which(sims["t",1,]%in%
           data$time_end[which(!is.na(data$cases_a1))])

pop1<-sum(params$pop[1:4])
pop2<-sum(params$pop[5:8])
pop3<-sum(params$pop[9:13])
pop4<-sum(params$pop[14])

irates<-1000*(cbind(sims["cases_year1",,t]/(sims["person_year1",,t]/365),
                    sims["cases_year2",,t]/(sims["person_year2",,t]/365),
                    sims["cases_year3",,t]/(sims["person_year3",,t]/365),
                    sims["cases_year4",,t]/(sims["person_year4",,t]/365),
                    sims["cases_year5",,t]/(sims["person_year5",,t]/365)))

irate_obs<-c(data$cases_a1[1],
             data$cases_a2[1],
             data$cases_a3[1],
             data$cases_a4[1],
             data$cases_a5[1])
np<-length(irate_obs)
xtick<-seq(1, np, by=1)

windows()
matplot(xtick,t(irates), type = "p", col = "#00000011", 
        xlab = "Age", ylab = "Incidence per 1000", las = 1,ylim=c(0,250),xaxt="n")

axis(side=1, at=xtick, labels = c("0_1","1_4","5_14","15_64","65+"))
arrows(x0=xtick, y0=data_iid2.c4$CI_lower, 
       x1=xtick, y1=data_iid2.c4$CI_upper,
       code=3, angle=90, length=0.1)
points(irate_obs , pch = 19, col = "red")


## Weekly cass reported by UKHSA all
id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported))])
reported<-sims["reported_wk",,id]
reported_obs<-data$reported[which(!is.na(data$reported))]
t<-seq(1,length(id),by=1)#sims["t",1,id]
windows()
matplot(t,t(reported), type = "l", col = "#00000011", 
        xlab = "week", ylab = "Weekly reported cases all", las = 1,ylim = c(0,200))
points(t,reported_obs, pch = 19, col = "red")



## Seroprevalence 

id<-which(sims["t",1,]%in%
            data$time_end[which(!is.na(data$sero1))])
sero_model<-rbind(
  sims['seroprev1.2',,id],
  sims['seroprev2.3',,id],
  sims['seroprev3.4',,id],
  sims['seroprev4.5',,id],
  sims['seroprev5.6',,id],
  sims['seroprev6.7',,id]
)
id<-which(!is.na(data$sero1))
sero_obs<-c(data$sero1[id],
            data$sero2[id],
            data$sero3[id],
            data$sero4[id],
            data$sero5[id],
            data$sero6[id])
windows()
matplot(c(1,2,3,4,5,6),sero_model, type = "p", col = "#00000011", 
        xlab = "Age", ylab = "Seropositivity", las = 1,ylim=c(0,1),xaxt="n")
xtick<-seq(1, 6, by=1)
axis(side=1, at=xtick, labels = c("0_1","1_2","2_3","3_4","5_6","6_7"))
arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
       x1=c(1,2,3,4,5,6), y1=sero$V3,
       code=3, angle=90, length=0.1)
points(sero_obs , pch = 19, col = "red")


# 
