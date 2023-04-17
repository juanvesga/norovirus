## Plot model fits

posteriors<-processed_chains$trajectories$state

nsim <- 300

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

irates<-1000*(cbind(sims["cases_year1",,t]/pop1,
                    sims["cases_year2",,t]/pop2,
                    sims["cases_year3",,t]/pop3,
                    sims["cases_year4",,t]/pop4))

irate_obs<-c(data$cases_a1[1],data$cases_a2[1],
             data$cases_a3[1],data$cases_a4[1])
windows()
matplot(c(1,2,3,4),t(irates), type = "p", col = "#00000011", 
        xlab = "Age", ylab = "Incidence per 1000", las = 1,ylim=c(0,250),xaxt="n")

xtick<-seq(1, 4, by=1)
axis(side=1, at=xtick, labels = c("0_4","5_14","15_64","65+"))
arrows(x0=c(1,2,3,4), y0=data_iid2.c4$CI_lower, 
       x1=c(1,2,3,4), y1=data_iid2.c4$CI_upper,
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



## Weekly cass reported by UKHSA 0-4
# id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported_04))])
# reported<-sims["reported_wk0_4",,id]
# reported_obs<-data$reported_04[which(!is.na(data$reported_04))]
# t<-seq(1,length(id),by=1)#sims["t",1,id]
# windows()
# matplot(t,t(reported), type = "l", col = "#00000011", 
#         xlab = "week", ylab = "Weekly reported cases 0 to 4", las = 1,ylim = c(0,200))
# points(t,reported_obs, pch = 19, col = "red")

## Weekly cass reported by UKHSA 5-65
# id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported_65))])
# reported<-sims["reported_wk5_65",,id]
# reported_obs<-data$reported_65[which(!is.na(data$reported_65))]
# t<-seq(1,length(id),by=1)#sims["t",1,id]
# windows()
# matplot(t,t(reported), type = "l", col = "#00000011", 
#         xlab = "week", ylab = "Weekly reported cases 5 to 65", las = 1,ylim = c(0,200))
# points(t,reported_obs, pch = 19, col = "red")
# 

## Weekly cass reported by UKHSA 65+
# id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported_65p))])
# reported<-sims["reported_wk65_p",,id]
# reported_obs<-data$reported_65p[which(!is.na(data$reported_65p))]
# t<-seq(1,length(id),by=1)#sims["t",1,id]
# windows()
# matplot(t,t(reported), type = "l", col = "#00000011", 
#         xlab = "week", ylab = "Weekly reported cases 65+", las = 1,ylim = c(0,200))
# points(t,reported_obs, pch = 19, col = "red")



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

# Asymp prevalence


# 
# id<-which(sims["t",1,]%in%
#             data_all$time_end[which(!is.na(data$pcr1))])
# pcr_model<-rbind(
#   sims['pcr_asymp1',,id],
#   sims['pcr_asymp2',,id],
#   sims['pcr_asymp3',,id],
#   sims['pcr_asymp4',,id],
#   sims['pcr_asymp5',,id],
#   sims['pcr_asymp6',,id],
#   sims['pcr_asymp7',,id],
#   sims['pcr_asymp8',,id],
#   sims['pcr_asymp9',,id],
#   sims['pcr_asymp10',,id]
# )
# id<-which(!is.na(data$pcr1))
# pcr_obs<-c(data$pcr1[id],
#            data$pcr2[id],
#            data$pcr3[id],
#            data$pcr4[id],
#            data$pcr5[id],
#            data$pcr6[id],
#            data$pcr7[id],
#            data$pcr8[id],
#            data$pcr9[id],
#            data$pcr10[id]
# )
# windows()
# matplot(c(1,2,3,4,5,6,7,8,9,10),pcr_model, type = "p", col = "#00000011", 
#         xlab = "Age", ylab = "Asymptomatic sheding", las = 1,ylim=c(0,1),xaxt="n")
# xtick<-seq(1, 10, by=1)
# axis(side=1, at=xtick, labels = c("0_1","1_4","5_9","10_19","20_29","30_39",
#                                   "40_49","50_59","60_69","70p" ))
# arrows(x0=c(1,2,3,4,5,6,7,8,9,10), y0=asymp$prev*0.8, 
#        x1=c(1,2,3,4,5,6,7,8,9,10), y1=asymp$prev*1.2,
#        code=3, angle=90, length=0.1)
# points(pcr_obs , pch = 19, col = "red")
# 
