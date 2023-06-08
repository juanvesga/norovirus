plot_single_fits<-function(sims,data){
  
  
  
  ## Community incidence (IID2)
  t<-which(sims["t",1,]%in%
             data_all$time_end[which(!is.na(data_all$cases_a1))])
  
  
  
  irates <-1000*(cbind(
    (sims['cases_year1_str1',,t]+sims['cases_year1_str2',,t])/(sims['person_year1',,t]/365),
    (sims['cases_year2_str1',,t]+sims['cases_year2_str2',,t])/(sims['person_year2',,t]/365) ,
    (sims['cases_year3_str1',,t]+sims['cases_year3_str2',,t])/(sims['person_year3',,t]/365) ,
    (sims['cases_year4_str1',,t]+sims['cases_year4_str2',,t])/(sims['person_year4',,t]/365) ,
    (sims['cases_year5_str1',,t]+sims['cases_year5_str2',,t])/(sims['person_year5',,t]/365)))
  
  irate_obs<-c(data$cases_a1[1],
               data$cases_a2[1],
               data$cases_a3[1],
               data$cases_a4[1],
               data$cases_a5[1])
  
  
  np<-length(irate_obs)
  xtick<-seq(1, np, by=1)
  
  matplot(xtick,t(irates), pch = 19, col = "black", 
          xlab = "Age", ylab = "Incidence per 1000", las = 1,ylim=c(0,250),xaxt="n")
  
  axis(side=1, at=xtick, labels = c("0_1","1_4","5_14","15_64","65+"))
  arrows(x0=xtick, y0=data_iid2.c4$CI_lower, 
         x1=xtick, y1=data_iid2.c4$CI_upper,
         code=3, angle=90, length=0.1)
  points(irate_obs , pch = 15, col = "red")
  
  
  
  
  
  ## sero 
  id<-which(sims["t",1,]%in%
              data_all$time_end[which(!is.na(data_all$sero1))])
  sero_model<-rbind(
  sims['seroprev_num1.2',,id]/sims['seroprev_den1.2',,id] ,
  sims['seroprev_num2.3',,id]/sims['seroprev_den2.3',,id] ,
  sims['seroprev_num3.4',,id]/sims['seroprev_den3.4',,id] ,
  sims['seroprev_num4.5',,id]/sims['seroprev_den4.5',,id] ,
  sims['seroprev_num5.6',,id]/sims['seroprev_den5.6',,id] ,
  sims['seroprev_num6.7',,id]/sims['seroprev_den6.7',,id] )
  
  id<-which(!is.na(data_all$sero1))
  sero_obs<-c(data_all$sero1[id],
              data_all$sero2[id],
              data_all$sero3[id],
              data_all$sero4[id],
              data_all$sero5[id],
              data_all$sero6[id])
  
  matplot(c(1,2,3,4,5,6),sero_model, pch=19, col = "black", 
          xlab = "Age", ylab = "Seropositivity", las = 1,ylim=c(0,1),xaxt="n")
  xtick<-seq(1, 6, by=1)
  axis(side=1, at=xtick, labels = c("1_2","2_3","3_4","4_5","5_6","6_7"))
  arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
         x1=c(1,2,3,4,5,6), y1=sero$V3,
         code=3, angle=90, length=0.1)
  points(sero_obs , pch = 19, col = "red")
  

  ## Weekly cass reported by UKHSA
  id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported))])
  reported<-sims["reported_wk",,id]
  reported_obs<-data_all$reported[which(!is.na(data_all$reported))]
  t<-sims["t",1,id]
  matplot(t,reported, type = "l", col = "black",  
          xlab = "week", ylab = "Weekly reported cases", las = 1,
          ylim = c(0 , max(reported_obs)))
  points(t, reported_obs, pch = 19, col = "red")
  
  
  
  
  
  # Strains 
  id<-which(sims["t",1,]%in%
              data$time_end[which(!is.na(data$gii4_prev))])
  
  strain_model<-
    (sims['cases_year1_str1',,id]+
       sims['cases_year2_str1',,id]+
       sims['cases_year3_str1',,id]+
       sims['cases_year4_str1',,id]+
       sims['cases_year5_str1',,id])/
    (sims['cases_year1_str1',,id]+
       sims['cases_year2_str1',,id]+
       sims['cases_year3_str1',,id]+
       sims['cases_year4_str1',,id]+
       sims['cases_year5_str1',,id]+
       sims['cases_year1_str2',,id]+
       sims['cases_year2_str2',,id]+
       sims['cases_year3_str2',,id]+
       sims['cases_year4_str2',,id]+
       sims['cases_year5_str2',,id])
  
  
  
  id<-which(!is.na(data$gii4_prev))
  gii4_obs<-c(data$gii4_prev[id])
  
  plot(1, gii4_obs, pch = 15, col = "red", 
       xlab = "All ages", ylab = "Proportion incidence GII.4",
       ylim=c(0,1),
       xlim=c(0.5,1.5),
       xaxt="n",
       cex=2)
  points(1,strain_model , pch = 19,col = "black")
  
  
  
  
}