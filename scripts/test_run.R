rm(list = ls()) 
#remotes::install_github("mrc-ide/odin.dust", upgrade = FALSE)
#load packages
library(odin.dust)
library(here)
library(socialmixr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmatplot)
library(profvis)
library(lubridate)
#################################
# 1 Source scripts
vanHoek<-FALSE
source(here("scripts","setup.model.R"))
source(here("src","model_functions.R"))


#############
# 2 Create dust object 
seiar <- odin.dust::odin_dust("src/seiar.age.R")

#############
# 3 Create filter 
filter <- mcstate::particle_filter$new(data_all, model = seiar, n_particles = 10,
                                       compare = compare, index = index)

###########

c_mat<-params$transmission
c_mat2<-params$transmission_holi

c_mat[params$infa_id,params$infa_id]<-c_mat[params$infa_id,params$infa_id]*4.67554665 
c_mat2[params$infa_id,params$infa_id]<-c_mat2[params$infa_id,params$infa_id]*4.67554665 

pars<-list(
  beta  = 0.059,   # transm coefficient
  repfac= 142 ,
  w1    = 0.092,
  w2    = 1.09,
  delta = 1/365,
  tau   = 6,
  init  = init,
  mu    = params$mu,
  m     = c_mat,
  m_holi= c_mat2,
  school= as.double(params$school_uk),
  aging_mat= params$aging_mat, 
  N_age = params$N_age)


filter$run(pars, save_history = TRUE)

sim<-filter$history()

## Community incidence (IID2)
t<-which(sim["t",1,]%in%
           data_all$time_end[which(!is.na(data_all$cases_a1))])

irates<-1000*(cbind(sim["cumu_inc1",,t],sim["cumu_inc2",,t],sim["cumu_inc3",,t],sim["cumu_inc4",,t])/
                (cbind(sim["n_age1",,t],sim["n_age2",,t],sim["n_age3",,t],sim["n_age4",,t])/365))

irate_obs<-c(data_all$cases_a1[1],data_all$cases_a2[1],
             data_all$cases_a3[1],data_all$cases_a4[1])
matplot(c(1,2,3,4),t(irates), type = "p", col = "#00000011", 
        xlab = "Age", ylab = "Incidence per 1000", las = 1,ylim=c(0,250),xaxt="n")
xtick<-seq(1, 4, by=1)
axis(side=1, at=xtick, labels = c("0_4","5_14","15_64","65+"))
arrows(x0=c(1,2,3,4), y0=data_iid2.c4$CI_lower, 
       x1=c(1,2,3,4), y1=data_iid2.c4$CI_upper,
       code=3, angle=90, length=0.1)
points(irate_obs , pch = 19, col = "red")



## Weekly cass reported by UKHSA
id<-which(sim["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported))])
reported<-sim["reported_wk",,id]
reported_obs<-data_all$reported[which(!is.na(data_all$reported))]
t<-sim["t",1,id]
matplot(t,t(reported), type = "l", col = "#00000011", 
        xlab = "Age", ylab = "Weekly reported cases", las = 1)
points(t,reported_obs, pch = 19, col = "red")




id<-which(sim["t",1,]%in%
            data_all$time_end[which(!is.na(data_all$sero1))])
sero_model<-rbind(
  sim['seroprev1.2',,id],
  sim['seroprev2.3',,id],
  sim['seroprev3.4',,id],
  sim['seroprev4.5',,id],
  sim['seroprev5.6',,id],
  sim['seroprev6.7',,id]
)
id<-which(!is.na(data_all$sero1))
sero_obs<-c(data_all$sero1[id],
            data_all$sero2[id],
            data_all$sero3[id],
            data_all$sero4[id],
            data_all$sero5[id],
            data_all$sero6[id])

matplot(c(1,2,3,4,5,6),sero_model, type = "p", col = "#00000011", 
        xlab = "Age", ylab = "Seropositivity", las = 1,ylim=c(0,1),xaxt="n")
xtick<-seq(1, 6, by=1)
axis(side=1, at=xtick, labels = c("1_2","2_3","3_4","4_5","5_6","6_7"))
arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
       x1=c(1,2,3,4,5,6), y1=sero$V3,
       code=3, angle=90, length=0.1)
points(sero_obs , pch = 19, col = "red")







