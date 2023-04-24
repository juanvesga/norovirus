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

stochastic <- 0

#############
# 2 Create dust object 
model_path<-here("src","seiar.age.R")
seiar <- odin.dust::odin_dust(model_path)

#############
# 3 Create filter
if (stochastic==1){
  filter <- mcstate::particle_filter$new(data_all, 
                                         model = seiar, 
                                         n_particles = 10,
                                         compare = compare, 
                                         index = index)
}else{
  
  filter <- mcstate::particle_deterministic$new(data_all, 
                                                model = seiar, 
                                                compare = compare, 
                                                index = index)
}
###########

## Load pre-saved samples
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det.RData"))  
  
}


id<- which(processed_chains$probabilities[,"log_posterior"] == 
             max(processed_chains$probabilities[,"log_posterior"]))[1]

thetas<-processed_chains$pars
#heta <- apply(thetas, 2, mean)
scaling_fac<-params$scaling_fac

theta <- thetas[id,]/unlist(scaling_fac)
print(theta)
c_mat<-params$transmission
c_mat2<-params$transmission_holi

c_mat[adult_id,adult_id]<-c_mat[adult_id,adult_id]* theta[["aduRR"]]
c_mat2[adult_id,adult_id]<-c_mat2[adult_id,adult_id]* theta[["aduRR"]]

pars<-list(
  beta  = theta[['beta']],   # transm coefficient
  repfac= theta[['repfac']],    # Community/reported ratio
  delta = theta[['delta']],      # maternal AB duration (days)
  tau   = theta[['tau']],       # Overall immunity duration (years)0
  w1 = theta[['w1']],
  rho=  theta[['rho']],
  pop  = pop,
  init  = init,
  mu    = params$mu,
  m     = c_mat,
  m_holi= c_mat2,
  school_step= as.double(params$school),
  n_school_steps=params$n_school_steps,
  N_age = params$N_age,
  aging_mat = params$aging_mat
  ) # number of age groups



reps<-10
model <- seiar$new(pars, 0, reps)
n_times<-length(days_vec)
tt<-seq(1,n_times,1)
sim<- model$simulate(tt)
sim<-drop(sim)



# Incidence
## Weekly cass reported by UKHSA
cols <- c("#8c8cd9", "#e67300", "#d279a6", "#ff4d4d", "#999966",
                "#660000")

idx<-model$info()$index
cases<-sim[idx$infections_day,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "#e673001A",  
        xlab = "day", ylab = "incidence", las = 1,
        xlim = c(0,365*2))

#cases year
idx<-model$info()$index
cases_yr<-sim[idx$cases_year[1],,]
t<-sim[idx$time,1,]
matplot(t,t(cases_yr), type = "l", col = "#e673001A",  
        xlab = "day", ylab = "incidence", las = 1,
        xlim = c(0,365*3))
lines(c(1095,1095),c(0,1e6))


# SIR
sir_col <- c("#8c8cd9", "#ff4d4d", "#999966","#e67300")
sir_col_transp <- paste0(sir_col, "1A")

M<-data.frame(M= t(colSums(sim[idx$M,,])))
G<-data.frame(G= t(colSums(sim[idx$G,,])))
S<-data.frame(S= t(colSums(sim[idx$S,,])))
E<-data.frame(I= t(colSums(sim[idx$E,,])))
I<-data.frame(I= t(colSums(sim[idx$I,,])))
A<-data.frame(R= t(colSums(sim[idx$A,,])))
R<-data.frame(R= t(colSums(sim[idx$R,,])))

sims<-(cbind(S,I,A,R))

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(t, sims, xlab = "Time", ylab = "Number of individuals",
        type = "l", 
        col = c(rep(sir_col_transp[1], reps),
                rep(sir_col_transp[2], reps),
                rep(sir_col_transp[3], reps),
                rep(sir_col_transp[4], reps)),
                lty = 1,
        xlim = c(0,365*2))
legend("right", lwd = 1, col = sir_col,
       legend = c("S", "I", "A",  "R"), bty = "n")

## Reproductive number
m <- params$contact$matrix # age-structured contact matrix
ngm<-c_mat*0
beta<- theta[['beta']]
  

# Next Generation matrix
for (i in 1:params$N_age){
  for (j in 1:params$N_age){
    ngm[i,j] <- beta *  m[i, j] * (params$theta + ((params$sigma+params$epsilon) * params$rho))
    
  }
}

r0<-eigen(ngm)
R0<-r0$values[1]

Rt<- R0*(S/(M+G+S+E+I+A+R))

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(t, Rt, xlab = "Time", ylab = "Effective Reproductive number",
        type = "l", 
        col = "#6600001A" ,
        lty = 1,
        ylim = c(0,R0))


## Seaonlaity
# School vs data 
x<-seq(1,365,7.2)
length(x)
length(sgss$cases)
aux<-sim[idx$aux,,]*1800
t<-sim[idx$time,1,]
matplot(t,t(aux), type = "l", col = "#e673001A",  
        xlab = "day", 
        ylab = "incidence", 
        las = 1,
        xlim = c(0,365),
        ylim=c(0,260)
)
points(x, sgss$cases)
lines(school_sche$day.no.,school_sche$School*200, col="red")


#
idx<-model$info()$index
cases<-sim[idx$reported_wk,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "#e673001A",  
        xlab = "day", 
        ylab = "incidence", 
        las = 1,
        xlim = c(sgss$day[1],sgss$day[length(sgss$day)]),
        ylim = c(0,max(sgss$cases))
        )
points(sgss$day, sgss$cases)




idx<-model$info()$index
cases<-sim[idx$reported_wk0_4,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases under4", 
        las = 1)

idx<-model$info()$index
cases<-sim[idx$reported_wk5_65,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases 5-65", 
        las = 1)

idx<-model$info()$index
cases<-sim[idx$reported_wk65_p,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases 65p", 
        las = 1)





## Run Filter

filter$run(pars, save_history = TRUE)
sim<-filter$history()


## sero 
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

matplot(c(1,2,3,4,5,6),sero_model, type = "p", col = "#00091169", 
        xlab = "Age", ylab = "Seropositivity", las = 1,ylim=c(0,1),xaxt="n")
xtick<-seq(1, 6, by=1)
axis(side=1, at=xtick, labels = c("1_2","2_3","3_4","4_5","5_6","6_7"))
arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
       x1=c(1,2,3,4,5,6), y1=sero$V3,
       code=3, angle=90, length=0.1)
points(sero_obs , pch = 19, col = "red")





## Community incidence (IID2)
t<-which(sim["t",1,]%in%
           data_all$time_end[which(!is.na(data_all$cases_a1))])


pop1<-sum(params$pop[1:4])
pop2<-sum(params$pop[5:8])
pop3<-sum(params$pop[9:13])
pop4<-sum(params$pop[14])

irates<-1000*(cbind(sim["cases_year1",,t]/pop1,
                    sim["cases_year2",,t]/pop2,
                    sim["cases_year3",,t]/pop3,
                    sim["cases_year4",,t]/pop4))
irate_obs<-c(data_all$cases_a1[1],data_all$cases_a2[1],
             data_all$cases_a3[1],data_all$cases_a4[1])
matplot(c(1,2,3,4),t(irates), type = "p", col =  "#00091169", 
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
matplot(t,t(reported), type = "l", col = "#00091169",  
        xlab = "week", ylab = "Weekly reported cases", las = 1,
        ylim = c(0 , max(reported_obs)))
points(t, reported_obs, pch = 19, col = "red")










