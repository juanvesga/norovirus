
# Call SEIAR Noro model using an 
rm(list = ls()) 
#remotes::install_github("mrc-ide/odin.dust", upgrade = FALSE)
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
# Source scripts
vanHoek<-FALSE
source(here("src","setup.model.R"))
source(here("src","model_functions.R"))


#############

seiar <- odin.dust::odin_dust ("src/seiar.age.R")

c_mat<-params$transmission
c_mat[params$infa_id,params$infa_id]<-c_mat[params$infa_id,params$infa_id]*9.4198910#params$und5inf
      
pars = list(
  beta =  0.3447092 ,   # transm coefficient
  repfac = 91.3121471 ,
  w1 = 0.3777385,
  w2= 0.3/12,
  delta = 1/ 24.9989594,
  init  = init,
  mu    = params$mu,
  m = c_mat,
  aging_mat= params$aging_mat, 
  N_age = params$N_age)

filter <- mcstate::particle_filter$new(data_all, model = seiar, n_particles = 10,
                                       compare = compare, index = index)
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
        xlab = "Age", ylab = "Weekly reported cases", las = 1,ylim = c(0,200))
points(t,reported_obs, pch = 19, col = "red")

## Seroprevalence 

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
axis(side=1, at=xtick, labels = c("0_1","1_2","2_3","3_4","5_6","6_7"))
arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
       x1=c(1,2,3,4,5,6), y1=sero$V3,
       code=3, angle=90, length=0.1)
points(sero_obs , pch = 19, col = "red")




mod <- seiar$new(pars, 0, 1)
ntimes<-length(days_vec)/params$dt
tt<-seq(1/params$dt,ntimes,1/params$dt)# output only the day step 
y<- mod$simulate(tt)

i <- mod$info()$index[["time"]]
j <- mod$info()$index[["infections_day"]]
matplot(drop(y[i,, ]), drop(t(y[j, ,])), type = "l", col = "#00000055", lty = 1, las = 1,
        xlab = "Day", ylab = "Cases")

i <- mod$info()$index[["time"]]
j <- mod$info()$index[["reported_wk"]]
matplot(drop(y[i, , ]), drop(t(y[j, , ])), type = "l", col = "#00000055", lty = 1, las = 1,
        xlab = "Day", ylab = "Cases")

i <- mod$info()$index[["time"]]
j <- mod$info()$index[["seroprev"]]
matplot(drop(y[i, , ]), drop(t(y[j, , ])), type = "l", col = "#00000055", lty = 1, las = 1,
        xlab = "Day", ylab = "Cases")


idx<- mod$info()$index

states<-drop(y)
st<-which(days_vec==idd2_startdate)#* 1/params$dt
ed<-which(days_vec==idd2_enddate)# * 1/params$dt
span<-seq(st,ed,1)

person_year<- states[idx$n_risk[1:4],span[length(span)]]/(365/params$dt)
cases<-states[idx$cumu_inc[1:4],span[length(span)]]

rate<- (cases/person_year) *1000
barplot(rate)


## particle filter
filter <- mcstate::particle_filter$new(data_all, model = seiar, n_particles = 10,
                                        compare = compare, index = index)

index(mod$info())

filter$run(pars)

filter$run(pars,save_history = TRUE)


#############
## PMCMC
#############

priors <- list(
  mcstate::pmcmc_parameter("beta", 0.04, min = 0),
  mcstate::pmcmc_parameter("und5inf", 8, min = 1),
  mcstate::pmcmc_parameter("w1", 0.15, min = 0),
  mcstate::pmcmc_parameter("w2", 2/12, min = 0),
  mcstate::pmcmc_parameter("delta", 25, min = 1),
  mcstate::pmcmc_parameter("repfac", 287, min = 1)#, prior = function(p)
    #  dnorm(p, mean = 287, sd = 50, log = TRUE))
  )

# proposal variance covariance matrix
vcv <- diag(0.01, 5)
vcv


#foor complex modles with other non free paraamters
make_transform <- function(params=params,init=init) {
  
  function(theta) {
    
    c_mat<-params$transmission
    
    c_mat[params$infa_id,params$infa_id]<-c_mat[params$infa_id,params$infa_id] *
      theta[["und5inf"]]
 
    list(
      beta  = theta[["beta"]] ,   # transm coefficient
      repfac= theta[["repfac"]],
      w1    = theta[['w1']],
      delta = 1/theta[["delta"]],
      init  = init,
      mu    = params$mu,
      m     = c_mat,
      aging_mat= params$aging_mat, 
      N_age = params$N_age,
      w1 = params$w1)
  }
}
transform <- make_transform(params,init)

#final parameter object
mcmc_pars <- mcstate::pmcmc_parameters$new(priors, vcv, transform)

# Control for PMCMC run
n_steps <- 20000
n_burnin <- 2000
control <- mcstate::pmcmc_control(
  n_steps = n_steps,
  progress = TRUE)

samples <- mcstate::pmcmc(mcmc_pars, filter, control = control)


# plot samples
plot(samples$probabilities[, "log_posterior"], type = "s",
     xlab = "Sample", ylab = "Log posterior",
     xlim = c(0,n_burnin))

processed_chains <- mcstate::pmcmc_thin(samples, burnin = n_burnin, thin = 2)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)
parameter_mean_hpd

mcmc1 <- coda::as.mcmc(cbind(samples$probabilities, samples$pars))
summary(mcmc1)
windows()
plot(mcmc1)

##########
vcv <- matrix(c(0.00057, 0.00052, 0.00052, 0.00057), 2, 2)

ini<-c(priors[[1]]$mean,
       priors[[2]]$mean,
       priors[[3]]$mean,
       priors[[4]]$mean,
       priors[[5]]$mean)*0.12

vcv <- diag(ini, 5)
mcmc_pars <- mcstate::pmcmc_parameters$new(priors, vcv, transform)


control <- mcstate::pmcmc_control(
  n_steps = n_steps,
  n_chains = 4,
  n_threads_total = 12,
  n_workers = 4,
  save_state = TRUE,
  save_trajectories = TRUE,
  progress = TRUE)
samples <- mcstate::pmcmc(mcmc_pars, filter, control = control)

save(samples,file=here("output","samples0503.RData"))

load(here("output","samples0503.RData"))


head(samples$probabilities)

plot(samples$probabilities[, "log_posterior"], type = "s")

processed_chains <- mcstate::pmcmc_thin(samples, burnin = n_burnin, thin = 2)
head(processed_chains$probabilities)
plot(processed_chains$probabilities[, "log_posterior"], type = "s")

head(processed_chains$pars)

parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)
parameter_mean_hpd

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


#######
## Tuning MCMC
#######
coda::effectiveSize(mcmc1)

1 - coda::rejectionRate(mcmc1)

# Use cov matrix of first run
proposal_matrix <- cov(samples$pars)
proposal_matrix<-diag(c(mcmc_pars$initial()*0.2),5)

mcmc_pars <- mcstate::pmcmc_parameters$new(
  priors, proposal_matrix, transform)
proposal_matrix

p<-mcmc_pars$initial()

mcmc_pars$propose(p)

control <- mcstate::pmcmc_control(
  n_steps,
  save_state = TRUE,
  n_threads_total = 12,
  n_workers = 4,
  save_trajectories = TRUE,
  progress = TRUE,
  n_chains = 4)
pmcmc_tuned_run <- mcstate::pmcmc(mcmc_pars, filter, control = control)

mcmc2 <- coda::as.mcmc(cbind(
  pmcmc_tuned_run$probabilities, pmcmc_tuned_run$pars))

summary(mcmc2)
plot(mcmc2)


processed_chains <- mcstate::pmcmc_thin(pmcmc_tuned_run, burnin = n_burnin, thin = 2)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)
parameter_mean_hpd
mcmc2 <- coda::as.mcmc(cbind(
  processed_chains$probabilities, processed_chains$pars))
plot(mcmc2)


coda::effectiveSize(mcmc2)
1 - coda::rejectionRate(mcmc2)

##again
load(here('output','pmcmc_tuned.RData'))
proposal_matrix <- cov(pmcmc_tuned_run$pars)
mcmc_pars <- mcstate::pmcmc_parameters$new(
  priors, proposal_matrix, transform)
proposal_matrix

p<-mcmc_pars$initial()

mcmc_pars$propose(p)

control <- mcstate::pmcmc_control(
  n_steps,
  save_state = TRUE,
  n_threads_total = 12,
  n_workers = 4,
  save_trajectories = TRUE,
  progress = TRUE,
  n_chains = 4)
pmcmc_tuned2 <- mcstate::pmcmc(mcmc_pars, filter, control = control)

plot(pmcmc_tuned2$probabilities[, "log_posterior"], type = "s")

processed_chains <- mcstate::pmcmc_thin(pmcmc_tuned2, burnin = n_burnin, thin = 2)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)
parameter_mean_hpd
mcmc2 <- coda::as.mcmc(cbind(
  processed_chains$probabilities, processed_chains$pars))
plot(mcmc2)


coda::effectiveSize(mcmc2)
1 - coda::rejectionRate(mcmc2)



save(pmcmc_tuned_run,file=here("output","pmcmc_tuned.RData"))











# Run the model for 10 years (in days)
# note dt step

params$beta<-0.042320079  
params$und5inf<-8.66073384
params$alpha<-0.05
params$w1<-0.15
params$rho<-0.05


sim<-run_model(params,ntimes, seiar)


# incidence plot (day)
idx<-sim$idx
y1<-sim$states[idx$infections_day[1],1,]
plot(days_vec,y1,type="l")

states<-sim$states
N<- 
  states[idx$M,1,]+
  states[idx$G,1,]+
  states[idx$S,1,]+
  states[idx$R,1,]

st<-which(days_vec==idd2_startdate)#* 1/params$dt
ed<-which(days_vec==idd2_enddate)# * 1/params$dt
span<-seq(st,ed,1)
cases<-states[idx$cumu_inc,1,span[length(span)]]-states[idx$cumu_inc,1,span[1]]
Num<-c(sum(cases[c(1,5)]),sum(cases[c(6,7)]),sum(cases[c(8,9,10,11,12)]),sum(cases[c(13,14)]))
PY<-(states[idx$M,1,span[1]]+
       states[idx$G,1,span[1]]+
       states[idx$S,1,span[1]]+
       states[idx$R,1,span[1]])

Na<-c(sum(PY[c(1,5)]),sum(PY[c(6,7)]),sum(PY[c(8,9,10,11,12)]),sum(PY[c(13,14)]))

rate<- 1000*((Num/Na))*length(span)/365 

df<-data_iid2.cat4
irate_pyear_4<-data.frame(value=rate, x=levels(df$age_cat), 
                          CI_lower= c(75,32,21,12),
                          CI_upper= c(152,75,60,35)) # cases per 1000 person-years by age 


p2<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
  geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
  theme_bw()+ 
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper),col="#cc0044")+
  geom_point(data=irate_pyear_4, aes(x=x, y=value),shape=21, size=3, fill="white")

gridExtra::grid.arrange(p2)

# incidence plot (week)
# idwk<-seq(0,ntimes, 7/params$dt)
# y2<-sim$states[idx$infections_wk[1],1,idwk]
# x<-sim$states[idx$time,1,idwk]
# plot(days_vec[idwk],y2,type="l")
# 
# y<-sim$states[idx$infections_wk[1],1,]
# x<-sim$states[idx$time,1,]
# plot(x,y,type="l")#,ylim = c(0,10000),xlim=c(100,400))
# 
# y1<-sim$states[idx$infections_tot[1],1,]
# y2<-sim$states[idx$infections_day[1],1,]
# y3<-sim$states[idx$infections_wk[1],1,]
# 
# test<-data.frame(
#   dt=head(y1,30),
#   day=head(y2,30),
#   wk=head(y3,30))

# check population distribution
p<-sim$age.distr.sim
plot_age_distr(p,params)

# reduce to a day step vector 
states<-sim$states

idx<-sim$idx
time<-days_vec


s_mat<- t(states[idx$S_tot, ,])
e_mat<- t(states[idx$E_tot, ,])
i_mat<- t(states[idx$I_tot, ,])
a_mat<- t(states[idx$A_tot, ,])
r_mat<- t(states[idx$R_tot, ,])

## plot SEIAR states 
cols <- c(S = "#8c8cd1", E = "#cc0044", I = "#999996", A = "#669966", R = "#4c8cd1" )
matplot(time, s_mat, type = "l", # Offset to access numbers in age compartment
        xlab = "", ylab = "", yaxt="none",
        col = cols[["S"]], lty = 1, lwd=2,ylim=c(0, max(states[idx$S_tot,, ])))
matlines(time, e_mat, col = cols[["E"]], lty = 1 ,lwd=2)
matlines(time, i_mat, col = cols[["I"]], lty = 1,lwd=2)
matlines(time, a_mat, col = cols[["A"]], lty = 1,lwd=2)
matlines(time, r_mat, col = cols[["R"]], lty = 1,lwd=2)
legend("right", lwd = 2, col = cols, legend = names(cols), bty = "n")
axis(2, las =2)
mtext("Number of individuals", side=2,line=1, outer=T)
mtext("Time (days)", side = 1, line = 0, outer =T)
# Plot I
matplot(time, i_mat, type = "l", # Offset to access numbers in age compartment
        xlab = "", ylab = "", yaxt="none",
        col = cols[["I"]], lty = 1, lwd=2,ylim=c(0, max(states[idx$I_tot,,c(200:length(time)) ])),
        xlim = c(as.Date("2008-03-01"),as.Date("2009-09-01")))
legend("right", lwd = 2, col = cols, legend = names(cols), bty = "n")
axis(2, las =2)
mtext("Number of individuals", side=2,line=1, outer=T)
mtext("Time (days)", side = 1, line = 0, outer =T)



### Plot IID2 data vs model (using fulll age brackets)
states<-sim$states
N<- 
  states[idx$M,1,]+
  states[idx$G,1,]+
  states[idx$S,1,]+
  states[idx$R,1,]

st<-which(days_vec==idd2_startdate)#* 1/params$dt
ed<-which(days_vec==idd2_enddate)# * 1/params$dt
span<-seq(st,ed,1)
cases<-states[idx$cumu_inc,1,span]
Num<-rbind(colSums(cases[c(1,5),]),colSums(cases[c(6,7),]),colSums(cases[c(8,9,10,11,12),]),colSums(cases[c(13,14),]))

cases<-tail(t(cases),1)-head(t(cases),1)
cases.4<-c(sum(cases[c(1,5)]),sum(cases[c(6,7)]),sum(cases[c(8,9,10,11,12)]),sum(cases[c(13,14)]))
#            c(1,2,3,4,5,6,15,25,35,45,55,65,75,100)

PY<-(states[idx$M,1,span]+
       states[idx$G,1,span]+
       states[idx$S,1,span]+
       states[idx$R,1,span])

Na<-rbind(colSums(PY[c(1,5),]),colSums(PY[c(6,7),]),colSums(PY[c(8,9,10,11,12),]),colSums(PY[c(13,14),]))

rate<- t(1000*((Num/Na)/))
rates<-rate[nrow(rate)/2,]
matplot(time[span], rate, type = "l", # Offset to access numbers in age compartment
        xlab = "", ylab = "", yaxt="none",
        col = cols[["I"]], lty = 1, lwd=2)



PY<-apply(PY,1,cumsum)
PY<-tail(PY,1)-head(PY,1)
PY<-PY/(365/1)
PY.4<-c(sum(PY[c(1,5)]),sum(PY[c(6,7)]),sum(PY[c(8,9,10,11,12)]),sum(PY[c(13,14)]))

irate<-1000*cases.4/PY.4

##
df<-data_iid2.cat4
irate_pyear_4<-data.frame(value=rates, x=levels(df$age_cat), 
                          CI_lower= c(75,32,21,12),
                          CI_upper= c(152,75,60,35)) # cases per 1000 person-years by age 


p2<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
  geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
  theme_bw()+ 
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper),col="#cc0044")+
  geom_point(data=irate_pyear_4, aes(x=x, y=value),shape=21, size=3, fill="white")

gridExtra::grid.arrange(p2)
# ## estimate simulated cases per person years 
# inc_period<-which(days_vec%in%idd2_fup) # select study dates to match data period
# inc<-rowMeans(states[idx$cumu_inc,,tail(inc_period, n=1)]-states[idx$cumu_inc,,1])
# inc<- c(inc[1:3],sum(inc[c(4,5,6,7,8)]),sum(inc[c(9,10)]))
# 
# person_time<- 
#   states[idx$M,,inc_period]+
#   states[idx$G,,inc_period]+
#   states[idx$S,,inc_period]+
#   states[idx$E,,inc_period]+
#   states[idx$A,,inc_period]+
#   states[idx$R,,inc_period]
# 
# pt<-apply(person_time,c(1,2),cumsum)
# 
# 
# pt<-rowMeans(pt[dim(pt)[1], ,])
# 
# pt<- c(pt[1:3],sum(pt[c(4,5,6,7,8)]),sum(pt[c(9,10)]))/365 # aggregate over age brackets 
# 
# irate_pyear<-data.frame(value=1000*inc/pt, x=levels(df$age_cat)) # cases per 1000 person-years by age 
# 
# 
# p<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
#   geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
#   theme_bw()+ 
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper))+
#   geom_point(data=irate_pyear, aes(x=x, y=value),shape=21, size=3, fill="white")
# 
# gridExtra::grid.arrange(p)
# 
# ### Plot IID2 data vs model (using 0-5 age bracket)
# # data from O'Brien
# df<-data_iid2.cat4
# 
# ## estimate simulated cases per person years 
# inc<-rowMeans(states[idx$cumu_inc,,tail(inc_period, n=1)]-states[idx$cumu_inc,,1])
# inc<- c( sum(inc[c(1,2)]),inc[3],sum(inc[c(4,5,6,7,8)]),sum(inc[c(9,10)]))
# 
# person_time<- 
#   states[idx$M,,inc_period]+
#   states[idx$G,,inc_period]+
#   states[idx$S,,inc_period]+
#   states[idx$E,,inc_period]+
#   states[idx$R,,inc_period]
# 
# pt<-array_cumsum(person_time,3)
# pt<-rowMeans(pt[,,dim(pt)[3]])
# pt<- c(sum(pt[c(1,2)]),pt[3],sum(pt[c(4,5,6,7,8)]),sum(pt[c(9,10)])) # aggregate over age brackets 
# 
# irate_pyear<-data.frame(value=1000*365*inc/pt, x=levels(df$age_cat)) # cases per 1000 person-years by age 
# 
# 
# p<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
#   geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
#   theme_bw()+ 
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper),col="#cc0044")+
#   geom_point(data=irate_pyear, aes(x=x, y=value),shape=21, size=3, fill="white")
# 
# gridExtra::grid.arrange(p)
# 

### Plot seroprevalence among adults 

# data from O'Brien
df<-data_sero09
df$x<-as.Date("2009-06-15")
## estimate simulated cases per person years 
sero_period<-which(days_vec%in%sero09_fup) # select study dates to match data period
st<-which(days_vec==head(sero09_fup,1))* 1/params$dt
ed<-which(days_vec==tail(sero09_fup,1)) * 1/params$dt
span<-seq(st,ed,1)


sample<-(states[idx$M,1,span]+
       states[idx$G,1,span]+
       states[idx$S,1,span]+
       states[idx$E,1,span]+
       states[idx$A,1,span]+
       states[idx$R,1,span])
sample<-colSums(sample[c(4,5,6), ])


posi<-(states[idx$A,1,span]+
           states[idx$R,1,span])
posi<-colSums(posi[c(4,5,6), ])

prev<-100*(posi/sample)
day<-seq(1,length(prev), 1/params$dt)
prev<-prev[day]

prevdf<-data.frame(seroprev=prev, x=sero09_fup) # cases per 1000 person-years by age 


p<-ggplot(df,aes(x=x,y=seroprev*100))+
  geom_point(shape=21, size=3, fill="black")+
  geom_errorbar(position=position_dodge(.9), width=.5, aes(ymin=CI_lower*100, ymax=CI_upper*100))+
  geom_line(data=prevdf,aes(x=x,y=seroprev), col="#ff8000",lwd=2)+
  theme_bw()+ 
  ylim(0, 100)+
  ggtitle("Seroprevalence among adults (15 to 45) in 2008-2010")+
  labs(y="Seroprevalence (%)")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b-%Y",limits=as.Date(c("2008-01-01" ,"2010-12-31" )))

gridExtra::grid.arrange(p)

## Plot population
sample<- 
  states[idx$M,,]+
  states[idx$G,,]+
  states[idx$S,,]+
  states[idx$E,,]+
  states[idx$I,,]+
  states[idx$A,,]+
  states[idx$R,,]
n_age<-as.data.frame(t(sample[,1,]))
names(n_age) <-contact$demography$age.group
n_age$days<-days_vec
n_age.m<-reshape2::melt(n_age, id="days")
head(n_age.m)

n_all<-array_cumsum(sample,1)
n_all<-colMeans(n_all[dim(n_all)[1],,])

df<-data.frame(x=days_vec,y=n_all)

# Plot all population
# p1<-ggplot(df, aes(x=x,y=y))+
#   geom_line(col="purple",linewidth=2)+
#   ylim(0,70e6)+
#   labs(y="Total population")



### Plot population by age group
# p2<-ggplot(data=n_age.m, aes(x=days, y= value, col=variable))+
#   geom_line()
# 
# gridExtra::grid.arrange(p1,p2)


w1<-0.15
w2<-2/12
t<-seq(1,365*3,1)
Z<-1 + w1*cos((2*pi*t)/364 + w2*pi)

plot(t,Z,type="l")


