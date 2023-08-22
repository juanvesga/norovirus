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

source(here("scripts","load_data.R"))
source(here("scripts","setup.model.R"))
source(here("src","model_functions.R"))

stochastic <- 0


#############
# 2 Create dust object 
#model_path<-here("src","seiar.age.2strain_alternative.R")
model_path<-here("src","seiar.age.4strain.imm.R")
seiar <- odin.dust::odin_dust(model_path)



## Load pre-saved samples
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det_4str_simplex.RData")) 
}




bestllk<-which(
  processed_chains$probabilities[, "log_posterior"] > -9500) 

sample_pars<-processed_chains$pars[bestllk,]
sample_state<- processed_chains$state[,bestllk]
nsim <- 100
ids<-sample(nrow(sample_pars),nsim)
pars<-sample_pars[ids,]
inits<-sample_state[,ids]
dim(inits)
# Sample


c1<-params$transmission
c2<-params$transmission_holi 
aduid<-params$adult_id 
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_vec<-params$aging_vec
scalefc<-params$scaling_fac
pop<-params$pop


# Baseline
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

base<-run_vaccine_scenario(pars, inits, footransform)
save(base, file = here("output","baseline.RData"))


### schedule 1yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_sche_cov[1]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

sche_1yr<-run_vaccine_scenario(pars, inits, footransform)
save(sche_1yr, file = here("output","sche_1yr.RData"))


### schedule 65yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_sche_cov[14]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

sche_65yr<-run_vaccine_scenario(pars, inits, footransform)
save(sche_65yr, file = here("output","sche_65yr.RData"))


### schedule 1 & 65yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_sche_cov[c(1,14)]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

sche_1_65yr<-run_vaccine_scenario(pars, inits, footransform)
save(sche_1_65yr, file = here("output","sche_1_65yr.RData"))


### Campaign 5yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_camp_cov[c(1,2,3,4,5)]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

camp_5yr<-run_vaccine_scenario(pars, inits, footransform)
save(camp_5yr, file = here("output","camp_5yr.RData"))


### camp 65yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_camp_cov[14]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

camp_65yr<-run_vaccine_scenario(pars, inits, footransform)
save(camp_65yr, file = here("output","camp_65yr.RData"))


### camp 1 & 65yr 
#########
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov
vac_camp_cov[c(1,2,3,4,5,14)]<- 1

footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)

camp_5_65yr<-run_vaccine_scenario(pars, inits, footransform)
save(camp_5_65yr, file = here("output","camp_5_65yr.RData"))
















matplot(tt, t(base$all_inf_day), type = "l", lty = 1, col = "#00000022",
        xlab = "Day", ylab = "Number infected (I)")
lines(tt, rowMeans(base$all_inf_day), col = "red", lwd = 1)

cumm<-cumsum(base$all_inf_day[1,])
plot(tt,cumsum(base$all_inf_day[1,]),type = "l",xlim = c(0,365))


plot(tt,sim[id$inc_day_gii4[1],],type="l")


plot(tt,sim[id$vaccines_perday,],type="l")



matplot(tt,t(sim[id$inc_day_gii4,]),type="l")
