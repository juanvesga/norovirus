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
scaling_fac<-list(
  beta = 1000,
  aduRR = 100,
  delta = 1,
  rho = 1000,
  tau = 10,
  w1 = 100,
  repfac=0.25
)


c1<-params$transmission
c2<-params$transmission_holi 
id<-params$adult_id
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_mat<-params$aging_mat
transform <- make_transform(c1,
                            c2, 
                            id, 
                            mu, 
                            school,
                            n_school_steps,
                            n_age,
                            aging_mat,
                            init,
                            pop=params$pop,
                            scaling_fac = scaling_fac)




init.theta<-c(
  beta = 0.2*1000,
  aduRR = 0.5*100,
  delta = 79*1,
  rho = 0.05*1000,
  tau = 5*10,
  w1 = 0.15*100,
  repfac=278*0.25)

theta_min<- c(
  beta = 0.01,
  aduRR = 0,
  delta = 1,
  rho = 0.001,
  tau = 0.1,
  w1 = 0.01,
  repfac =1
)
theta_min<-theta_min*unlist(scaling_fac)

tags<-labels(init.theta)

loglikelihood <- function(theta, foo=transform,llk=filter, labs=tags, mins=theta_min){
  
  
  if (sum(theta<mins)>0){
    Inf
  }else{
    
    names(theta)<-labs
    pars<-foo(theta)
    filter$run(pars)*-1
    }
}
# 


fminsearch <- neldermead::fminsearch
opt <- neldermead::optimset(TolX = 1.e-2,  Display = "iter", MaxFunEvals =1000)
x1 <- fminsearch(fun = loglikelihood,
                 x0 = xx,#init.theta,
                 # xmin =c(1,0.0001,2),
                 # xmax = c(10,1,5),
                 options = opt
)
xx  <- c(x1$optbase$xopt)

write.csv(xx, here("output",paste("simplex_set",".csv",sep = "")))

xx/unlist(scaling_fac)
