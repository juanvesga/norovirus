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
source(here("src","plot_single_fits.R"))

stochastic <- 0
simplex<-0
#############
# 2 Create dust object 
model_path<-here("src","seiar.age.2strain.R")
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
scaling_fac<-params$scaling_fac


c1<-params$transmission
c2<-params$transmission_holi 
adu_id<-params$adult_id 
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_vec<-params$aging_vec
scalefc<-scaling_fac
pop<-params$pop
footransform <- make_transform(c1,
                               c2, 
                               adu_id, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc)


## Load pre-saved samples
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det.RData"))  
  theta_simplex<-read.csv(here("output","simplex_set.csv"), header=TRUE)#, sep=,)
  
}

id<- which(processed_chains$probabilities[,"log_posterior"] == 
             max(processed_chains$probabilities[,"log_posterior"]))[1]
thetas<-processed_chains$pars


if (simplex==0){
  thetas<-processed_chains$pars
  theta <- c(thetas[id,],aduRR=100)#/unlist(scaling_fac)
}else{
  tmp<-t(theta_simplex$x)
  theta <- c(tmp)#/unlist(scaling_fac))
  names(theta)<-names(scaling_fac)
}



theta_min<- c(
  beta_j = 0,
  beta_k = 0,
  aduRR  = 0,
  delta = 1,
  rho = 0,
  tau = 0.5,
  w1 = 0.01,
  repfac =1
 
)

theta<- c(
  beta_j = 0.5,
  beta_k = 0.5,
  aduRR  = 0.5,
  delta = 365,
  rho = 0.05,
  tau = 5,
  w1 = 0.1,
  repfac =287

)

init.theta<-theta*unlist(scaling_fac)

theta_min<-theta_min*unlist(scaling_fac)

tags<-labels(init.theta)

loglikelihood <- function(theta, foo=footransform,llk=filter, labs=tags, mins=theta_min){
  
  
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
                 x0 = init.theta,
                 # xmin =c(1,0.0001,2),
                 # xmax = c(10,1,5),
                 options = opt
)
xx  <- c(x1$optbase$xopt)

write.csv(xx, here("output",paste("simplex_set2",".csv",sep = "")))

xx/unlist(scaling_fac)
