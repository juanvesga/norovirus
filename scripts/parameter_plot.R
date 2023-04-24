rm(list = ls()) 
#remotes::install_github("mrc-ide/odin.dust", upgrade = FALSE)
#load packages
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lemon)

stochastic=0

scaling_fac=c(
  beta = 1000,
  aduRR = 100,
  delta = 1,
  rho = 1000,
  tau = 10,
  w1 = 100,
  repfac=0.25)


if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det.RData"))  
  
}

nsim <- 500
posteriors<-processed_chains$pars
# Sample
pars <- posteriors[sample(nrow(posteriors), nsim), ]
pars<-as.data.frame(t(t(pars) / scaling_fac))
pars_m<-reshape2::melt(pars)
pars_m$variable<-as.character(pars_m$variable)

windows()
ggplot(pars_m, aes(x=value, fill=variable)) +
  geom_histogram() +
  facet_rep_wrap(~variable, scales="free")
