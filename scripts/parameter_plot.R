rm(list = ls()) 
#remotes::install_github("mrc-ide/odin.dust", upgrade = FALSE)
#load packages
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lemon)

fake= 0

stochastic=0

scaling_fac=list(
  beta_1 = 1000,
  beta_2 = 1000,
  beta_3 = 1000,
  beta_4 = 1000,
  aduRR=100,
  delta = 1,
  rho = 1000,
  tau = 10,
  w1_1 = 100,
  w1_2 = 100,
  w1_3 = 100,
  w1_4 = 100,
  repfac=0.25,
  crossp_12 = 1000,
  crossp_21 = 1000,
  crossp_34 = 1000,
  crossp_43 = 1000
)

if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det_4str_simplex.RData"))  
  
}

nsim <- 500
posteriors<-processed_chains$pars
llk<-as.data.frame(processed_chains$probabilities)
# Sample
pars <- posteriors[sample(nrow(posteriors), nsim), ]
pars<-as.data.frame(t(t(pars) / unlist(scaling_fac)))
if(fake==1){

  pars$beta_k<-pars$beta_k*0.15  
  
}

pars_m<-reshape2::melt(pars)
colnames(pars_m)<-c("parameter","value")
pars_m$parameter<-as.character(pars_m$parameter)


pars<-ggplot(pars_m, aes(x=value, fill=parameter)) +
  geom_histogram() +
  facet_rep_wrap(~parameter, scales="free")+
  theme(
    axis.text = element_text(colour = "black", size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
gridExtra::grid.arrange(pars)



#####################
posts <- posteriors
posts<-as.data.frame(t(t(posts) / unlist(scaling_fac)))
if(fake==1){
  
  posts$beta_k<-posts$beta_k*0.15  
  
}

x<-seq(1,dim(posts)[1],1)
posts_m<-reshape2::melt(posts)
posts_m$x<-rep(x,dim(posts)[2])
colnames(posts_m)<-c("parameter","value","x")
posts_m$parameter<-as.character(posts_m$parameter)


traces<-ggplot(posts_m, aes(x= x, y=value, col=parameter)) +
  geom_line() +
  facet_rep_wrap(~parameter, scales="free")
gridExtra::grid.arrange(traces)

llk$x<-seq(1,length(llk$log_posterior))
logPost<-ggplot(llk, aes(x=x,y=log_posterior))+
  theme_bw()+
  geom_line(col="red")+
  labs(x = "Iteration", y = "LogPosterior") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_text(colour = "black", size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11), legend.key = element_blank()
  )
gridExtra::grid.arrange(logPost)



