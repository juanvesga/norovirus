
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
#################################
# Source scripts
source(here("src","setup.model.R"))
source(here("src","model_functions.R"))
model.seiar <- odin.dust::odin_dust("src/seiar.age.R")

set.seed(2)

########################################
########################################
##Fine tune age distribution through mortality 

mu0<-c(0.004,0.0001,0.0003,0.006,0.012, 0.019, 0.065, 0.1, 0.4,0.4)*10000


loglikelihood <- function(p){
  
  llk<-0
  if ( prod(p) < 0){
    print(p)
    
    llk<- 1000
    
  }else{
    sim<-run_demog_model(p)
    llk<-sum(dbinom(round(contact$demography$proportion*1000), 
                    1000, 
                    sim$age.distr.sim, log = TRUE))*-1
  }
  return(llk)
  
}



fminsearch <- neldermead::fminsearch
opt <- neldermead::optimset(TolX = 1.e-2,  Display = "iter", MaxFunEvals =500)
x1 <- fminsearch(fun = loglikelihood,  
                 x0 = mu0,
                 # xmin =c(1,0.0001,2),
                 # xmax = c(10,1,5),
                 options = opt
)

x  <- c(x1$optbase$xopt)
mu1<-x/1000
write.csv(x, here("data",paste("mortality_MLE",".csv",sep = "")))

ssi<-run_demog_model(x)
plot_age_distr(ssi$age.distr.sim,model.params)
