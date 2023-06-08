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
model_path<-here("src","seiar.age.2strain_alternative.R")
seiar <- odin.dust::odin_dust(model_path)

#############
# 3 Create filter
if (stochastic==1){
  filter <- mcstate::particle_filter$new(data_all, 
                                         model = seiar, 
                                         n_particles = 11,
                                         compare = compare, 
                                         index = index)
  }else{
  
  filter <- mcstate::particle_deterministic$new(data_all, 
                                                model = seiar, 
                                                compare = compare, 
                                                index = index)
  
  
  filter2 <- mcstate::particle_deterministic$new(data=data_empty,
                                                model = seiar,
                                                compare=compare_empty,
                                                index = index)
  
}


###########
# 4 run  pMCMC
source(here("scripts","pMCMC_1.R"))





## Load pre-saved samples
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det_full.RData"))  
  
}




###########
# 5 plot model fits 
source(here("scripts","plot_model_fits.R"))


###########
# 6 plot model fits 
source(here("scripts","parameter_plot.R"))


###########
# 7 plot model dynamics 
source(here("scripts","plot_dynamics.R"))

