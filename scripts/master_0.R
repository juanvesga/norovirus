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
# 4 run  pMCMC
source(here("scripts","pMCMC_1.R"))

# or 

## Load pre-saved samples
load(here("output","processed_samples.RData"))

###########
# plot model fits 
source(here("scripts","plot_model_fits.R"))



