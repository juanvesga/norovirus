

#######################################################################
## Set up necessary model structures and load data and parameters
############### Demographic model #######################################
vanHoek<- FALSE

# Mort rates (from ONS life tables)
mort_rates<-c(   #read.csv(here("data",paste("mortality_MLE",".csv",sep = "")))
  0.0039640, #1
  0.0002937, #2
  0.0001537, #3
  0.0001155, #4
  0.0000924, #5
  0.0000924, #6
  0.0000874, #7
  0.0000891, #7-15
  0.0002980, #16-25
  0.0005477, #26-35
  0.0011747, #36-45
  0.0026180, #46-55
  0.0064792, #56-65
  0.0163398) #66-75



# Load polymod

# we use 10 age bands 
ages   <-  c(1,2,3,4,5,6,7,15,25,35,45,55,65,75) # Upper end of age bands
#ages   <-  seq(1,75,1) # Upper end of age bands
age_sq <- ages-1
adults <-  seq(tail(which(ages<=5),1),length(ages),1) 
infa_id<- which(ages<5)
adult_id<-which(ages>=15)
da <- diff(c(0,ages))

aging_vec = c(1/diff(ages), 0)
# find diagonal matrix for aging transitions
aging_mat <- diag(-1/da)
aging_mat[row(aging_mat)-col(aging_mat)==1] <- 1/head(da,-1)
aging_mat[length(ages),length(ages)]<-0 # Last age group aging accounted for with mortality
age.categories <- as.factor(ages)

#Load age contact matrix for Infants in UK according to vanHoek supplement
vh<-read.csv(here("data","vanHoek.csv"), header=TRUE)#, sep=,)
cont_infants<-
  unlist(c(vh[1,c(1,2)], 
           sum(vh[1,c(3,4)]),
           sum(vh[1,c(5,6)]),
           sum(vh[1,c(7,8)]),
           sum(vh[1,c(9,10)]),
           sum(vh[1,c(11,12)]),
           sum(vh[1,c(13,14)]),
           vh[1,c(15,16)]
  ))

cont_w_infants<-
  c(vh[c(1,2),1], 
    sum(vh[c(3,4),1]),
    sum(vh[c(5,6),1]),
    sum(vh[c(7,8),1]),
    sum(vh[c(9,10),1]),
    sum(vh[c(11,12),1]),
    sum(vh[c(13,14),1]),
    vh[c(15,16),1]
  )



data(polymod, package = "socialmixr") # POLYMOD for all other contacts

contact = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))),
  symmetric = TRUE)

contact_holi = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))),
  filter = list(cnt_school=0),
  symmetric = TRUE)

# replace with vanHoek infants matrix

if(vanHoek==TRUE){
  contact$matrix[1,]<-cont_infants
  contact$matrix[,1]<-cont_w_infants
  
  # Make matrix symmetric
  x1<-contact$matrix
  x<-((x1+t(x1))/2)
  contact$matrix<-x
}

pop = rep(contact$demography$population) # Population numbers

# Matrix to input into transmission formula, note is corrected for pop size
transmission <- contact$matrix /
  rep(contact$demography$population, each = ncol(contact$matrix))

transmission_holi <- contact_holi$matrix /
  rep(contact$demography$population, each = ncol(contact$matrix))
# UK holiday schedule 
school_sche<- read.csv(here("data","uk_holidays.csv"), header=TRUE)#, sep=,)

id<-which(school_sche$Date=="28-Feb")
school_year<-school_sche$School
val<-school_year[id]
school_year_leap <-c(school_year[1:id],            # Applying rbind function
                     val,
                     school_year[- (1:id)])

# School year vector accounting for leap years (2004 & 2008)
school_uk<-c(
  school_year, school_year, # 2002, 2003
  school_year_leap,         # 2004
  school_year, school_year, school_year, # 2005-2007
  school_year_leap,         # 2008
  school_year, school_year, school_year, # 2009-2011
  school_year_leap, # 2012
  school_year, school_year, school_year, # 2013-2015
  school_year_leap, # 2016
  school_year, school_year, school_year, # 2017-2019
  school_year_leap, # 2020
  school_year, school_year, school_year, # 2021-2023
  school_year_leap, # 2024
  school_year, school_year, school_year, # 2025-2027
  school_year_leap, # 2028
  school_year, school_year, school_year, # 2029-2031
  school_year_leap, # 2032
  school_year, school_year, school_year, # 2033-2035
  school_year_leap)
#plot(days_vec,school_uk,type = "l", xlim = as.Date(c("2002-01-01","2002-12-31")))
########## Model parameters ##############################################
params<-list(
  
  age_sq=age_sq,  
  aging_mat=aging_mat/365, # get the day step aging
  aging_vec=aging_vec/365,# yearly aging rates 
  age.categories = age.categories,
  contact = contact,
  contact_holi=contact_holi,
  pop = pop,
  transmission =transmission,
  transmission_holi=transmission_holi,
  school_uk=school_uk,
  n_school_steps=length(school_uk),
  N_age = length(ages),
  age_select= c(1,seq(2,length(ages),1)*0),
  delta = 25,  # maternal Ab decay (days)
  epsilon = 1,   # incubation
  theta = 2,   # duration symptoms
  sigma = 15, # duration asymp shedding (days)
  tau   = 5.1 ,#(365*5.1),    # duration immunity
  rho   = 0.05, # rel infect asymptomatic 
  p_nonsecretor=0.2, # Fraction immune genetically
  mu    = mort_rates/365,
  age_beta = 1+(seq(1,length(ages),1)*0),
  adult_id = adult_id,# indices for adult groups 
  infa_id=infa_id,
  aduRR=0.1, # cofactor of infectiousness for adults
  repfac= 287,# factor to amplify reported to community
  adult_beta=1, # adult with adult transmission scalar
  w1 = 0.15, # sesonality (if 0 not seasonal)
  w2 = 2.2/12, # 
  alpha = 1, # relative suscept in R compartment 
  # simulation
  dt=1,
  index_idd2=index_idd2,
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
  ),
  
  vac_camp_cov= c(seq(1,length(ages),1)*0),
  vac_sche_cov= c(seq(1,length(ages),1)*0)
)

# Initial conditions of the model (M,G,S,E,I,A,R) x age cats X 4

#1:3 M,G,S,
#4:7 Ej,Ij,Aj,Rj
#8:10 Ekj,Ikj,Akj
#11:14 Ejk,Ijk,Ajk,Rjk
#15:18 Ek,Ik,Ak,Rk
#19:22 El,Il,Al,Rl
#23:25 Eml,Iml,Aml
#26:29 Elm,Ilm,Alm,Rlm
#30:33 Em,Im,Am,Rm

init<-matrix(0, nrow = 114, ncol = length(ages))
init[2,]<-round(params$pop*params$p_nonsecretor) # G
init[3,]<-params$pop - round(params$pop*params$p_nonsecretor)
init[5,5]<-1 # Seed in 15yrs old in Ij
init[9,5]<-1 # Seed in 15yrs old in Ik
init[13,5]<-1 # Seed in 15yrs old in Il
init[17,5]<-1 # Seed in 15yrs old in Im
#init[7,1]<-500


## Clear space
rm(contact, 
   contact_holi, 
   vh,
   transmission,
   transmission_holi,
   fulldata_iid2,
   #   data_iid2.c4,
   #   sgss,
   #   sero,
   asymp,
   synth,
   agg_synth,
   d,
   newd,
   mort_rates,
   polymod,
   aging)#,
#school_sche)



