

#######################################################################
## Set up necessary model structures and load data and parameters
############### Demographic model #######################################

# Mort rates 

mort_rates<-read.csv(here("data","mortrates_ons_yearly.csv"), header=TRUE)

# Load polymod

# we use 10 age bands 
ages   <-  seq(1,75,1) # Upper end of age bands
age_sq <- ages-1
adults <-  seq(tail(which(ages<=5),1),length(ages),1) 
infa_id<- which(ages<5)
adult_id<-which(ages>=15)
da <- diff(c(0,ages))

# find diagonal matrix for aging transitions
aging <- diag(-1/da) 
aging[row(aging)-col(aging) ==1] <- 1/head(da,-1)
aging[length(ages),length(ages)]<-0 # Last age group aging accounted for with mortality
age.categories <- as.factor(ages)


data(polymod, package = "socialmixr") # POLYMOD for all other contacts
contact = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))),
  symmetric = TRUE,
  per.capita = TRUE)

contact_holi = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))),
  filter = list(cnt_school=0),
  symmetric = TRUE,
  per.capita = TRUE)


pop = round(1e5*contact$demography$proportion) # Population numbers

# Matrix to input into transmission formula, note is corrected for pop size
transmission <- contact$matrix.per.capita

transmission_holi <- contact_holi$matrix.per.capita

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
  school_year, school_year, school_year # 2009-2011
)

#plot(days_vec,school_uk,type = "l", xlim = as.Date(c("2002-01-01","2002-12-31")))
########## Model parameters ##############################################
params<-list(
  
  age_sq=age_sq,  
  aging_mat=aging/365, # get the day step aging
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
  beta_j = 0.05,   # transm coefficient strain 1
  beta_k = 0.05,   # transm coefficient strain 2
  gamma_jk = 0.01,  # cross-protection from k to j
  gamma_kj = 0.01,  # cross-protection from j to k
  delta = 25,  # maternal Ab decay (days)
  epsilon = 1,   # incubation
  theta = 2,   # duration symptoms
  sigma = 15, # duration asymp shedding (days)
  tau   = 5.1 ,#(365*5.1),    # duration immunity
  rho   = 0.05, # rel infect asymptomatic 
  p_nonsecretor=0.2, # Fraction immune genetically
  mu    = mort_rates$mu/365,
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
    beta_j = 1000,
    beta_k = 1000,
    delta = 1,
    rho = 1000,
    tau = 10,
    w1 = 100,
    repfac=0.25,
    aduRR=100
  )
)

# Initial conditions of the model (M,G,S,E,I,A,R) x age cats

#1:3 M,G,S,
#4:7 Ej,Ij,Aj,Rj
#8:10 Ekj,Ikj,Akj
#11:14 Ejk,Ijk,Ajk,Rjk
#15:18 Ek,Ik,Ak,Rk

init<-matrix(0, nrow = 18, ncol = length(ages))
init[2,]<-round(params$pop*params$p_nonsecretor) # G
init[3,]<-params$pop - round(params$pop*params$p_nonsecretor)
init[5,5]<-1 # Seed in 15yrs old in Ij
init[16,5]<-1 # Seed in 15yrs old in Ik
#init[7,1]<-500 <- use this to test aging of Ab response


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



