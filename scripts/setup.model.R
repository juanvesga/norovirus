#######################################################################
## Load and set up Data for calibration 
######################################################

#Simulation duration
sim_startdate<-as.Date("2002-01-01")
sim_enddate  <-as.Date("2011-12-31")
days_vec<-seq(sim_startdate,sim_enddate,"day")


##IID2 Incidence data 
# Dates
idd2_startdate<-as.Date("2008-04-01")
idd2_enddate<-as.Date("2009-08-31")
idd2_fup<- seq(idd2_startdate,idd2_enddate,"day")

st<-which(days_vec==idd2_startdate)
ed<-which(days_vec==idd2_enddate)
span<-seq(st,ed,1) # time span of idd2 in the sim

fulldata_iid2<- read.csv(here("data","IID2_incidence_OBrien.csv"), header=TRUE)#, sep=,)
fulldata_iid2$ci<-as.numeric(fulldata_iid2$CI_upper-fulldata_iid2$CI_lower)
fulldata_iid2$CI_lower<-as.numeric(fulldata_iid2$CI_lower)
fulldata_iid2$CI_upper<-as.numeric(fulldata_iid2$CI_upper)

data_iid2<-fulldata_iid2 %>% filter (age_cat!="5-")
data_iid2$age_cat<- factor(data_iid2$age_cat, levels =data_iid2$age_cat)
head(data_iid2)

data_iid2.cat4<-fulldata_iid2 %>%
  filter (age_cat!="0 to 1") %>%
  filter (age_cat!="1 to 5")
  
data_iid2.cat4$age_cat<- factor(data_iid2.cat4$age_cat, 
                               levels = c("5-","5 to 15","15 to 64","65+"))

data_iid2.c4<-data_iid2.cat4[order(data_iid2.cat4$age_cat),]

##SGSS
sgss<- read.csv(here("data","sgss_weekly_cases.csv"), header=TRUE)#, sep=,)

sgss<-sgss %>% 
  arrange(week)
sgss_start<-as.Date('2010-07-01')
sgss_end<-as.Date('2011-06-20')

sgss_vec<-seq(ymd(sgss_start), ymd(sgss_end), by='1 week')
wknum<-lubridate::isoweek(sgss_vec)
span_sgss<-which(days_vec%in%sgss_vec)
sgss$cases<-round(sgss$cases)
sgss$day<-span_sgss

## Seroprevalence data
sero<- read.csv(here("data","serology_prev.csv"), header=TRUE)#, sep=,)
date_sero<-as.Date("2011-07-01")
day_sero<-which(days_vec%in%date_sero)

## Get all data in mcstate data format

d <- data.frame(
  day =c(span[length(span)],sgss$day,day_sero),
  cases_a1 = c(round(data_iid2.c4$per1000personyears[1]),rep(NA,nrow(sgss)),NA),
  cases_a2 = c(round(data_iid2.c4$per1000personyears[2]),rep(NA,nrow(sgss)),NA),
  cases_a3 = c(round(data_iid2.c4$per1000personyears[3]),rep(NA,nrow(sgss)),NA),
  cases_a4 = c(round(data_iid2.c4$per1000personyears[4]),rep(NA,nrow(sgss)),NA),
  reported = c(NA,sgss$cases,NA),
  sero1    = c(NA,rep(NA,nrow(sgss)),sero$V1[1]), 
  sero2    = c(NA,rep(NA,nrow(sgss)),sero$V1[2]),
  sero3    = c(NA,rep(NA,nrow(sgss)),sero$V1[3]),
  sero4    = c(NA,rep(NA,nrow(sgss)),sero$V1[4]),
  sero5    = c(NA,rep(NA,nrow(sgss)),sero$V1[5]),
  sero6    = c(NA,rep(NA,nrow(sgss)),sero$V1[6])
  )
                
data_all<-mcstate::particle_filter_data(d, "day", 1, 0)




#######################################################################
## Set up necessary model structures and load data and parameters
############### Demographic model #######################################


# Mort rats
mort_rates<-read.csv(here("data",paste("mortality_MLE",".csv",sep = "")))

# Load polymod

# In this example we use 10 age bands 
ages   <-  c(1,2,3,4,5,6,7,15,25,35,45,55,65,75,100) # Upper end of age bands
adults <-  seq(tail(which(ages<=5),1),length(ages),1) 
infa_id<- which(ages<=5)
da <- diff(c(0,ages))

# find diagonal matrix for aging transitions
aging <- diag(-1/da) 
aging[row(aging)-col(aging)==1] <- 1/head(da,-1)
aging[length(ages),length(ages)]<-0 # Last age group aging accounted for with mortality
age.categories <- as.factor(ages)

# Create polymod contact matrix
 # uk_vh<-readRDS(here("data","uk_vanHoek.rds"))# vanHoek for infants contacts
 #  contact_vh = contact_matrix(
 #    uk_vh, countries = "United Kingdom", 
 #    symmetric = TRUE)
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

# replace with vanHoek infants matrix
if(vanHoek==TRUE){
contact$matrix[1,]<-cont_infants
contact$matrix[,1]<-cont_w_infants

# Make matrix symmetric
x1<-contact$matrix
x<-((x1+t(x1))/2)
contact$matrix<-x
}

pop = rep(contact$demography$population) # Population numbers (using per 100K)

# Matrix to input into transmission formula, note is corrected for pop size
transmission <- contact$matrix /
  rep(contact$demography$population, each = ncol(contact$matrix))



########## Model parameters ##############################################
params<-list(
  
aging_mat=aging/365, # get the day step aging
age.categories = age.categories,
contact = contact,
pop = pop,
transmission =transmission,
N_age = length(ages),
age_select= c(1,seq(2,length(ages),1)*0),
beta = 0.05,   # transm coefficient
delta = 25,  # maternal Ab decay (days)
epsilon = 1,   # incubation
theta = 1/2,   # duration symptoms
sigma = 1/15, # duration asymp shedding
tau   = 1/(5.1*365),#(365*5.1),    # duration immunity
rho   = 0.05, # rel infect asymptomatic 
p_nonsecretor=0.2, # Fraction immune genetically
mu    = mort_rates$x/365,
age_beta = 1+(seq(1,length(ages),1)*0),
adults_id = adults,# indices for adult groups 
infa_id=infa_id,
und5inf=8, # cofactor of infectiousness for under 5
repfac= 287,# factor to amplify reported to community
adult_beta=1, # adult with adult transmission scalar
w1 = 0.15, # sesonality (if 0 not seasonal)
w2 = 2.2/12, # 
alpha = 1, # relative suscept in R compartment 
# simulation
dt=1

)

# Initial conditions of the model (M,G,S,E,I,A,R) x age cats
init<-matrix(0, nrow = 7, ncol = length(ages))
init[2,]<-round(params$pop*params$p_nonsecretor) # G
init[3,]<-params$pop - round(params$pop*params$p_nonsecretor)
init[5,5]<-1 # Seed in 5yrs old in I

