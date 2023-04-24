#######################################################################
## Load and set up Data for calibration 
######################################################

#Simulation duration
sim_startdate<-as.Date("2006-01-01")
sim_enddate  <-as.Date("2019-12-31")
days_vec<-seq(sim_startdate,sim_enddate,"day")


##IID2 Incidence data 
# Dates
idd2_startdate<-as.Date("2008-04-01")
idd2_enddate<-as.Date("2009-08-31")
idd2_fup<- seq(idd2_startdate,idd2_enddate,"day")
index_idd2<-which(days_vec %in% idd2_startdate)


st<-which(days_vec==idd2_startdate)
ed<-which(days_vec==idd2_enddate)
span<-seq(st,ed,1) # time span of idd2 in the sim
iid2_time<-span[ which(span%%365 ==0) ]

fulldata_iid2<- read.csv(here("data","IID2_incidence_OBrien.csv"), header=TRUE)#, sep=,)
fulldata_iid2$ci<-as.numeric(fulldata_iid2$CI_upper-fulldata_iid2$CI_lower)
fulldata_iid2$CI_lower<-as.numeric(fulldata_iid2$CI_lower)
fulldata_iid2$CI_upper<-as.numeric(fulldata_iid2$CI_upper)

# data_iid2<-fulldata_iid2 %>% filter (age_cat!="5-")
# data_iid2$age_cat<- factor(data_iid2$age_cat, levels =data_iid2$age_cat)
# head(data_iid2)

data_iid2.c4<-fulldata_iid2 %>%
  filter (age_cat!="5-") 
  
data_iid2.c4$age_cat<- factor(data_iid2.c4$age_cat, 
                               levels = c("0 to 1","1 to 5","5 to 15","15 to 64","65+"))

data_iid2.c4<-data_iid2.c4[order(data_iid2.c4$age_cat),]

###############SGSS
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

#################### Seroprevalence data Lindesmith et al
sero<- read.csv(here("data","serology_prev.csv"), header=TRUE)#, sep=,)
date_sero<-as.Date("2011-07-01")
day_sero<-which(days_vec%in%date_sero)


#################### AMAR IID1 prev of asymptomatic shedding (PCR+ in stool)  
asymp<- read.csv(here("data","IID1_pcr_asymp_Amar.csv"), header=TRUE)#, sep=,)
date_pcr<-as.Date("2011-07-01")
day_pcr<-which(days_vec%in%date_pcr)


##################################
## Synth Noro Data 

synth<- read.csv(here("data","Synth_Noro_Data.csv"), header=TRUE)#, sep=,)

synth$wk<-synth$Week_Key %% 100 
synth$yr<-(synth$Week_Key - synth$Week_Key %% 100)/100


agg_synth <- synth %>% group_by(yr, wk) %>% 
  summarise(count_cases = sum(Count_CDR_OPIE_ID),
            .groups = 'drop')

agg_synth$date<-as.Date(paste(agg_synth$yr, agg_synth$wk, 1, sep="-"), "%Y-%U-%u")
agg_synth<-agg_synth[-which(is.na(agg_synth$date)==1),]
agg_synth$day<-which(days_vec%in%agg_synth$date)

# 
#  synth_04<-synth %>%
#    filter(nw_age_grp==".[00-04]")
#  
#  synth_5_65<-synth %>%
#    filter(
#        nw_age_grp==".[05-14]"| 
#        nw_age_grp==".[15-24]"|
#        nw_age_grp==".[25-44]"| 
#        nw_age_grp==".[45-64]")
#  
#  synth_65p<-synth %>%
#    filter(nw_age_grp==".[65+")
# 
#  
#  agg_synth_04 <- synth_04 %>% group_by(yr, wk) %>%
#    summarise(count_cases = sum(Count_CDR_OPIE_ID),
#              .groups = 'drop')
# 
#  agg_synth_5_65 <- synth_5_65 %>% group_by(yr, wk) %>%
#    summarise(count_cases = sum(Count_CDR_OPIE_ID),
#              .groups = 'drop')
#  
#  agg_synth_65p <- synth_65p %>% group_by(yr, wk) %>%
#    summarise(count_cases = sum(Count_CDR_OPIE_ID),
#              .groups = 'drop')
# 
# 
# agg_synth_04$date<-as.Date(paste(agg_synth_04$yr, agg_synth_04$wk, 1, sep="-"), "%Y-%U-%u")
# agg_synth_5_65$date<-as.Date(paste(agg_synth_5_65$yr, agg_synth_5_65$wk, 1, sep="-"), "%Y-%U-%u")
# agg_synth_65p$date<-as.Date(paste(agg_synth_65p$yr, agg_synth_65p$wk, 1, sep="-"), "%Y-%U-%u")
# 
# agg_synth_04<-agg_synth_04[-which(is.na(agg_synth_04$date)==1),]
# agg_synth_5_65<-agg_synth_5_65[-which(is.na(agg_synth_5_65$date)==1),]
# agg_synth_65p<-agg_synth_65p[-which(is.na(agg_synth_65p$date)==1),]
# 
# agg_synth_04$day<-which(days_vec%in%agg_synth_04$date)
# agg_synth_5_65$day<-which(days_vec%in%agg_synth_5_65$date)
# agg_synth_65p$day<-which(days_vec%in%agg_synth_65p$date)
# 
# id<-which(agg_synth_5_65$date%in%agg_synth_04$date)
# 
# agg_synth_5_65<-agg_synth_5_65[id,]
# agg_synth_65p<-agg_synth_65p[id,]




## Get all data in mcstate data format

d <- data.frame(
  day =c(iid2_time,agg_synth$day,day_sero),
  cases_a1 = c(round(data_iid2.c4$per1000personyears[1]),rep(NA,nrow(agg_synth)),NA),
  cases_a2 = c(round(data_iid2.c4$per1000personyears[2]),rep(NA,nrow(agg_synth)),NA),
  cases_a3 = c(round(data_iid2.c4$per1000personyears[3]),rep(NA,nrow(agg_synth)),NA),
  cases_a4 = c(round(data_iid2.c4$per1000personyears[4]),rep(NA,nrow(agg_synth)),NA),
  cases_a5 = c(round(data_iid2.c4$per1000personyears[5]),rep(NA,nrow(agg_synth)),NA),
  reported = c(NA,agg_synth$count_cases,NA),
  sero1    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[1]), 
  sero2    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[2]),
  sero3    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[3]),
  sero4    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[4]),
  sero5    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[5]),
  sero6    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[6]))

newd<-d[order(d$day),]

                
data_all<-mcstate::particle_filter_data(newd, "day", 1, 0)




#######################################################################
## Set up necessary model structures and load data and parameters
############### Demographic model #######################################


# Mort rats
mort_rates<-read.csv(here("data",paste("mortality_MLE",".csv",sep = "")))

# Load polymod

# we use 10 age bands 
ages   <-  c(1,2,3,4,5,6,7,15,25,35,45,55,65,75) # Upper end of age bands
#ages   <-  seq(1,75,1) # Upper end of age bands
age_sq <- ages-1
adults <-  seq(tail(which(ages<=5),1),length(ages),1) 
infa_id<- which(ages<5)
adult_id<-which(ages>=15)
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
beta = 0.05,   # transm coefficient
delta = 25,  # maternal Ab decay (days)
epsilon = 1,   # incubation
theta = 2,   # duration symptoms
sigma = 15, # duration asymp shedding (days)
tau   = 5.1 ,#(365*5.1),    # duration immunity
rho   = 0.05, # rel infect asymptomatic 
p_nonsecretor=0.2, # Fraction immune genetically
mu    = mort_rates$x/365,
age_beta = 1+(seq(1,length(ages),1)*0),
adult_id = adult_id,# indices for adult groups 
infa_id=infa_id,
und5inf=8, # cofactor of infectiousness for under 5
repfac= 287,# factor to amplify reported to community
adult_beta=1, # adult with adult transmission scalar
w1 = 0.15, # sesonality (if 0 not seasonal)
w2 = 2.2/12, # 
alpha = 1, # relative suscept in R compartment 
# simulation
dt=1,
index_idd2=index_idd2,
scaling_fac=list(
  beta = 1000,
  aduRR = 100,
  delta = 1,
  rho = 1000,
  tau = 10,
  w1 = 100,
  repfac=0.25
)
)

# Initial conditions of the model (M,G,S,E,I,A,R) x age cats
init<-matrix(0, nrow = 7, ncol = length(ages))
init[2,]<-round(params$pop*params$p_nonsecretor) # G
init[3,]<-params$pop - round(params$pop*params$p_nonsecretor)
init[5,9]<-1 # Seed in 5yrs old in I




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



