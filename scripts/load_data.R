#######################################################################
## Load and set up Data for calibration 
######################################################

#Simulation duration
sim_startdate<-as.Date("1990-01-01")
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


## Harris strains data 
strains<- read.csv(here("data","Harris_IID2_strains.csv"), header=TRUE)#, sep=,)
strains<-strains[strains$genotype!="Mixed",]

GI3.prev<- sum(strains$samples[strains$genotype=="GI.3"])/
  sum(strains$samples)

GI.prev<- (sum(strains$samples[strains$genogroup=="GI"]) - 
  sum(strains$samples[strains$genotype=="GI.3"]))/sum(strains$samples)


GII4.prev<- sum(strains$samples[strains$genotype=="GII.4"])/
  sum(strains$samples)

GII.prev<- (sum(strains$samples[strains$genogroup=="GII"]) - 
              sum(strains$samples[strains$genotype=="GII.4"]))/sum(strains$samples)


## Get all data in mcstate data format

d <- data.frame(
  day =c(iid2_time,agg_synth$day,day_sero),
  cases_a1 = c(round(data_iid2.c4$per1000personyears[1]),rep(NA,nrow(agg_synth)),NA),
  cases_a2 = c(round(data_iid2.c4$per1000personyears[2]),rep(NA,nrow(agg_synth)),NA),
  cases_a3 = c(round(data_iid2.c4$per1000personyears[3]),rep(NA,nrow(agg_synth)),NA),
  cases_a4 = c(round(data_iid2.c4$per1000personyears[4]),rep(NA,nrow(agg_synth)),NA),
  cases_a5 = c(round(data_iid2.c4$per1000personyears[5]),rep(NA,nrow(agg_synth)),NA),
  gi3_prev = c(GI3.prev,rep(NA,nrow(agg_synth)),NA),
  gi_prev  = c(GI.prev,rep(NA,nrow(agg_synth)),NA),
  gii4_prev = c(GII4.prev,rep(NA,nrow(agg_synth)),NA),
  gii_prev = c(GII.prev,rep(NA,nrow(agg_synth)),NA),
  reported = c(NA,agg_synth$count_cases,NA),
  sero1    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[1]), 
  sero2    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[2]),
  sero3    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[3]),
  sero4    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[4]),
  sero5    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[5]),
  sero6    = c(NA,rep(NA,nrow(agg_synth)),sero$V1[6]))

newd<-d[order(d$day),]


data_all<-mcstate::particle_filter_data(newd, "day", 1, 0)

## data Empty : to run full sim
d <- data.frame(
  day =seq(1,length(days_vec)),
  cases = rep(1,length(days_vec))  )

data_empty<-mcstate::particle_filter_data(d, "day", 1, 0)


## Clear space
rm(fulldata_iid2,
   #   data_iid2.c4,
   #   sgss,
   #   sero,
   asymp,
   synth,
   agg_synth,
   d,
   newd)
