#######################################################################
## Load and set up Data for calibration 
######################################################

##IID2 Incidence data 
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

# Dates
idd2_startdate<-as.Date("2008-04-01")
idd2_enddate<-as.Date("2009-08-01")
idd2_fup<- seq(idd2_startdate,idd2_enddate,"day")

#  Noropatrol pre-2009 sample
sero09_fup<- seq(as.Date("2008-01-01"),as.Date("2010-12-31"),"day")
data_sero09<-data.frame(seroprev=0.564, 
                        sample=101, 
                        positive= 57, 
                        CI_lower= 0.54,
                        CI_upper= 0.58)

sim_startdate<-as.Date("2007-01-01")
sim_enddate  <-as.Date("2013-12-31")
days_vec<-seq(sim_startdate,sim_enddate,"day")

# Demographics
mort_rates<-read.csv(here("data",paste("mortality_MLE",".csv",sep = "")))/1000

#######################################################################
## Set up necessary model structures and load data and parameters
############### Demographic model #######################################

# Load polymod
data(polymod, package = "socialmixr")

# In this example we use 10 age bands 
ages =  c(1,5,15,25,35,45,55,65,75,100) # Upper end of age bands

da <- diff(c(0,ages))

# find diagonal matrix for aging transitions
aging <- diag(-1/da) 
aging[row(aging)-col(aging)==1] <- 1/head(da,-1)
aging[length(ages),length(ages)]<-0 # Last age group aging accounted for with mortality
age.categories <- as.factor(ages)

# Create polymod contact matrix
contact = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))), 
  symmetric = TRUE)

pop = rep(contact$demography$population) # Population numbers (using per 100K)

# Matrix to input into transmission formula, note is corrected for pop size
transmission <- contact$matrix /
  rep(contact$demography$population, each = ncol(contact$matrix))
########## Model parameters ##############################################
model.params<-list(
  
aging_mat=aging/(365), # get the day step aging
age.categories = age.categories,
contact = contact,
pop = pop,
transmission =transmission,
N_age = length(ages),
age_select= c(1,seq(2,length(ages),1)*0),
beta = 0.05,   # transm coefficient
delta = 1/25,  # maternal Ab decay
epsilon = 1,   # incubation
theta = 1/2,   # duration symptoms
sigma = 1/15, # duration asymp shedding
tau   = 1/(5.1*365),#(365*5.1),    # duration immunity
rho   = 0.05, # rel infect asymptomatic 
p_nonsecretor=0.2, # Fraction immune genetically
mu    = mort_rates$x/365,
age_beta = 1+(seq(1,length(ages),1)*0),
und5inf=5, # cofactor of infectiousness for under 5
# simulation
dt=0.25

)


