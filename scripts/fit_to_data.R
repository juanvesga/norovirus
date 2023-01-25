
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

# Run the model for 10 years (in days)
ntimes<-length(days_vec)/model.params$dt# note dt step

model.params$beta<-0.035
model.params$und5inf<-5
#####


loglikelihood <- function(thetas,label=names){
  llk<-0
  if (dim(thetas)[2] == 1) {
    thetas <- as.data.frame(t(thetas))
    colnames(thetas) <- label
  }
  
  thetas<-bind_rows(thetas)
  if ( any(thetas) < 0){
    llk<- 1000
  }else{
    out<-get_output(thetas,model.params,ntimes,model.seiar)
    llk <- sum(dpois(
      x = round(data_iid2$per1000personyears),
      lambda = out$irate_pyear,
      log = TRUE
    ), na.rm = FALSE)*-1
  }
  return(llk)
}

theta<-data.frame(
  beta=0.035,
  und5inf=5
)
names <- colnames(theta)

loglikelihood(theta)

init.theta<-c(
  beta=0.035,
  und5inf=5
)

fminsearch <- neldermead::fminsearch
opt <- neldermead::optimset(TolX = 1.e-2,  Display = "iter", MaxFunEvals =500)
x1 <- fminsearch(fun = loglikelihood,  
                 x0 = init.theta,
                 # xmin =c(1,0.0001,2),
                 # xmax = c(10,1,5),
                 options = opt
)

x  <- c(x1$optbase$xopt)

write.csv(x, here("data",paste("simplex_MLE",".csv",sep = "")))


#######################################
### Run results
model.params$beta<-x[1]
model.params$und5inf<-x[2]

sim<-run_model(model.params,ntimes, model.seiar)

# check population distribution
p<-sim$age.distr.sim
plot_age_distr(p,model.params)

# reduce to a day step vector 
idday<-seq(1,ntimes, 1/model.params$dt)
states<-sim$states[,,idday]

idx<-sim$idx
time<-days_vec


s_mat<- t(states[idx$S_tot, ,])
e_mat<- t(states[idx$E_tot, ,])
i_mat<- t(states[idx$I_tot, ,])
a_mat<- t(states[idx$A_tot, ,])
r_mat<- t(states[idx$R_tot, ,])

# plot SEIAR states 
cols <- c(S = "#8c8cd1", E = "#cc0044", I = "#999996", A = "#669966", R = "#4c8cd1" )
matplot(time, s_mat, type = "l", # Offset to access numbers in age compartment
        xlab = "", ylab = "", yaxt="none",
        col = cols[["S"]], lty = 1, lwd=2,ylim=c(0, max(states[idx$S_tot,, ])))
matlines(time, e_mat, col = cols[["E"]], lty = 1 ,lwd=2)
matlines(time, i_mat, col = cols[["I"]], lty = 1,lwd=2)
matlines(time, a_mat, col = cols[["A"]], lty = 1,lwd=2)
matlines(time, r_mat, col = cols[["R"]], lty = 1,lwd=2)
legend("right", lwd = 2, col = cols, legend = names(cols), bty = "n")
axis(2, las =2)
mtext("Number of individuals", side=2,line=1, outer=T)
mtext("Time (days)", side = 1, line = 0, outer =T)


### Plot IID2 data vs model (using fulll age brackets)
# data from O'Brien
df<-data_iid2

## estimate simulated cases per person years 
inc_period<-which(days_vec%in%idd2_fup) # select study dates to match data period
inc<-rowMeans(states[idx$cumu_inc,,tail(inc_period, n=1)]-states[idx$cumu_inc,,1])
inc<- c(inc[1:3],sum(inc[c(4,5,6,7,8)]),sum(inc[c(9,10)]))

person_time<- 
  states[idx$M,,inc_period]+
  states[idx$G,,inc_period]+
  states[idx$S,,inc_period]+
  states[idx$E,,inc_period]+
  states[idx$R,,inc_period]

pt<-array_cumsum(person_time,3)
pt<-rowMeans(pt[,,dim(pt)[3]])
pt<- c(pt[1:3],sum(pt[c(4,5,6,7,8)]),sum(pt[c(9,10)])) # aggregate over age brackets 

irate_pyear<-data.frame(value=1000*365*inc/pt, x=levels(df$age_cat)) # cases per 1000 person-years by age 


p<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
  geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
  theme_bw()+ 
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper))+
  geom_point(data=irate_pyear, aes(x=x, y=value),shape=21, size=3, fill="white")

gridExtra::grid.arrange(p)

### Plot IID2 data vs model (using 0-5 age bracket)
# data from O'Brien
df<-data_iid2.cat4

## estimate simulated cases per person years 
inc<-rowMeans(states[idx$cumu_inc,,tail(inc_period, n=1)]-states[idx$cumu_inc,,1])
inc<- c( sum(inc[c(1,2)]),inc[3],sum(inc[c(4,5,6,7,8)]),sum(inc[c(9,10)]))

person_time<- 
  states[idx$M,,inc_period]+
  states[idx$G,,inc_period]+
  states[idx$S,,inc_period]+
  states[idx$E,,inc_period]+
  states[idx$R,,inc_period]

pt<-array_cumsum(person_time,3)
pt<-rowMeans(pt[,,dim(pt)[3]])
pt<- c(sum(pt[c(1,2)]),pt[3],sum(pt[c(4,5,6,7,8)]),sum(pt[c(9,10)])) # aggregate over age brackets 

irate_pyear<-data.frame(value=1000*365*inc/pt, x=levels(df$age_cat)) # cases per 1000 person-years by age 


p<-ggplot(df,aes(x=age_cat, y=per1000personyears))+
  geom_bar(stat = "identity", col="#cc0044", fill="#cc0044")+
  theme_bw()+ 
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=CI_lower, ymax=CI_upper),col="#cc0044")+
  geom_point(data=irate_pyear, aes(x=x, y=value),shape=21, size=3, fill="white")

gridExtra::grid.arrange(p)


### Plot seroprevalence among adults 

# data from O'Brien
df<-data_sero09
df$x<-as.Date("2009-06-15")
## estimate simulated cases per person years 
sero_period<-which(days_vec%in%sero09_fup) # select study dates to match data period

sample<- 
  states[idx$M,,sero_period]+
  states[idx$G,,sero_period]+
  states[idx$S,,sero_period]+
  states[idx$E,,sero_period]+
  states[idx$R,,sero_period]
sample<-sample[c(4,5,6), , ]
n<-array_cumsum(sample,1)
n<-colMeans(n[dim(n)[1],,])

posi<-  states[idx$R,,sero_period]
posi<-posi[c(4,5,6), , ]
pos<-array_cumsum(posi,1)
pos<-colMeans(pos[dim(pos)[1],,])



prev<-data.frame(seroprev=100*(pos/n), x=sero09_fup) # cases per 1000 person-years by age 


p<-ggplot(df,aes(x=x,y=seroprev*100))+
  geom_point(shape=21, size=3, fill="black")+
  geom_errorbar(position=position_dodge(.9), width=.5, aes(ymin=CI_lower*100, ymax=CI_upper*100))+
  geom_line(data=prev,aes(x=x,y=seroprev), col="#ff8000",lwd=2)+
  theme_bw()+ 
  ylim(0, 100)+
  ggtitle("Seroprevalence among adults (15 to 45) in 2008-2010")+
  labs(y="Seroprevalence (%)")+
  scale_x_date(date_breaks = "3 months",date_labels = "%b-%Y",limits=as.Date(c("2008-01-01" ,"2010-12-31" )))

gridExtra::grid.arrange(p)






