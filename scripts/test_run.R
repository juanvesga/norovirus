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
source(here("src","plot_single_fits.R"))

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
                                         n_particles = 10,
                                         compare = compare, 
                                         index = index)
}else{
  
  filter <- mcstate::particle_deterministic$new(data_all, 
                                                model = seiar, 
                                                compare = compare, 
                                                index = index)
}


###########

# ## Load pre-saved samples
# if (stochastic==1){
#   load(here("output","processed_samples.RData")) 
# }else{
#   
#   load(here("output","processed_samples_det.RData"))  
#   theta_simplex<-read.csv(here("output","simplex_set.csv"), header=TRUE)#, sep=,)
#   
# }
# 
# 
# id<- which(processed_chains$probabilities[,"log_posterior"] == 
#              max(processed_chains$probabilities[,"log_posterior"]))[1]
# 
# 
# scaling_fac<-params$scaling_fac
# 
# if (simplex==0){
#   thetas<-processed_chains$pars
#   theta <- thetas[id,]/unlist(scaling_fac)
# }else{
#   tmp<-t(theta_simplex$x)
#   theta <- c(tmp/unlist(scaling_fac))
#   names(theta)<-names(scaling_fac)
#   }
# 
# 
# print(theta)
c_mat<-params$transmission
c_mat2<-params$transmission_holi
  
theta=c(
    beta_j =  0.4 , 
    beta_k =  0.3,   
    aduRR = 0.3 ,
    delta = 170,   
    rho = 0.05  ,
    tau = 2,   
    w1_j = 0.1 ,
    w1_k = 0.1 ,
    repfac=145,
    crossp_j=0.05,
    crossp_k=0.05)

c_mat[adult_id,adult_id]<-c_mat[adult_id,adult_id] * theta[["aduRR"]]
c_mat2[adult_id,adult_id]<-c_mat2[adult_id,adult_id]*  theta[["aduRR"]]


pars<-list(
  beta_j  = theta[['beta_j']],   # transm coefficient
  beta_k  = theta[['beta_k']],   # transm coefficient
  repfac= theta[['repfac']],    # Community/reported ratio
  delta = theta[['delta']],      # maternal AB duration (days)
  tau   = theta[['tau']],       # Overall immunity duration (years)0
  w1 = theta[['w1_j']],
  w1 = theta[['w1_k']],
  rho=  theta[['rho']],
  crossp_j=  theta[['crossp_j']],
  crossp_k=  theta[['crossp_k']],
  pop  = pop,
  init  = init,
  mu    = params$mu,
  m     = c_mat,
  m_holi= c_mat2,
  aging_vec =params$aging_vec,
  school_step= as.double(params$school),
  n_school_steps=params$n_school_steps,
  N_age = params$N_age
) # number of age groups


## Run Filter

filter$run(pars, save_history = TRUE)
sims<-filter$history()
state<-filter$state()
data<-data_all
plot_single_fits(sims,data)





#### Other simulations


reps<-10
model <- seiar$new(pars, 0, reps)
n_times<-length(days_vec)
tt<-seq(1,n_times,1)
sim<- model$simulate(tt)
sim<-drop(sim)




idx<-model$info()$index
prev<-sim[idx$seroprev_num[1:6],1,]/sim[idx$seroprev_den[1:6],1,]
t<-sim[idx$time,1,]
plot(t,prev[1,], type = "l", col = "blue",  
        xlab = "day", ylab = "prevalence", las = 1,
        ylim=c(0,1))
lines(t,prev[2,], col = "red")
lines(t,prev[3,], col = "orange")
lines(t,prev[4,], col = "green")
lines(t,prev[5,], col = "black")
lines(t,prev[6,], col = "purple")
lines(c(7852,7852),c(0,1))





# Incidence
## Weekly cass reported by UKHSA
cols <- c("#8c8cd9", "#e67300", "#d279a6", "#ff4d4d", "#999966",
          "#660000")

idx<-model$info()$index
cases<-sim[idx$infections_day_j,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "#e673001A",  
        xlab = "day", ylab = "incidence", las = 1,
        xlim = c(0,365*2))

idx<-model$info()$index
cases<-sim[idx$infections_day_k,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "#ff4d4d",  
        xlab = "day", ylab = "incidence", las = 1,
        xlim = c(0,365*2))



#cases year
idx<-model$info()$index
cases_yr<-sim[idx$cases_year[1],,]
t<-sim[idx$time,1,]
matplot(t,t(cases_yr), type = "l", col = "#e673001A",  
        xlab = "day", ylab = "incidence", las = 1,
        xlim = c(0,365*3))
lines(c(1095,1095),c(0,1e6))


# SIR
sir_col <- c("#8c8cd9", "#ff4d4d", "#999966","#e67300")
sir_col_transp <- paste0(sir_col, "1A")

M<-data.frame(M= t(colSums(sim[idx$M,,])))
G<-data.frame(G= t(colSums(sim[idx$G,,])))
S<-data.frame(S= t(colSums(sim[idx$S,,])))
E<-data.frame(I= t(colSums(sim[idx$E,,])))
I<-data.frame(I= t(colSums(sim[idx$I,,])))
A<-data.frame(R= t(colSums(sim[idx$A,,])))
R<-data.frame(R= t(colSums(sim[idx$R,,])))

sims<-(cbind(S,I,A,R))

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(t, sims, xlab = "Time", ylab = "Number of individuals",
        type = "l", 
        col = c(rep(sir_col_transp[1], reps),
                rep(sir_col_transp[2], reps),
                rep(sir_col_transp[3], reps),
                rep(sir_col_transp[4], reps)),
        lty = 1,
        xlim = c(0,365*2))
legend("right", lwd = 1, col = sir_col,
       legend = c("S", "I", "A",  "R"), bty = "n")

## Reproductive number
m <- params$contact$matrix # age-structured contact matrix
ngm<-c_mat*0
beta<- theta[['beta']]


# Next Generation matrix
for (i in 1:params$N_age){
  for (j in 1:params$N_age){
    ngm[i,j] <- beta *  m[i, j] * (params$theta + ((params$sigma+params$epsilon) * params$rho))
    
  }
}

r0<-eigen(ngm)
R0<-r0$values[1]

Rt<- R0*(S/(M+G+S+E+I+A+R))

par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
matplot(t, Rt, xlab = "Time", ylab = "Effective Reproductive number",
        type = "l", 
        col = "#6600001A" ,
        lty = 1,
        ylim = c(0,R0))


## Seaonlaity
# School vs data 
x<-seq(1,365,7.2)
length(x)
length(sgss$cases)
aux<-sim[idx$aux,,]*1800
t<-sim[idx$time,1,]
matplot(t,t(aux), type = "l", col = "#e673001A",  
        xlab = "day", 
        ylab = "incidence", 
        las = 1,
        xlim = c(0,365),
        ylim=c(0,260)
)
points(x, sgss$cases)
lines(school_sche$day.no.,school_sche$School*200, col="red")


#
idx<-model$info()$index
cases<-sim[idx$reported_wk,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "#e673001A",  
        xlab = "day", 
        ylab = "incidence", 
        las = 1,
        xlim = c(sgss$day[1],sgss$day[length(sgss$day)]),
        ylim = c(0,max(sgss$cases))
)
points(sgss$day, sgss$cases)




idx<-model$info()$index
cases<-sim[idx$reported_wk0_4,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases under4", 
        las = 1)

idx<-model$info()$index
cases<-sim[idx$reported_wk5_65,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases 5-65", 
        las = 1)

idx<-model$info()$index
cases<-sim[idx$reported_wk65_p,,]
t<-sim[idx$time,1,]
matplot(t,t(cases), type = "l", col = "red",  
        xlab = "day", 
        ylab = "Cases 65p", 
        las = 1)









