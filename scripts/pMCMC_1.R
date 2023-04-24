#############
## PMCMC
#############
# Controls 
n_steps  <- 5000
n_burnin <- round(n_steps*0.4)
n_out    <- round(n_steps*0.50)
n_thin   <- 10#round((n_steps-n_burnin)/n_out)

# Load previous chains
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det.RData"))  
  
}



id<- which(processed_chains$probabilities[,"log_posterior"] == 
             max(processed_chains$probabilities[,"log_posterior"]))[1]
# 
thetas<-processed_chains$pars

mean_hpd <- apply(thetas, 2, mean)
last_best <- thetas[id,]

# Define  priors 
scaling_fac<-params$scaling_fac

priors <- list(
  mcstate::pmcmc_parameter("beta",
                           initial= last_best[["beta"]],#*scaling_fac[["beta"]], # Transmission per capita
                           min = 0*scaling_fac[["beta"]]),

  mcstate::pmcmc_parameter("aduRR",
                           initial= last_best[["aduRR"]]), # Excess infectiousness in Und 5
                           #min = 0*scaling_fac[["aduRR"]], max = 1*scaling_fac[["aduRR"]]),

  mcstate::pmcmc_parameter("delta",
                           initial= last_best[["delta"]], # Duration of maternal Abs
                           min = 1*scaling_fac[["delta"]], max = 365*4*scaling_fac[["delta"]]),

  mcstate::pmcmc_parameter("rho",
                           initial= last_best[["rho"]],#*scaling_fac[["rho"]], # Duration of maternal Abs
                           min = 0*scaling_fac[["rho"]]),

  #
  mcstate::pmcmc_parameter("tau",
                           initial=last_best[["tau"]],#*scaling_fac[["tau"]], # Duration of immunity
                           min = 0.5*scaling_fac[["tau"]] ),

  mcstate::pmcmc_parameter("w1",
                           initial= last_best[["w1"]],#*scaling_fac[["w1"]], # Seasonality amplitude
                           min = 0.01*scaling_fac[["w1"]]),

  mcstate::pmcmc_parameter("repfac",
                           initial= last_best[["repfac"]],#*scaling_fac[["repfac"]], # Seasonality amplitude
                           min = 1)
)
# priors <- list(
#   mcstate::pmcmc_parameter("beta",
#                            initial= 0.05*scaling_fac[["beta"]], # Transmission per capita
#                            min = 0*scaling_fac[["beta"]]),
# 
#   mcstate::pmcmc_parameter("aduRR",
#                            initial= 0.1*scaling_fac[["aduRR"]], # Excess infectiousness in Und 5
#                            min = 0*scaling_fac[["aduRR"]], max = 1*scaling_fac[["aduRR"]]),
# 
#   mcstate::pmcmc_parameter("delta",
#                            initial= 79*scaling_fac[["delta"]], # Duration of maternal Abs
#                            min = 1*scaling_fac[["delta"]]),
# 
#   mcstate::pmcmc_parameter("rho",
#                            initial= 0.05*scaling_fac[["rho"]], # Duration of maternal Abs
#                            min = 0*scaling_fac[["rho"]]),
# 
#   #
#   mcstate::pmcmc_parameter("tau",
#                            initial=2*scaling_fac[["tau"]], # Duration of immunity
#                            min = 0.5*scaling_fac[["tau"]] ),
# 
#   mcstate::pmcmc_parameter("w1",
#                            initial= 0.15*scaling_fac[["w1"]], # Seasonality amplitude
#                            min = 0.01*scaling_fac[["w1"]]),
# 
#   mcstate::pmcmc_parameter("repfac",
#                            initial= 287*scaling_fac[["repfac"]], # Seasonality amplitude
#                            min = 1)
# )


c1<-params$transmission
c2<-params$transmission_holi 
id<-params$adult_id 
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_mat<-params$aging_mat
scalefc<-params$scaling_fac
pop<-params$pop
footransform <- make_transform(c1,
                            c2, 
                            id, 
                            mu, 
                            school,
                            n_school_steps,
                            n_age,
                            aging_mat,
                            init,
                            pop,
                            scalefc)


# Define covariance matrix at start
ini<-c(priors[[1]]$mean,
       priors[[2]]$mean,
       priors[[3]]$mean,
       priors[[4]]$mean,
       priors[[5]]$mean,
       priors[[6]]$mean,
       priors[[7]]$mean
)*0.12

#colnames(thetas)[2]<-"aduRR"
vcv <-cov(thetas)
#vcv <- diag(ini,7)
# Create pmcmc parameters
mcmc_pars <- mcstate::pmcmc_parameters$new(priors, vcv, transform = footransform)

if (stochastic==1){
  control <- mcstate::pmcmc_control(
    n_steps = n_steps,
    n_chains = 1,
    n_threads_total = 2,
    n_workers = 1,
    save_state = TRUE,
    save_trajectories = TRUE,
    progress = TRUE)
}else{
  control <- mcstate::pmcmc_control(
    n_steps = n_steps,
    n_chains = 3,
    n_threads_total = 15,
    n_workers = 3,
    save_state = TRUE,
    save_trajectories = TRUE,
    progress = TRUE,
    adaptive_proposal = TRUE)
  
}
control_premulti<- mcstate::pmcmc_control(
  n_steps = 20,
  n_chains = 1,
  n_threads_total = 1,
  n_workers = 1,
  save_state = FALSE,
  save_trajectories = FALSE,
  progress = TRUE,
  adaptive_proposal = FALSE)

samples_temp <- mcstate::pmcmc(mcmc_pars, filter, control = control_premulti)
samples <- mcstate::pmcmc(mcmc_pars, filter, control = control)


# Plot initial 
plot(samples$probabilities[, "log_posterior"], type = "l",
     xlab = "Sample", ylab = "Log posterior")


# Process samples
matplot(samples$pars,type = "l" , xlim = c(0,n_steps))


processed_chains <- mcstate::pmcmc_thin(samples, burnin = n_burnin, thin = n_thin)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)

print(parameter_mean_hpd/unlist(scaling_fac))

plot(processed_chains$probabilities[, "log_posterior"], type = "l",
     xlab = "Sample", ylab = "Log posterior")



if (stochastic==1){
  save(processed_chains,file=here("output","processed_samples.RData"))
  
}else{
  
  save(processed_chains,file=here("output","processed_samples_det.RData"))
  
  
}

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


coda::effectiveSize(mcmc1)
1 - coda::rejectionRate(mcmc1)


