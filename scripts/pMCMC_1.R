#############
## PMCMC
#############




# Define  priors 
scaling_fac<-params$scaling_fac

if (start_from_best){
  
  
  # Load previous chains
  if (stochastic==1){
    load(here("output","processed_samples.RData")) 
  }else{
    
    
    load(here("output","processed_samples_det_4str.RData"))  
    
    xvals<-read.csv( here("output",paste("simplex_set",".csv",sep = "")))
    
  }
  
  id<- which(processed_chains$probabilities[,"log_posterior"] == 
               max(processed_chains$probabilities[,"log_posterior"]))[1]
  thetas<-processed_chains$pars
  
  mean_hpd <- apply(thetas, 2, mean)
  last_best <- thetas[id,]
  
  # last_best2<-xvals$x
  # names(last_best2)<-names(last_best)
  # 
  # last_best<-last_best2
  
  priors <- list(
    mcstate::pmcmc_parameter("beta_1",
                             initial= last_best[["beta_1"]],#0.4*scaling_fac[["beta_1"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_1"]]),
    mcstate::pmcmc_parameter("beta_2",
                             initial= last_best[["beta_2"]],#0.1*scaling_fac[["beta_2"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_2"]]),
    
    mcstate::pmcmc_parameter("beta_3",
                             initial= last_best[["beta_3"]],#0.4*scaling_fac[["beta_1"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_3"]]),
    mcstate::pmcmc_parameter("beta_4",
                             initial= last_best[["beta_4"]],#0.1*scaling_fac[["beta_2"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_4"]]),
    
    mcstate::pmcmc_parameter("aduRR",
                             initial= last_best[["aduRR"]],#0.3*scaling_fac[["aduRR"]], # Excess infectiousness in Und 5
                             min = 0*scaling_fac[["aduRR"]], max = 1*scaling_fac[["aduRR"]]),
    
    mcstate::pmcmc_parameter("delta",
                             initial= last_best[["delta"]],#170*scaling_fac[["delta"]], # Duration of maternal Abs
                             min = 1*scaling_fac[["delta"]], max = 365*scaling_fac[["delta"]] ),
    
    mcstate::pmcmc_parameter("rho",
                             initial= last_best[["rho"]],#0.05*scaling_fac[["rho"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["rho"]],max = 1*scaling_fac[["rho"]]),
    
    mcstate::pmcmc_parameter("tau",
                             initial=last_best[["tau"]],#2*scaling_fac[["tau"]], # Duration of immunity
                             min = 0.5*scaling_fac[["tau"]] ),
    
    mcstate::pmcmc_parameter("w1_1",
                             initial= last_best[["w1_1"]],#0.1*scaling_fac[["w1_1"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_1"]], max = 1*scaling_fac[["w1_1"]]),
    
    mcstate::pmcmc_parameter("w1_2",
                             initial= last_best[["w1_2"]],#0.1*scaling_fac[["w1_2"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_2"]], max = 1*scaling_fac[["w1_2"]]),
    
    mcstate::pmcmc_parameter("w1_3",
                             initial= last_best[["w1_3"]],#0.1*scaling_fac[["w1_1"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_3"]], max = 1*scaling_fac[["w1_3"]]),
    
    mcstate::pmcmc_parameter("w1_4",
                             initial= last_best[["w1_4"]],#0.1*scaling_fac[["w1_2"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_4"]], max = 1*scaling_fac[["w1_4"]]),
    
    
    mcstate::pmcmc_parameter("repfac",
                             initial= last_best[["repfac"]],#287*scaling_fac[["repfac"]], # Seasonality amplitude
                             min = 150*scaling_fac[["repfac"]], max = 400*scaling_fac[["repfac"]]),
    
    mcstate::pmcmc_parameter("crossp_12",
                             initial= last_best[["crossp_12"]],#0.05*scaling_fac[["crossp_12"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_12"]],max = 1*scaling_fac[["crossp_12"]]),
    
    mcstate::pmcmc_parameter("crossp_21",
                             initial= last_best[["crossp_21"]],#0.05*scaling_fac[["crossp_21"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_21"]],max = 1*scaling_fac[["crossp_21"]]),
    
    mcstate::pmcmc_parameter("crossp_34",
                             initial= last_best[["crossp_34"]],#0.05*scaling_fac[["crossp_12"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_34"]],max = 1*scaling_fac[["crossp_34"]]),
    
    mcstate::pmcmc_parameter("crossp_43",
                             initial= last_best[["crossp_43"]],#0.05*scaling_fac[["crossp_21"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_43"]],max = 1*scaling_fac[["crossp_43"]]))
  
  
  # Covariance matrix
  vcv <-cov(thetas)
  
  
}else {
  
  priors <- list(
    mcstate::pmcmc_parameter("beta_1",
                             initial= 0.4*scaling_fac[["beta_1"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_1"]]),
    mcstate::pmcmc_parameter("beta_2",
                             initial= 0.1*scaling_fac[["beta_2"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_2"]]),
    
    mcstate::pmcmc_parameter("beta_3",
                             initial= 0.4*scaling_fac[["beta_3"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_3"]]),
    mcstate::pmcmc_parameter("beta_4",
                             initial= 0.1*scaling_fac[["beta_4"]], # Transmission per capita
                             min = 0*scaling_fac[["beta_4"]]),
    
    mcstate::pmcmc_parameter("aduRR",
                             initial= 0.3*scaling_fac[["aduRR"]], # Excess infectiousness in Und 5
                             min = 0*scaling_fac[["aduRR"]], max = 1*scaling_fac[["aduRR"]]),
    
    mcstate::pmcmc_parameter("delta",
                             initial= 170*scaling_fac[["delta"]], # Duration of maternal Abs
                             min = 1*scaling_fac[["delta"]], max = 365*scaling_fac[["delta"]] ),
    
    mcstate::pmcmc_parameter("rho",
                             initial= 0.05*scaling_fac[["rho"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["rho"]],max = 1*scaling_fac[["rho"]]),
    
    mcstate::pmcmc_parameter("tau",
                             initial= 2*scaling_fac[["tau"]], # Duration of immunity
                             min = 0.5*scaling_fac[["tau"]] ),
    
    mcstate::pmcmc_parameter("w1_1",
                             initial= 0.1*scaling_fac[["w1_1"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_1"]], max = 1*scaling_fac[["w1_1"]]),
    
    mcstate::pmcmc_parameter("w1_2",
                             initial= 0.1*scaling_fac[["w1_2"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_2"]], max = 1*scaling_fac[["w1_2"]]),
    
    mcstate::pmcmc_parameter("w1_3",
                             initial= 0.1*scaling_fac[["w1_3"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_3"]], max = 1*scaling_fac[["w1_3"]]),
    
    mcstate::pmcmc_parameter("w1_4",
                             initial= 0.1*scaling_fac[["w1_4"]], # Seasonality amplitude
                             min = 0.05*scaling_fac[["w1_4"]], max = 1*scaling_fac[["w1_4"]]),
    
    
    mcstate::pmcmc_parameter("repfac",
                             initial= 287*scaling_fac[["repfac"]], # Seasonality amplitude
                             min = 150*scaling_fac[["repfac"]], max = 400*scaling_fac[["repfac"]]),
    
    mcstate::pmcmc_parameter("crossp_12",
                             initial= 0.05*scaling_fac[["crossp_12"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_12"]],max = 1*scaling_fac[["crossp_12"]]),
    
    mcstate::pmcmc_parameter("crossp_21",
                             initial= 0.05*scaling_fac[["crossp_21"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_21"]],max = 1*scaling_fac[["crossp_21"]]),
    
    mcstate::pmcmc_parameter("crossp_34",
                             initial= 0.05*scaling_fac[["crossp_34"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_34"]],max = 1*scaling_fac[["crossp_34"]]),
    
    mcstate::pmcmc_parameter("crossp_43",
                             initial= 0.05*scaling_fac[["crossp_43"]], # Duration of maternal Abs
                             min = 0*scaling_fac[["crossp_43"]],max = 1*scaling_fac[["crossp_43"]]))
  
  
  # Define covariance matrix at start
  ini<-c(priors[[1]]$mean,
         priors[[2]]$mean,
         priors[[3]]$mean,
         priors[[4]]$mean,
         priors[[5]]$mean,
         priors[[6]]$mean,
         priors[[7]]$mean,
         priors[[8]]$mean,
         priors[[9]]$mean,
         priors[[10]]$mean,
         priors[[11]]$mean,
         priors[[12]]$mean,
         priors[[13]]$mean,
         priors[[14]]$mean,
         priors[[15]]$mean,
         priors[[16]]$mean,
         priors[[17]]$mean
  )*0.12
  
  vcv <- diag(ini,length(ini))
  
}


c1<-params$transmission
c2<-params$transmission_holi 
aduid<-params$adult_id 
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_vec<-params$aging_vec
scalefc<-params$scaling_fac
pop<-params$pop
vac_camp_cov<-params$vac_camp_cov
vac_sche_cov<-params$vac_sche_cov


footransform <- make_transform(c1,
                               c2, 
                               aduid, 
                               mu, 
                               school,
                               n_school_steps,
                               n_age,
                               aging_vec,
                               init,
                               pop,
                               scalefc,
                               vac_camp_cov,
                               vac_sche_cov)


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


processed_chains <- mcstate::pmcmc_thin(samples, 
                                        burnin = n_burnin, 
                                        thin = 1)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)

print(parameter_mean_hpd/unlist(scaling_fac))

plot(processed_chains$probabilities[, "log_posterior"], type = "l",
     xlab = "Sample", ylab = "Log posterior")



if (stochastic==1){
  save(processed_chains,file=here("output","processed_samples.RData"))
  
}else{
  
  save(processed_chains,file=here("output","processed_samples_det_4str_simplex.RData"))
  
  
}

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


coda::effectiveSize(mcmc1)
1 - coda::rejectionRate(mcmc1)


