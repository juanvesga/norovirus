#############
## PMCMC
#############
# Controls 
n_steps  <- 20000
n_burnin <- round(n_steps*0.5)
n_out    <- round(n_steps*0.50)
n_thin   <- 10#round((n_steps-n_burnin)/n_out)

# Load previous chains
if (stochastic==1){
  load(here("output","processed_samples.RData")) 
}else{
  
  load(here("output","processed_samples_det_full.RData"))  
  
}


id<- which(processed_chains$probabilities[,"log_posterior"] == 
             max(processed_chains$probabilities[,"log_posterior"]))[1]
# 
thetas<-processed_chains$pars

mean_hpd <- apply(thetas, 2, mean)
last_best <- thetas[id,]

# Define  priors 
scaling_fac<-params$scaling_fac
#print(last_best/unlist(scaling_fac))
# # 
priors <- list(
  mcstate::pmcmc_parameter("beta_j",
                           initial= last_best[["beta_j"]],#0.4*scaling_fac[["beta_j"]], # Transmission per capita
                           min = 0*scaling_fac[["beta_j"]]),
  mcstate::pmcmc_parameter("beta_k",
                           initial= last_best[["beta_k"]],#0.1*scaling_fac[["beta_k"]], # Transmission per capita
                           min = 0*scaling_fac[["beta_k"]]),
  
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
  
  mcstate::pmcmc_parameter("w1_j",
                           initial= last_best[["w1_j"]],#0.1*scaling_fac[["w1_j"]], # Seasonality amplitude
                           min = 0.05*scaling_fac[["w1_j"]], max = 1*scaling_fac[["w1_j"]]),
  
  mcstate::pmcmc_parameter("w1_k",
                           initial= last_best[["w1_k"]],#0.1*scaling_fac[["w1_k"]], # Seasonality amplitude
                           min = 0.05*scaling_fac[["w1_k"]], max = 1*scaling_fac[["w1_k"]]),
  
  mcstate::pmcmc_parameter("repfac",
                           initial= last_best[["repfac"]],#287*scaling_fac[["repfac"]], # Seasonality amplitude
                           min = 150*scaling_fac[["repfac"]], max = 400*scaling_fac[["repfac"]]),
  
  mcstate::pmcmc_parameter("crossp_jk",
                           initial= last_best[["crossp_jk"]],#0.05*scaling_fac[["crossp_jk"]], # Duration of maternal Abs
                           min = 0*scaling_fac[["crossp_jk"]],max = 1*scaling_fac[["crossp_jk"]]),
  
  mcstate::pmcmc_parameter("crossp_kj",
                           initial= last_best[["crossp_kj"]],#0.05*scaling_fac[["crossp_kj"]], # Duration of maternal Abs
                           min = 0*scaling_fac[["crossp_kj"]],max = 1*scaling_fac[["crossp_kj"]])
)
# 
# priors <- list(
#   mcstate::pmcmc_parameter("beta_j",
#                            initial= 0.4*scaling_fac[["beta_j"]], # Transmission per capita
#                            min = 0*scaling_fac[["beta_j"]]),
#   mcstate::pmcmc_parameter("beta_k",
#                            initial= 0.1*scaling_fac[["beta_k"]], # Transmission per capita
#                            min = 0*scaling_fac[["beta_k"]]),
# 
#   mcstate::pmcmc_parameter("aduRR",
#                            initial= 0.3*scaling_fac[["aduRR"]], # Excess infectiousness in Und 5
#                            min = 0*scaling_fac[["aduRR"]], max = 1*scaling_fac[["aduRR"]]),
# 
#   mcstate::pmcmc_parameter("delta",
#                            initial= 170*scaling_fac[["delta"]], # Duration of maternal Abs
#                            min = 1*scaling_fac[["delta"]], max = 365*scaling_fac[["delta"]] ),
# 
#   mcstate::pmcmc_parameter("rho",
#                            initial= 0.05*scaling_fac[["rho"]], # Duration of maternal Abs
#                            min = 0*scaling_fac[["rho"]],max = 1*scaling_fac[["rho"]]),
# 
#   mcstate::pmcmc_parameter("tau",
#                            initial=2*scaling_fac[["tau"]], # Duration of immunity
#                            min = 0.5*scaling_fac[["tau"]] ),
# 
#   mcstate::pmcmc_parameter("w1_j",
#                            initial= 0.1*scaling_fac[["w1_j"]], # Seasonality amplitude
#                            min = 0.05*scaling_fac[["w1_j"]], max = 1*scaling_fac[["w1_j"]]),
# 
#   mcstate::pmcmc_parameter("w1_k",
#                            initial= 0.1*scaling_fac[["w1_k"]], # Seasonality amplitude
#                            min = 0.05*scaling_fac[["w1_k"]], max = 1*scaling_fac[["w1_k"]]),
#   
#   mcstate::pmcmc_parameter("repfac",
#                            initial= 287*scaling_fac[["repfac"]], # Seasonality amplitude
#                            min = 150*scaling_fac[["repfac"]], max = 400*scaling_fac[["repfac"]]),
#   
#   mcstate::pmcmc_parameter("crossp_jk",
#                            initial= 0.05*scaling_fac[["crossp_jk"]], # Duration of maternal Abs
#                            min = 0*scaling_fac[["crossp_jk"]],max = 1*scaling_fac[["crossp_jk"]]),
#   
#   mcstate::pmcmc_parameter("crossp_kj",
#                            initial= 0.05*scaling_fac[["crossp_kj"]], # Duration of maternal Abs
#                            min = 0*scaling_fac[["crossp_kj"]],max = 1*scaling_fac[["crossp_kj"]])
# )


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
                            scalefc)


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
       priors[[11]]$mean
)*0.12

#colnames(thetas)[2]<-"aduRR"
vcv <-cov(thetas)
#vcv <- diag(ini,11)
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
  
  save(processed_chains,file=here("output","processed_samples_det_full.RData"))
  
  
}

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


coda::effectiveSize(mcmc1)
1 - coda::rejectionRate(mcmc1)


