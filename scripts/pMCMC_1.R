#############
## PMCMC
#############
# Controls 
n_steps  <- 10000
n_burnin <- round(n_steps*0.1)
n_out    <- round(n_steps*0.50)
n_thin   <- round((n_steps-n_burnin)/n_out)

# Load previous chains
 load(here("output","processed_samples.RData"))
 id<- which(processed_chains$probabilities[,"log_posterior"] == 
              max(processed_chains$probabilities[,"log_posterior"]))[1]
# 
 thetas<-processed_chains$pars
 mean_hpd <- apply(thetas, 2, mean)
 last_best <- thetas[id,]

 # Define  priors 
 scaling_fac<-list(
   beta = 1000,
   und5inf = 10,
   delta = 1,
   rho = 1000,
   tau = 10,
   w1 = 100
 )
 
priors <- list(
  mcstate::pmcmc_parameter("beta", 
                           initial= 0.05*scaling_fac[["beta"]], # Transmission per capita
                           min = 0*scaling_fac[["beta"]]),
  
  mcstate::pmcmc_parameter("und5inf",
                           initial= 5*scaling_fac[["und5inf"]], # Excess infectiousness in Und 5 
                           min = 1*scaling_fac[["und5inf"]]),
  
  mcstate::pmcmc_parameter("delta", 
                           initial= 79*scaling_fac[["delta"]], # Duration of maternal Abs
                           min = 1*scaling_fac[["delta"]]),
  
  mcstate::pmcmc_parameter("rho", 
                           initial= 0.05*scaling_fac[["rho"]], # Duration of maternal Abs
                           min = 0*scaling_fac[["rho"]]),
  
  # 
  mcstate::pmcmc_parameter("tau", 
                           initial=2*scaling_fac[["tau"]], # Duration of immunity 
                           min = 0.5*scaling_fac[["tau"]] ),
  
  mcstate::pmcmc_parameter("w1", 
                           initial= 0.15*scaling_fac[["w1"]], # Seasonality amplitude 
                            min = 0.01*scaling_fac[["w1"]])
)

# Create params transformation function
make_transform <- function(c_mat,
                           c_mat2, 
                           infa_id, 
                           mu, 
                           school,
                           n_school_steps,
                           n_age,
                           aging_mat,
                           ini,
                           pop,
                           scaling_fac) {
  
  function(theta) {
    
    theta_back<-c(
      beta = theta[["beta"]]/scaling_fac[["beta"]],
      und5inf = theta[["und5inf"]]/scaling_fac[["und5inf"]],
      delta = theta[["delta"]]/scaling_fac[["theta"]],
      rho = theta[["rho"]]/scaling_fac[["rho"]],
      tau = theta[["tau"]]/scaling_fac[["tau"]],
      w1 = theta[["w1"]]/scaling_fac[["w1"]]
    )
    
    c_mat[infa_id,infa_id]<-c_mat[infa_id,infa_id]* theta_back[["und5inf"]]
    c_mat2[infa_id,infa_id]<-c_mat2[infa_id,infa_id]* theta_back[["und5inf"]]
    
    c(list(
      pop  = pop,
      init  = ini,
      mu    = mu,
      m     = c_mat,
      m_holi= c_mat2,
      school_step= as.double(school),
      n_school_steps=n_school_steps,
      N_age = n_age,
      aging_mat = aging_mat ),
      as.list(theta_back))
  }
}

c1<-params$transmission
c2<-params$transmission_holi 
id<-params$infa_id 
mu<-params$mu 
school<-params$school_uk
n_school_steps<-params$n_school_steps
n_age<-params$N_age
aging_mat<-params$aging_mat
transform <- make_transform(c1,
                            c2, 
                            id, 
                            mu, 
                            school,
                            n_school_steps,
                            n_age,
                            aging_mat,
                            init,
                            pop=params$pop,
                            scaling_fac = scaling_fac)


# Define covariance matrix at start
ini<-c(priors[[1]]$mean,
       priors[[2]]$mean,
       priors[[3]]$mean,
       priors[[4]]$mean,
       priors[[5]]$mean,
       priors[[6]]$mean
       )*0.12


#vcv <- diag(ini, 6)
vcv<-cov(thetas)

# Create pmcmc parameters
mcmc_pars <- mcstate::pmcmc_parameters$new(priors, vcv, transform)


control <- mcstate::pmcmc_control(
  n_steps = n_steps,
  n_chains = 1,
  n_threads_total = 1,
  n_workers = 1,
  save_state = TRUE,
  save_trajectories = FALSE,
  progress = TRUE)


samples <- mcstate::pmcmc(mcmc_pars, filter, control = control)


# Plot initial 
plot(samples$probabilities[, "log_posterior"], type = "s",
     xlab = "Sample", ylab = "Log posterior",
     xlim = c(0,n_burnin))


# Process samples
plot(samples$probabilities[,"log_likelihood"])
matplot(samples$pars,type = "l" )


processed_chains <- mcstate::pmcmc_thin(samples, burnin = n_burnin, thin = n_thin)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)


save(processed_chains,file=here("output","processed_samples.RData"))

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


coda::effectiveSize(mcmc1)
1 - coda::rejectionRate(mcmc1)


