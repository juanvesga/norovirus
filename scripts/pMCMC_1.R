#############
## PMCMC
#############
# Controls 
n_steps  <- 5000
n_burnin <- 200
n_out    <- 2000
n_thin   <- 5#round((n_steps-n_burnin)/n_out)

# Load previous chains
load(here("output","processed_samples.RData"))

thetas<-processed_chains$pars
mean_hpd <- apply(thetas, 2, mean)

# Define  priors 
priors <- list(
  mcstate::pmcmc_parameter("beta", mean_hpd["beta"], min = 0),
  mcstate::pmcmc_parameter("und5inf", mean_hpd["und5inf"], min = 1),
  mcstate::pmcmc_parameter("w1", mean_hpd["w1"], min = 0),
  mcstate::pmcmc_parameter("w2", mean_hpd["w2"], min = 0),
  mcstate::pmcmc_parameter("delta", mean_hpd["delta"], min = 1),
  mcstate::pmcmc_parameter("repfac", mean_hpd["repfac"], min = 1),
  mcstate::pmcmc_parameter("tau", 5.1, min = 0.5)
  #, prior = function(p)
  #  dnorm(p, mean = 287, sd = 50, log = TRUE))
)

# Create params transformation function
make_transform <- function(par=params,ini=init) {
  function(theta) {
    
    c_mat<-par$transmission
    
    c_mat[par$infa_id,par$infa_id]<-c_mat[par$infa_id,par$infa_id]*theta[["und5inf"]]
    
    list(
      beta  = theta[["beta"]] ,   # transm coefficient
      repfac= theta[["repfac"]],
      w1    = theta[['w1']],
      w2    = theta[['w2']],
      delta = 1/theta[["delta"]],
      tau   = theta[["tau"]],
      init  = ini,
      mu    = par$mu,
      m     = c_mat,
      aging_mat= par$aging_mat, 
      N_age = par$N_age)
  }
}
transform <- make_transform(params,init)


# Define covariance matrix at start
ini<-c(priors[[1]]$mean,
       priors[[2]]$mean,
       priors[[3]]$mean,
       priors[[4]]$mean,
       priors[[5]]$mean,
       priors[[6]]$mean,
       priors[[7]]$mean)*0.12


vcv <- diag(ini, 7)
vcv[1:6,1:6]<-cov(thetas)

# Create pmcmc parameters
mcmc_pars <- mcstate::pmcmc_parameters$new(priors, vcv, transform)


control <- mcstate::pmcmc_control(
  n_steps = n_steps,
  n_chains = 1,
  n_threads_total = 8,
  n_workers = 1,
  save_state = TRUE,
  save_trajectories = TRUE,
  progress = TRUE)


samples <- mcstate::pmcmc(mcmc_pars, filter, control = control)


# Plot initial 
plot(samples$probabilities[, "log_posterior"], type = "s",
     xlab = "Sample", ylab = "Log posterior",
     xlim = c(0,n_burnin))


# Process samples

processed_chains <- mcstate::pmcmc_thin(samples, burnin = n_burnin, thin = n_thin)
parameter_mean_hpd <- apply(processed_chains$pars, 2, mean)
parameter_mean_hpd

save(processed_chains,file=here("output","processed_samples.RData"))

mcmc1 <- coda::as.mcmc(cbind(processed_chains$probabilities, processed_chains$pars))
summary(mcmc1)
windows()
plot(mcmc1)


coda::effectiveSize(mcmc1)
1 - coda::rejectionRate(mcmc1)


