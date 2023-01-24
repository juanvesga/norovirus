# Demographicmodel
run_demog_model<-function(mu, pars=model.params, times=365*5,seiar=model.seiar){

  n_particles <- 1L
  dt <- pars$dt
  seed<-0
  s.ini<-pars$pop
  mort.rates<- c(mu)/10000
  model <- seiar$new(pars = list(dt = dt,
                                   M_ini= pars$contact$demography$population*0,
                                   G_ini= pars$contact$demography$population*0,
                                   S_ini=s.ini,
                                   E_ini=pars$contact$demography$population*0,
                                   I_ini=pars$contact$demography$population*0,
                                   A_ini=pars$contact$demography$population*0,
                                   R_ini=pars$contact$demography$population*0,
                                   age_select=pars$age_select,
                                   mu = mort.rates/365,
                                   m = pars$transmission,
                                   age_beta = pars$age_beta,
                                   aging_mat= pars$aging_mat, 
                                   N_age = pars$N_age),
                       time = 0,
                       n_particles = n_particles,
                       n_threads = 4L,
                       seed = 1L)
  
  # Define how long the model runs for, number of time steps
  n_times <- times
  x <- array(NA, dim = c(model$info()$len, n_times))

  # For loop to run the model iteratively
  for (t in seq_len(n_times)) {
    x[ ,t] <- model$run(t)
  }
  time <- x[1, ]
  
  # Plotting the trajectories
  idx<-model$info()$index
  
  # Check age distribution
  age.init<-rbind(
    x[idx$M,dim(x)[2]],
    x[idx$G,dim(x)[2]],
    x[idx$S,dim(x)[2]],
    x[idx$E,dim(x)[2]],
    x[idx$I,dim(x)[2]],
    x[idx$A,dim(x)[2]],
    x[idx$R,dim(x)[2]])
  
  age.distr.sim=colSums(age.init)/sum(age.init)
  
  results<-list(
    states=x,
    age.distr.sim=age.distr.sim
    
  )

  return(results)
}

########################
## Run model

run_model<-function(pars, times, seiar=model.seiar){
  
  n_particles <- 5L
  dt <- pars$dt
  seed<-5
  g.ini<-round(pars$pop*pars$p_nonsecretor)
  s.ini<-pars$pop-g.ini
  i.ini<-pars$contact$demography$population*0
  i.ini[2]<-seed
  mort.rates<- pars$mu
  pars$age_beta[1]<- pars$und5inf
  pars$age_beta[2]<- pars$und5inf
  
  model <- seiar$new(pars = list(dt = dt,
                                   M_ini= pars$contact$demography$population*0,
                                   G_ini= g.ini,
                                   S_ini=s.ini,
                                   E_ini=pars$contact$demography$population*0,
                                   I_ini=i.ini,
                                   A_ini=pars$contact$demography$population*0,
                                   R_ini=pars$contact$demography$population*0,
                                   age_select=pars$age_select,
                                   beta = pars$beta,   # transm coefficient
                                   delta = pars$delta,  # maternal Ab decay
                                   epsilon = pars$epsilon,   # incubation
                                   theta = pars$theta,   # duration symptoms
                                   sigma = pars$sigma, # duration asymp shedding
                                   tau   = pars$tau,#(365*5.1),    # duration immunity
                                   rho   = pars$rho, # rel infect asymptomatic 
                                   p_nonsecretor=pars$p_nonsecretor, # Fraction immune genetically
                                   mu    = mort.rates/365,
                                   m = pars$transmission,
                                   age_beta = pars$age_beta,
                                   aging_mat= pars$aging_mat, 
                                   N_age = pars$N_age),
                       time = 0,
                       n_particles = n_particles,
                       n_threads = 4L,
                       seed = 1L)
  
  # Define how long the model runs for, number of time steps
  n_times <- times
  x <- array(NA, dim = c(model$info()$len,n_particles, n_times))
  
  # For loop to run the model iteratively
  for (t in seq_len(n_times)) {
    x[ ,  ,t] <- model$run(t)
  }
  d<-model$run(1)
  time <- x[1, 1, ]
  # Plotting the trajectories
  idx<-model$info()$index
  
  
  # Check age distribution
  age.init<-rbind(
    x[idx$M,1 ,dim(x)[3]],
    x[idx$G,1 ,dim(x)[3]],
    x[idx$S,1 ,dim(x)[3]],
    x[idx$E,1 ,dim(x)[3]],
    x[idx$I,1 ,dim(x)[3]],
    x[idx$A,1 ,dim(x)[3]],
    x[idx$R,1 ,dim(x)[3]])
  
  age.distr.sim=colSums(age.init)/sum(age.init)
  results<-list(
    idx=idx,
    states=x,
    age.distr.sim=age.distr.sim
  )
  
  return(results)
}

################################
# Plot age dsitributions
plot_age_distr<-function(sim_age, pars=pars){
  df<-data.frame(
    age=factor(pars$contact$demography$age.group),
    age.distr.sim=sim_age,
    age.distr.ons=pars$contact$demography$proportion
  )
  
  df$age <- factor(df$age, levels = df$age)
  
  df.melt<- reshape2::melt(df) 
  
  p1<-ggplot(data = df.melt, aes(x=age,y=value, fill=variable) )+
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme_minimal()
  p1 + scale_fill_manual(values=c('#999999','#E69F00'))
}

# helper funcs
array_cumsum <- function(a, margin) {
  n <- length(dim(a))
  permorder <- append(x = 2:n, 1, margin - 1)
  aperm(apply(a, -margin, cumsum), permorder)
}
