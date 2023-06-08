## make transform
# Create params transformation function
make_transform <- function(c_mat,
                           c_mat2, 
                           adult_id, 
                           mu, 
                           school,
                           n_school_steps,
                           n_age,
                           aging_vec,
                           ini,
                           pop,
                           scaling_fac) {
  
  function(theta) {
    
    theta_back<-c(
      beta_j = theta[["beta_j"]]/scaling_fac[["beta_j"]],
      beta_k = theta[["beta_k"]]/scaling_fac[["beta_k"]],
      delta = theta[["delta"]]/scaling_fac[["delta"]],
      rho = theta[["rho"]]/scaling_fac[["rho"]],
      tau = theta[["tau"]]/scaling_fac[["tau"]],
      w1_j = theta[["w1_j"]]/scaling_fac[["w1_j"]],
      w1_k = theta[["w1_j"]]/scaling_fac[["w1_j"]],
      repfac=theta[["repfac"]]/scaling_fac[["repfac"]],
      crossp_jk=theta[["crossp_jk"]]/scaling_fac[["crossp_jk"]],
      crossp_kj=theta[["crossp_kj"]]/scaling_fac[["crossp_kj"]]
    )

    c_mat[adult_id,adult_id]<-c_mat[adult_id,adult_id] * (theta[["aduRR"]]/scaling_fac[["aduRR"]])
    c_mat2[adult_id,adult_id]<-c_mat2[adult_id,adult_id]* (theta[["aduRR"]]/scaling_fac[["aduRR"]])
    
    #print(theta/unlist(scaling_fac))

    c(list(
      pop  = pop,
      init  = ini,
      mu    = mu,
      m     = c_mat,
      m_holi= c_mat2,
      school_step= as.double(school),
      n_school_steps=n_school_steps,
      N_age = n_age,
      aging_vec = aging_vec ),
      as.list(theta_back))
  }
}



## Compare 
compare <- function(state, observed, pars = NULL) {
  exp_noise <- 1e6

 # pop1<-sum(pars$pop[1:4])
 # pop2<-sum(pars$pop[5:8])
 # pop3<-sum(pars$pop[9:13])
 # pop4<-sum(pars$pop[14])

 noise<-rexp(n = length(state['cases_year1_str1',]), rate = exp_noise)

 # modelled_irate <-rbind(
 # 1000*(state['cases_year1',]/pop1) + noise,
 # 1000*(state['cases_year2',]/pop2) + noise,
 # 1000*(state['cases_year3',]/pop3) + noise,
 # 1000*(state['cases_year4',]/pop4) + noise)
 
  # Incidence rates per 1000 py
   modelled_irate <-rbind(
     1000*( (state['cases_year1_str1',]+state['cases_year1_str2',])/(state['person_year1',]/365)) + noise,
     1000*((state['cases_year2_str1',]+state['cases_year2_str2',])/(state['person_year2',]/365)) + noise,
     1000*((state['cases_year3_str1',]+state['cases_year3_str2',])/(state['person_year3',]/365)) + noise,
     1000*((state['cases_year4_str1',]+state['cases_year4_str2',])/(state['person_year4',]/365)) + noise,
     1000*((state['cases_year5_str1',]+state['cases_year5_str2',])/(state['person_year5',]/365)) + noise)
   
  
  observations_irate <-rbind(
    observed$cases_a1,
    observed$cases_a2,
    observed$cases_a3,
    observed$cases_a4,
    observed$cases_a5
  ) 
  llk_irate<-colSums(dpois(x = observations_irate, lambda = modelled_irate, log = TRUE)/5,na.rm=TRUE)

  # Strain prevalence in IID2
  modelled_strain<-rbind(
    ((state['cases_year1_str2',]+
      state['cases_year2_str2',]+
       state['cases_year3_str2',]+
       state['cases_year4_str2',]+
       state['cases_year5_str2',])/
      (state['cases_year1_str2',]+
         state['cases_year2_str1',]+
         state['cases_year3_str1',]+
         state['cases_year4_str1',]+
         state['cases_year5_str1',]+
         state['cases_year1_str2',]+
         state['cases_year2_str2',]+
         state['cases_year3_str2',]+
         state['cases_year4_str2',]+
         state['cases_year5_str2',])) + noise)
  
  observed_event<-40
  
  observed_size<-476
  
  llk_strain<-dbinom(x=round(observed_event),
                           size = observed_size,
                           prob = modelled_strain,
                           log = TRUE)

  # # Weekly cases reported 0-4
  # 
  # modelled_report<-state['reported_wk0_4',]
  # observations_reported<-observed$reported_04
  # llk_reported_04<-dpois(x = observations_reported, 
  #                             lambda = modelled_report, log = TRUE)
  # 
  # # Weekly cases reported 5-65
  # 
  # modelled_report<-state['reported_wk5_65',]
  # observations_reported<-observed$reported_65
  # llk_reported_65<-dpois(x = observations_reported, 
  #                     lambda = modelled_report, log = TRUE)
  # 
  # # Weekly cases reported 65+
  # 
  # modelled_report<-state['reported_wk65_p',]
  # observations_reported<-observed$reported_65p
  # llk_reported_65p<-dpois(x = observations_reported, 
  #                     lambda = modelled_report, log = TRUE)
  # 
  # Weekly cases reported 65+
  
  modelled_report<-state['reported_wk',] + noise
  observations_reported<-observed$reported
  llk_reported<-dpois(x = observations_reported, 
                          lambda = modelled_report, log = TRUE)/354
  
  # Seroprevalence in children 1 to 7
  modelled_sero<-rbind(
    state['seroprev_num1.2',]/state['seroprev_den1.2',] + noise,
    state['seroprev_num2.3',]/state['seroprev_den2.3',] + noise,
    state['seroprev_num3.4',]/state['seroprev_den3.4',] + noise,
    state['seroprev_num4.5',]/state['seroprev_den4.5',] + noise,
    state['seroprev_num5.6',]/state['seroprev_den5.6',] + noise,
    state['seroprev_num6.7',]/state['seroprev_den6.7',] + noise
    )

  observed_event<-rbind(
    observed$sero1*103,
    observed$sero2*107,
    observed$sero3*121,
    observed$sero4*124,
    observed$sero5*122,
    observed$sero6*109
  )
  
  observed_size<-rbind(
    103,
    107,
    121,
    124,
    122,
    109
  )
  
  llk_sero<-colSums(dbinom(x=round(observed_event),
                           size = observed_size,
                           prob = modelled_sero,
                           log = TRUE),na.rm = TRUE)
  
  
  posterior<-colSums(rbind(llk_irate,
                           llk_strain,
                           llk_reported,
                           # llk_reported_04,
                           # llk_reported_65,
                           # llk_reported_65p,
                           llk_sero
                           ),na.rm=T)
 # print(posterior)
  
  return(posterior)
  
}

index <- function(info) {
  list(run = c(cases_year1_str1 = info$index$cases_year_str1[1],
               cases_year2_str1 = info$index$cases_year_str1[2],
               cases_year3_str1 = info$index$cases_year_str1[3],
               cases_year4_str1 = info$index$cases_year_str1[4],
               cases_year5_str1 = info$index$cases_year_str1[5],
               cases_year1_str2 = info$index$cases_year_str2[1],
               cases_year2_str2 = info$index$cases_year_str2[2],
               cases_year3_str2 = info$index$cases_year_str2[3],
               cases_year4_str2 = info$index$cases_year_str2[4],
               cases_year5_str2 = info$index$cases_year_str2[5],
               
               cases_day1_str1 = info$index$cases_day_str1[1],
               cases_day2_str1 = info$index$cases_day_str1[2],
               cases_day3_str1 = info$index$cases_day_str1[3],
               cases_day4_str1 = info$index$cases_day_str1[4],
               cases_day5_str1 = info$index$cases_day_str1[5],
               cases_day1_str2 = info$index$cases_day_str2[1],
               cases_day2_str2 = info$index$cases_day_str2[2],
               cases_day3_str2 = info$index$cases_day_str2[3],
               cases_day4_str2 = info$index$cases_day_str2[4],
               cases_day5_str2 = info$index$cases_day_str2[5],
               
               person_year1 = info$index$person_year[1],
               person_year2 = info$index$person_year[2],
               person_year3 = info$index$person_year[3],
               person_year4 = info$index$person_year[4],
               person_year5 = info$index$person_year[5],
               reported_wk = info$index$reported_wk,
               seroprev_num1.2 = info$index$seroprev_num[2],
               seroprev_num2.3 = info$index$seroprev_num[3],
               seroprev_num3.4 = info$index$seroprev_num[4],
               seroprev_num4.5 = info$index$seroprev_num[5],
               seroprev_num5.6 = info$index$seroprev_num[6],
               seroprev_num6.7 = info$index$seroprev_num[7],
               seroprev_den1.2 = info$index$seroprev_den[2],
               seroprev_den2.3 = info$index$seroprev_den[3],
               seroprev_den3.4 = info$index$seroprev_den[4],
               seroprev_den4.5 = info$index$seroprev_den[5],
               seroprev_den5.6 = info$index$seroprev_den[6],
               seroprev_den6.7 = info$index$seroprev_den[7],
               S      = info$index$S,    
               Ek      = info$index$EK,      
               Ik      = info$index$IK,    
               Ak      = info$index$AK,
               Rk      = info$index$RK),
       
       state = c(
         t = info$index$time,
         inc_day_j = info$index$infections_day_j,
         inc_day_k = info$index$infections_day_k,
         cases_year1_str1 = info$index$cases_year_str1[1],
         cases_year2_str1 = info$index$cases_year_str1[2],
         cases_year3_str1 = info$index$cases_year_str1[3],
         cases_year4_str1 = info$index$cases_year_str1[4],
         cases_year5_str1 = info$index$cases_year_str1[5],
         cases_year1_str2 = info$index$cases_year_str2[1],
         cases_year2_str2 = info$index$cases_year_str2[2],
         cases_year3_str2 = info$index$cases_year_str2[3],
         cases_year4_str2 = info$index$cases_year_str2[4],
         cases_year5_str2 = info$index$cases_year_str2[5],
         cases_day1_str1 = info$index$cases_day_str1[1],
         cases_day2_str1 = info$index$cases_day_str1[2],
         cases_day3_str1 = info$index$cases_day_str1[3],
         cases_day4_str1 = info$index$cases_day_str1[4],
         cases_day5_str1 = info$index$cases_day_str1[5],
         cases_day1_str2 = info$index$cases_day_str2[1],
         cases_day2_str2 = info$index$cases_day_str2[2],
         cases_day3_str2 = info$index$cases_day_str2[3],
         cases_day4_str2 = info$index$cases_day_str2[4],
         cases_day5_str2 = info$index$cases_day_str2[5],
         person_year1 = info$index$person_year[1],
         person_year2 = info$index$person_year[2],
         person_year3 = info$index$person_year[3],
         person_year4 = info$index$person_year[4],
         person_year5 = info$index$person_year[5],
         reported_wk = info$index$reported_wk,
         seroprev_num1.2 = info$index$seroprev_num[2],
         seroprev_num2.3 = info$index$seroprev_num[3],
         seroprev_num3.4 = info$index$seroprev_num[4],
         seroprev_num4.5 = info$index$seroprev_num[5],
         seroprev_num5.6 = info$index$seroprev_num[6],
         seroprev_num6.7 = info$index$seroprev_num[7],
         seroprev_den1.2 = info$index$seroprev_den[2],
         seroprev_den2.3 = info$index$seroprev_den[3],
         seroprev_den3.4 = info$index$seroprev_den[4],
         seroprev_den4.5 = info$index$seroprev_den[5],
         seroprev_den5.6 = info$index$seroprev_den[6],
         seroprev_den6.7 = info$index$seroprev_den[7],
         seroprev1.2 = info$index$seroprev[2],
         seroprev2.3 = info$index$seroprev[3],
         seroprev3.4 = info$index$seroprev[4],
         seroprev4.5 = info$index$seroprev[5],
         seroprev5.6 = info$index$seroprev[6],
         seroprev6.7 = info$index$seroprev[7],
         S      = info$index$S,    
         Ek      = info$index$EK,      
         Ik      = info$index$IK,    
         Ak      = info$index$AK,
         Rk      = info$index$RK
         
       )
  )
}


compare_empty <- function(state, observed, pars = NULL) {
  
  llk<- -100
}




















## Plot fits 
plot_fits<-function(sims,data){

  ## Community incidence (IID2)
  t<-which(sims["t",1,]%in%
             data$time_end[which(!is.na(data$cases_a1))])
  
  irates<-1000*(cbind(sims["cumu_inc1",,t],
                      sims["cumu_inc2",,t],
                      sims["cumu_inc3",,t],
                      sims["cumu_inc4",,t])/
                  (cbind(sims["n_age1",,t],
                         sims["n_age2",,t],
                         sims["n_age3",,t],
                         sims["n_age4",,t])/365))
  
  irate_obs<-c(data$cases_a1[1],data$cases_a2[1],
               data$cases_a3[1],data$cases_a4[1])
  matplot(c(1,2,3,4),t(irates), type = "p", col = "#00000011", 
          xlab = "Age", ylab = "Incidence per 1000", las = 1,ylim=c(0,250),xaxt="n")
  
  xtick<-seq(1, 4, by=1)
  axis(side=1, at=xtick, labels = c("0_4","5_14","15_64","65+"))
  arrows(x0=c(1,2,3,4), y0=data_iid2.c4$CI_lower, 
         x1=c(1,2,3,4), y1=data_iid2.c4$CI_upper,
         code=3, angle=90, length=0.1)
  points(irate_obs , pch = 19, col = "red")
  
  ## Weekly cass reported by UKHSA
  id<-which(sims["t",1,]%in%data_all$time_end[which(!is.na(data_all$reported))])
  reported<-sims["reported_wk",,id]
  reported_obs<-data$reported[which(!is.na(data$reported))]
  t<-seq(1,length(id),by=1)#sims["t",1,id]
  matplot(t,t(reported), type = "l", col = "#00000011", 
          xlab = "week", ylab = "Weekly reported cases", las = 1,ylim = c(0,200))
  points(t,reported_obs, pch = 19, col = "red")
  
  ## Seroprevalence 
  
  id<-which(sims["t",1,]%in%
              data$time_end[which(!is.na(data$sero1))])
  sero_model<-rbind(
    sims['seroprev1.2',,id],
    sims['seroprev2.3',,id],
    sims['seroprev3.4',,id],
    sims['seroprev4.5',,id],
    sims['seroprev5.6',,id],
    sims['seroprev6.7',,id]
  )
  id<-which(!is.na(data$sero1))
  sero_obs<-c(data$sero1[id],
              data$sero2[id],
              data$sero3[id],
              data$sero4[id],
              data$sero5[id],
              data$sero6[id])
  
  matplot(c(1,2,3,4,5,6),sero_model, type = "p", col = "#00000011", 
          xlab = "Age", ylab = "Seropositivity", las = 1,ylim=c(0,1),xaxt="n")
  xtick<-seq(1, 6, by=1)
  axis(side=1, at=xtick, labels = c("0_1","1_2","2_3","3_4","5_6","6_7"))
  arrows(x0=c(1,2,3,4,5,6), y0=sero$V2, 
         x1=c(1,2,3,4,5,6), y1=sero$V3,
         code=3, angle=90, length=0.1)
  points(sero_obs , pch = 19, col = "red")
  
}



# Demographicmodel
run_demog_model<-function(mu, ini=init, p=params, times=365*5, seiar_inst=seiar){

  
  
  n_particles <- 1L
  c_mat<-p$transmission
  c_mat[p$infa_id,p$infa_id]<-c_mat[p$infa_id,p$infa_id]*p$aduRR
  
  c_mat2<-p$transmission_holi
  c_mat2[p$infa_id,p$infa_id]<-c_mat2[p$infa_id,p$infa_id]*p$aduRR
  
  
  mort<-c(mu)/10000
  
  pars = list(
    beta = 0.0 ,   # transm coefficient
    repfac = 287,
    rho   = p$rho, # rel infect asymptomatic 
    init  = ini,
    mu    = mort/365,
    m     = c_mat,
    m_holi= c_mat2,
    aging_mat= p$aging_mat, 
    N_age = p$N_age,
    w1 = p$w1,
    school= as.double(p$school_uk))
  

  model <- seiar_inst$new(pars, 0, 1)
  n_times<-times
  tt<-seq(1,n_times,1)
  x<- model$simulate(tt)
  x<-drop(x)

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
  idx<-model$info()$index
  results<-list(
    idx = idx,
    states=x,
    age.distr.sim=age.distr.sim
    
  )

  return(results)
}

########################
## Run model

run_model<-function(pars, times, seiar=model.seiar){
 
  n_particles <- 3L
  dt <- pars$dt
  seed<-1
  g.ini<-round(pars$pop*pars$p_nonsecretor)
  s.ini<-pars$pop-g.ini
  i.ini<-pars$contact$demography$population*0
  i.ini[2]<-seed
  mort.rates<- pars$mu
  c_mat<-pars$transmission
  c_mat[pars$infa_id,pars$infa_id]<-c_mat[pars$infa_id,pars$infa_id]*pars$aduRR

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
                                   rho   = pars$rho, # rel infect asymptomatic 
                                   mu    = mort.rates,
                                   m = c_mat,
                                   aging_mat= pars$aging_mat, 
                                   N_age = pars$N_age,
                                   w1 = pars$w1),
                       time = 0,
                       n_particles = n_particles,
                       n_threads = 4L,
                       seed = 1L)
  
  # Define how long the model runs for, number of time steps
  n_times <- times
  x <- array(NA, dim = c(model$info()$len,n_particles, n_times))

  # For loop to run the model iteratively

    # for (t in seq_len(n_times))
    #   {
    # x[ ,  ,t] <- model$run(t)
    # }

  tt<-seq(1/pars$dt,n_times,1/pars$dt)# output only the day step 
  x<- model$simulate(tt)
  

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


get_output<-function(theta,pars, ntimes, seiar=model.seiar){
 
  
  nruns <- nrow(theta)
  
  
  # Allocate memory
  irate <- matrix(NA, nrow = nruns, ncol = 4)
  
  
  for (jj in 1:nruns) {
   
  pars$beta<-theta[['beta']][jj]
  pars$aduRR<-theta[['aduRR']][jj]
  
  sim<-run_model(pars,ntimes, seiar)
  
  states<-sim$states
  idday<-seq(1,ntimes, 1/pars$dt)
  idx<-sim$idx
  
  N<- states[idx$M,1,]+
    states[idx$G,1,]+
    states[idx$S,1,]+
    states[idx$R,1,]
  
  st<-which(days_vec==idd2_startdate)* 1/pars$dt
  ed<-which(days_vec==idd2_enddate) * 1/pars$dt
  span<-seq(st,ed,1)
  cases<-states[idx$cumu_inc,1,span]
  cases<-tail(t(cases),1)-head(t(cases),1)
  cases.4<-c(sum(cases[c(1,2)]),cases[3],sum(cases[c(4,5,6,7,8)]),sum(cases[c(9,10)]))
  
  PY<-(states[idx$M,1,span]+
         states[idx$G,1,span]+
         states[idx$S,1,span]+
         states[idx$R,1,span])
  
  PY<-apply(PY,1,cumsum)
  PY<-tail(PY,1)-head(PY,1)
  PY<-PY/(365/pars$dt)
  PY.4<-c(sum(PY[c(1,2)]),PY[3],sum(PY[c(4,5,6,7,8)]),sum(PY[c(9,10)]))

    irate[jj,]<-1000*cases.4/PY.4
  
  }
  
  
  out<-list(
    irate_pyear=irate#irate_pyear
  )
  
  return(out)
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


