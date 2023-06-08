## Definition of the time-step and output as "time"
dt <- user(1)
steps_per_week<- 7/dt
steps_per_year<- 365/dt
initial(time) <- 0
update(time) <- (step + 1) * dt

## Equations for transitions between compartments by age group
# Not infected
update(M[]) <-  
  M[i] + 
  n_bM[i] - 
  n_ageoM[i] + 
  n_ageiM[i] - 
  n_MS[i] - 
  n_muM[i] 

update(G[]) <-   
  G[i] + 
  n_bG[i] - 
  n_ageoG[i] + 
  n_ageiG[i] - 
  n_muG[i]


update(S[]) <-
  S[i] + 
  n_bS[i] - 
  n_ageoS[i] + 
  n_ageiS[i] + 
  n_MS[i] + 
  n_Rj_S[i] + 
  n_Rjk_S[i] + 
  n_Rk_S[i] -
  n_S_Ej[i] - 
  n_S_Ek[i] - 
  n_muS[i]


# Strain 1infected(GII.4)
update(Ej[]) <-
  Ej[i]- 
  n_ageoEj[i] + 
  n_ageiEj[i] + 
  n_S_Ej[i] - 
  n_Ej_Ij[i] - 
  n_muEj[i]


update(Ij[]) <-   
  Ij[i] - 
  n_ageoIj[i] + 
  n_ageiIj[i] + 
  n_Ej_Ij[i] -
  n_Ij_Aj[i] - 
  n_muIj[i] 


update(Aj[]) <-  
  Aj[i]- 
  n_ageoAj[i] + 
  n_ageiAj[i] + 
  n_Ij_Aj[i] + 
  n_Rj_Aj[i] - 
  n_Aj_Rj[i] - 
  n_muAj[i]

update(Rj[]) <- 
  Rj[i] - 
  n_ageoRj[i] + 
  n_ageiRj[i] + 
  n_Aj_Rj[i] - 
  n_Rj_S[i] -
  n_Rj_Ekj[i] - 
  n_muRj[i] - 
  n_Rj_Aj[i]


# Strain 2 infected(non-GII.4) with strain 1 protection
update(Ekj[]) <-
  Ekj[i] - 
  n_ageoEkj[i] + 
  n_ageiEkj[i] + 
  n_Rj_Ekj[i] - 
  n_Ekj_Ikj[i] -
  n_muEkj[i]

update(Ikj[]) <-
  Ikj[i]- 
  n_ageoIkj[i] + 
  n_ageiIkj[i] + 
  n_Ekj_Ikj[i] - 
  n_Ikj_Akj[i] - 
  n_muIkj[i]

update(Akj[]) <-  
  Akj[i] - 
  n_ageoAkj[i] + 
  n_ageiAkj[i] + 
  n_Ikj_Akj[i] - 
  n_Akj_Rjk[i] - 
  n_muAkj[i]

# Strain 1 infected (GII.4) with strain 2 protection
update(Ejk[]) <- 
  Ejk[i] - 
  n_ageoEjk[i] + 
  n_ageiEjk[i] + 
  n_Rk_Ejk[i] - 
  n_Ejk_Ijk[i] - 
  n_muEjk[i]

update(Ijk[]) <- 
  Ijk[i] - 
  n_ageoIjk[i] + 
  n_ageiIjk[i] + 
  n_Ejk_Ijk[i] - 
  n_Ijk_Ajk[i] - 
  n_muIjk[i]

update(Ajk[]) <-  
  Ajk[i] - 
  n_ageoAjk[i] + 
  n_ageiAjk[i] + 
  n_Ijk_Ajk[i] -
  n_Ajk_Rjk[i] - 
  n_muAjk[i]

# Recovered and transiently protected from all strains
update(Rjk[]) <- 
  Rjk[i] - 
  n_ageoRjk[i] + 
  n_ageiRjk[i] + 
  n_Ajk_Rjk[i] +
  n_Akj_Rjk[i] - 
  n_Rjk_S[i] - 
  n_muRjk[i]

# Strain 2 infected (non-GII.4) 
update(Ek[]) <-
  Ek[i] - 
  n_ageoEk[i] + 
  n_ageiEk[i] + 
  n_S_Ek[i] - 
  n_Ek_Ik[i] - 
  n_muEk[i] 

update(Ik[]) <- 
  Ik[i] - 
  n_ageoIk[i] + 
  n_ageiIk[i] + 
  n_Ek_Ik[i] - 
  n_Ik_Ak[i] - 
  n_muIk[i]


update(Ak[]) <- 
  Ak[i] - 
  n_ageoAk[i] + 
  n_ageiAk[i] + 
  n_Ik_Ak[i] + 
  n_Rk_Ak[i] - 
  n_Ak_Rk[i]  - 
  n_muAk[i] 

update(Rk[]) <-  
  Rk[i] - 
  n_ageoRk[i] + 
  n_ageiRk[i] + 
  n_Ak_Rk[i] - 
  n_Rk_S[i] - 
  n_Rk_Ejk[i] - 
  n_muRk[i] - 
  n_Rk_Ak[i]


######### Outputs

# Daily infections incidence
update(infections_day_j) <- 
  sum(n_Ej_Ij) + 
  sum(n_Ejk_Ijk)

update(infections_day_k) <- 
  sum(n_Ekj_Ikj) + 
  sum(n_Ek_Ik)



# Weekly reported cases (match sgss)
update(reported_wk) <- if (step %% steps_per_week == 0)
  (sum(n_Ej_Ij) + 
     sum(n_Ekj_Ikj) + 
     sum(n_Ek_Ik) + 
     sum(n_Ejk_Ijk)) * (1/repfac) else  
       reported_wk + 
  (sum(n_Ej_Ij) + 
     sum(n_Ekj_Ikj) + 
     sum(n_Ek_Ik) + 
     sum(n_Ejk_Ijk)) * (1/repfac) 

# # Weekly reported cases in under 4 (match sgss)
# update(reported_wk0_4) <- if (step %% steps_per_week == 0)
#   sum(n_EI[1:4])*(1/repfac_04) else  (1/repfac_04)*(reported_wk0_4 + sum(n_EI[1:4])) 
# 
# # Weekly reported cases in under 4-65 (match sgss)
# update(reported_wk5_65) <- if (step %% steps_per_week == 0)
#   sum(n_EI[5:13])*(1/repfac) else  (1/repfac)*(reported_wk5_65 + sum(n_EI[5:13])) 
# 
# # Weekly reported cases in under 65p (match sgss)
# update(reported_wk65_p) <- if (step %% steps_per_week == 0)
#   sum(n_EI[14])*(1/repfac_65p) else  (1/repfac_65p)*(reported_wk65_p + sum(n_EI[14])) 


## compute cumulative incidence ( match IDD2 data)
# update(cumu_inc[]) <- if (step <= (index_idd2/dt)) 0 else
#   if(i==1)  sum(cumu_inc[1:4]) + sum(n_EI[1:4]) else
#     if(i==2) sum(cumu_inc[5:8]) + sum(n_EI[5:8])  else
#       if (i==3) sum(cumu_inc[9:13]) + sum(n_EI[9:13])  else
#         if (i==4) sum(cumu_inc[14]) + sum(n_EI[14])  else 0
#### 
update(cases_day_str1[]) <- 
  (if(i==1)  sum(n_Ej_Ij[1]) + sum(n_Ejk_Ijk[1])  else
    if(i==2)  sum(n_Ej_Ij[2:4]) + sum(n_Ejk_Ijk[2:4])  else 
      if(i==3)  sum(n_Ej_Ij[5:8]) + sum(n_Ejk_Ijk[5:8])  else 
        if (i==4) sum(n_Ej_Ij[9:13]) + sum(n_Ejk_Ijk[9:13])  else
          sum(n_Ej_Ij[14]) + sum(n_Ejk_Ijk[14]) ) 


update(cases_day_str2[]) <- 
  (if(i==1)  sum(n_Ek_Ik[1]) + sum(n_Ekj_Ikj[1])  else
    if(i==2)  sum(n_Ek_Ik[2:4]) + sum(n_Ekj_Ikj[2:4])  else 
      if(i==3)  sum(n_Ek_Ik[5:8]) + sum(n_Ekj_Ikj[5:8])  else 
        if (i==4) sum(n_Ek_Ik[9:13]) + sum(n_Ekj_Ikj[9:13])  else
          sum(n_Ek_Ik[14]) + sum(n_Ekj_Ikj[14]) ) 


######
update(cases_year_str1[]) <- if (step %% steps_per_year==0)
  (if(i==1)  sum(n_Ej_Ij[1]) + sum(n_Ejk_Ijk[1])  else
    if(i==2)  sum(n_Ej_Ij[2:4]) + sum(n_Ejk_Ijk[2:4])  else 
      if(i==3)  sum(n_Ej_Ij[5:8]) + sum(n_Ejk_Ijk[5:8])  else 
        if (i==4) sum(n_Ej_Ij[9:13]) + sum(n_Ejk_Ijk[9:13])  else
          sum(n_Ej_Ij[14]) + sum(n_Ejk_Ijk[14]) ) else
            (if(i==1)  cases_year_str1[1] + sum(n_Ej_Ij[1]) + sum(n_Ejk_Ijk[1]) else
              if(i==2)  cases_year_str1[2] + sum(n_Ej_Ij[2:4]) + sum(n_Ejk_Ijk[2:4]) else
                if(i==3) cases_year_str1[3] + sum(n_Ej_Ij[5:8]) + sum(n_Ejk_Ijk[5:8])  else 
                  if (i==4)  cases_year_str1[4] + sum(n_Ej_Ij[9:13]) + sum(n_Ejk_Ijk[9:13])  else
                    cases_year_str1[5] + sum(n_Ej_Ij[14]) + sum(n_Ejk_Ijk[14]) )

update(cases_year_str2[]) <- if (step %% steps_per_year==0)
  (if(i==1)  sum(n_Ek_Ik[1]) + sum(n_Ekj_Ikj[1])  else
    if(i==2)  sum(n_Ek_Ik[2:4]) + sum(n_Ekj_Ikj[2:4])  else 
      if(i==3)  sum(n_Ek_Ik[5:8]) + sum(n_Ekj_Ikj[5:8])  else 
        if (i==4) sum(n_Ek_Ik[9:13]) + sum(n_Ekj_Ikj[9:13])  else
          sum(n_Ek_Ik[14]) + sum(n_Ekj_Ikj[14]) ) else
            (if(i==1)  cases_year_str2[1] + sum(n_Ek_Ik[1]) + sum(n_Ekj_Ikj[1]) else
              if(i==2)  cases_year_str2[2] + sum(n_Ek_Ik[2:4]) + sum(n_Ekj_Ikj[2:4]) else
                if(i==3) cases_year_str2[3] + sum(n_Ek_Ik[5:8]) + sum(n_Ekj_Ikj[5:8])  else 
                  if (i==4)  cases_year_str2[4] + sum(n_Ek_Ik[9:13]) + sum(n_Ekj_Ikj[9:13])  else
                    cases_year_str2[5] + sum(n_Ek_Ik[14]) + sum(n_Ekj_Ikj[14]) )


## compute at risk population ( match IDD2 data)
update(person_year[]) <- if (step %% steps_per_year==0)
  (if(i==1)  sum(Nrisk[1]) else
    if(i==2)  sum(Nrisk[2:4])  else 
      if(i==3)  sum(Nrisk[5:8])  else 
        if (i==4)  sum(Nrisk[9:13])  else
          sum(Nrisk[14]) ) else
            (if(i==1)  person_year[1] + sum(Nrisk[1]) else
              if(i==2) person_year[2] + sum(Nrisk[2:4])  else 
                if(i==3) person_year[3] + sum(Nrisk[5:8])   else 
                  if (i==4)  person_year[4] + sum(Nrisk[9:13])  else
                    person_year[5] + sum(Nrisk[14]) )


## compute prevalence 
update(seroprev_num[]) <- if (step %% steps_per_year==0)
  Rj[i]+M[i]+Rjk[i]+Rk[i] else
    seroprev_num[i] + Rj[i]+M[i]+Rjk[i]+Rk[i]

update(seroprev_den[]) <- if (step %% steps_per_year==0)
  (M[i]+G[i]+S[i]+
     Ej[i]+Ij[i]+Aj[i]+Rj[i]+
     Ejk[i]+Ijk[i]+Ajk[i]+
     Ekj[i]+Ikj[i]+Akj[i]+
     Ek[i]+Ik[i]+Ak[i]+Rk[i]+
     Rjk[i]) else
       seroprev_den[i] + (M[i]+G[i]+S[i]+
                            Ej[i]+Ij[i]+Aj[i]+Rj[i]+
                            Ejk[i]+Ijk[i]+Ajk[i]+
                            Ekj[i]+Ikj[i]+Akj[i]+
                            Ek[i]+Ik[i]+Ak[i]+Rk[i]+
                            Rjk[i])

update(seroprev[]) <- 
  Rj[i]+M[i]+Rjk[i]+Rk[i]/(M[i]+G[i]+S[i]+
                             Ej[i]+Ij[i]+Aj[i]+Rj[i]+
                             Ejk[i]+Ijk[i]+Ajk[i]+
                             Ekj[i]+Ikj[i]+Akj[i]+
                             Ek[i]+Ik[i]+Ak[i]+Rk[i]+
                             Rjk[i]) 

## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_aging[]<- 1 - exp(-aging_vec[i] * dt)
p_MS[] <- 1 - exp(-(1/delta) * dt)  # M to S
p_SEj[] <- 1 - exp(-lambda_j[i] * dt) # S to E
p_SEk[] <- 1 - exp(-lambda_k[i] * dt) # S to E
p_RjAj[] <- 1 - exp(-lambda_j[i] * (1-alpha) * dt)
p_RkAk[] <- 1 - exp(-lambda_k[i] * (1-alpha) * dt)
p_RjEkj[] <- 1 - exp(-lambda_k[i] * (1-crossp_kj) * dt)
p_RkEjk[] <- 1 - exp(-lambda_j[i] * (1-crossp_jk) * dt)
p_EI   <- 1 - exp(-(1/epsilon) * dt) # E to I
p_IA_5    <- 1 - exp(-(1/theta_5) * dt) # I to A
p_IA_5p   <- 1 - exp(-(1/theta_5p) * dt) # I to A
p_AR   <- 1 - exp(-(1/sigma) * dt) # A to R
p_RS   <- 1 - exp(- (1/(tau*365)) * dt) # R to S
N      <- sum(M) + sum(G)+sum(S)+sum(Ej)+sum(Ij)+sum(Aj)+sum(Rj)+
  sum(Ejk)+sum(Ijk)+sum(Ajk)+sum(Rjk)+
  sum(Ek)+sum(Ik)+sum(Ak)+sum(Rk)+
  sum(Ekj)+sum(Ikj)+sum(Akj)


Nrisk[] <- M[i] + G[i] + S[i] + Ej[i]+ Aj[i]+Rj[i]+
  Ejk[i]+Ajk[i]+Rjk[i]+
  Ek[i]+Ak[i]+Rk[i]+
  Ekj[i]+Akj[i]

prev   <- (sum(Ej)+sum(Ij)+sum(Aj)+sum(Rj)+
             sum(Ejk)+sum(Ijk)+sum(Ajk)+sum(Rjk)+
             sum(Ek)+sum(Ik)+sum(Ak)+sum(Rk)+
             sum(Ekj)+sum(Ikj)+sum(Akj))/N  

p_birth[1]   <- (1-p_nonsecretor)*prev
p_birth[2]   <- p_nonsecretor
p_birth[3]   <- (1-p_nonsecretor)*(1-prev) 

dim(Nrisk)<-N_age
## Force of infection
m[, ] <- user() # age-structured contact matrix
m_holi[,]<-user()
#switcher<-school[as.integer(step)] 

switcher <- if (as.integer(step) >= n_school_steps)
  school_step[n_school_steps]*
  (1- (as.integer(step)-n_school_steps)) else 
    school_step[as.integer(step) + 1] 


cj_ij[, ] <- (Ij[j] + Ijk[j] + rho *(Aj[j] + Ej[j] + Ajk[j] + Ejk[j] ) ) *
  (switcher*m[i, j]  + (1-switcher)*m_holi[i, j])

ck_ij[, ] <- (Ik[j] + Ikj[j] + rho *(Ak[j] + Ek[j] + Akj[j] + Ekj[j] ) ) *
  (switcher*m[i, j]  + (1-switcher)*m_holi[i, j])

# if (school[as.integer(step)]==1) m[i, j] else m_holi[i, j]

beta_j_t <- beta_j *(1 + w1_j*cos((2*pi*time)/364 + w2*pi))
beta_k_t <- beta_k *(1 + w1_k*cos((2*pi*time)/364 + w2*pi))
lambda_j[] <- beta_j_t  * sum(cj_ij[i , ])
lambda_k[] <- beta_k_t  * sum(ck_ij[i , ])



########### Aging numbers ::::::::::::::::::::::::
#Age out
n_ageoG[] <- rbinom(G[i] , p_aging[i])
n_ageoM[] <- rbinom(M[i] , p_aging[i])
n_ageoS[] <- rbinom(S[i] , p_aging[i])
n_ageoEj[] <- rbinom(Ej[i] , p_aging[i])
n_ageoIj[] <- rbinom(Ij[i] , p_aging[i])
n_ageoAj[] <- rbinom(Aj[i] , p_aging[i])
n_ageoRj[] <- rbinom(Rj[i] , p_aging[i])
n_ageoEkj[] <- rbinom(Ekj[i], p_aging[i])
n_ageoIkj[] <- rbinom(Ikj[i] , p_aging[i])
n_ageoAkj[] <- rbinom(Akj[i] , p_aging[i])
n_ageoEjk[] <- rbinom(Ejk[i] , p_aging[i])
n_ageoIjk[] <- rbinom(Ijk[i] , p_aging[i])
n_ageoAjk[] <- rbinom(Ajk[i] , p_aging[i])
n_ageoRjk[] <- rbinom(Rjk[i] , p_aging[i])
n_ageoEk[] <- rbinom(Ek[i] , p_aging[i])
n_ageoIk[] <- rbinom(Ik[i] , p_aging[i])
n_ageoAk[] <- rbinom(Ak[i] , p_aging[i])
n_ageoRk[] <- rbinom(Rk[i] , p_aging[i])
#Age in
n_ageiG[] <- if (i>1) rbinom(G[i-1] , p_aging[i-1]) else 0
n_ageiM[] <- if (i>1) rbinom(M[i-1] , p_aging[i-1])else 0
n_ageiS[] <- if (i>1)rbinom(S[i-1] , p_aging[i-1])else 0
n_ageiEj[] <- if (i>1)rbinom(Ej[i-1] , p_aging[i-1])else 0
n_ageiIj[] <- if (i>1)rbinom(Ij[i-1] , p_aging[i-1])else 0
n_ageiAj[] <- if (i>1)rbinom(Aj[i-1] , p_aging[i-1])else 0
n_ageiRj[] <- if (i>1)rbinom(Rj[i-1] , p_aging[i-1])else 0
n_ageiEkj[] <- if (i>1)rbinom(Ekj[i-1] , p_aging[i-1])else 0
n_ageiIkj[] <- if (i>1)rbinom(Ikj[i-1] , p_aging[i-1])else 0
n_ageiAkj[] <- if (i>1)rbinom(Akj[i-1] , p_aging[i-1])else 0
n_ageiEjk[] <- if (i>1)rbinom(Ejk[i-1] , p_aging[i-1])else 0
n_ageiIjk[] <- if (i>1)rbinom(Ijk[i-1] , p_aging[i-1])else 0
n_ageiAjk[] <- if (i>1)rbinom(Ajk[i-1] , p_aging[i-1])else 0
n_ageiRjk[] <- if (i>1)rbinom(Rjk[i-1] , p_aging[i-1])else 0
n_ageiEk[] <- if (i>1)rbinom(Ek[i-1] , p_aging[i-1])else 0
n_ageiIk[] <- if (i>1)rbinom(Ik[i-1] , p_aging[i-1])else 0
n_ageiAk[] <- if (i>1)rbinom(Ak[i-1] , p_aging[i-1])else 0
n_ageiRk[] <- if (i>1)rbinom(Rk[i-1] , p_aging[i-1])else 0

## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i]-n_ageoG[i], p_mu[i])

n_muM[]<- rbinom(M[i]-n_ageoM[i], p_mu[i])
n_MS[] <- rbinom(M[i]-n_ageoM[i]-n_muM[i],p_MS[i])

n_muS[]  <- rbinom(S[i]-n_ageoS[i], p_mu[i])
n_S_Ej[] <- rbinom(S[i]-n_ageoS[i]-n_muS[i], p_SEj[i])
n_S_Ek[] <- rbinom(S[i]-n_ageoS[i]-n_muS[i]-n_S_Ej[i], p_SEk[i])

# j
n_muEj[]<- rbinom(Ej[i]-n_ageoEj[i], p_mu[i])
n_Ej_Ij[] <- rbinom(Ej[i]-n_ageoEj[i]-n_muEj[i], p_EI)

n_muIj[]<- rbinom(Ij[i]-n_ageoIj[i], p_mu[i])
n_Ij_Aj[] <- rbinom(Ij[i]-n_ageoIj[i]-n_muIj[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAj[]  <- rbinom(Aj[i]-n_ageoAj[i], p_mu[i])
n_Aj_Rj[] <- rbinom(Aj[i]-n_ageoAj[i]-n_muAj[i], p_AR)

n_muRj[]   <- rbinom(Rj[i]-n_ageoRj[i], p_mu[i])
n_Rj_Aj[]  <- rbinom(Rj[i]-n_ageoRj[i]-n_muRj[i], p_RjAj[i])
n_Rj_S[]   <- rbinom(Rj[i]-n_ageoRj[i]-n_muRj[i]-n_Rj_Aj[i], p_RS)
n_Rj_Ekj[] <- rbinom(Rj[i]-n_ageoRj[i]-n_muRj[i]-n_Rj_Aj[i]-n_Rj_S[i], p_RjEkj[i])

# kj
n_muEkj[]   <- rbinom(Ekj[i]-n_ageoEkj[i], p_mu[i])
n_Ekj_Ikj[] <- rbinom(Ekj[i]-n_ageoEkj[i]-n_muEkj[i], p_EI)

n_muIkj[]   <- rbinom(Ikj[i]-n_ageoIkj[i], p_mu[i])
n_Ikj_Akj[] <- rbinom(Ikj[i]-n_ageoIkj[i]-n_muIkj[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAkj[]   <- rbinom(Akj[i]-n_ageoAkj[i], p_mu[i])
n_Akj_Rjk[] <- rbinom(Akj[i]-n_ageoAkj[i]-n_muAkj[i], p_AR)

#jk
n_muEjk[]   <- rbinom(Ejk[i]-n_ageoEjk[i], p_mu[i])
n_Ejk_Ijk[] <- rbinom(Ejk[i]-n_ageoEjk[i]-n_muEjk[i], p_EI)

n_muIjk[]   <- rbinom(Ijk[i]-n_ageoIjk[i], p_mu[i])
n_Ijk_Ajk[] <- rbinom(Ijk[i]-n_ageoIjk[i]-n_muIjk[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAjk[]   <- rbinom(Ajk[i]-n_ageoAjk[i], p_mu[i])
n_Ajk_Rjk[] <- rbinom(Ajk[i]-n_ageoAjk[i]-n_muAjk[i], p_AR)

# Rjk
n_muRjk[]   <- rbinom(Rjk[i]-n_ageoRjk[i], p_mu[i])
n_Rjk_S[]   <- rbinom(Rjk[i]-n_ageoRjk[i]-n_muRjk[i], p_RS)

#k
n_muEk[]  <- rbinom(Ek[i]-n_ageoEk[i], p_mu[i])
n_Ek_Ik[] <- rbinom(Ek[i]-n_ageoEk[i]-n_muEk[i], p_EI)

n_muIk[]  <- rbinom(Ik[i]-n_ageoIk[i], p_mu[i])
n_Ik_Ak[] <- rbinom(Ik[i]-n_ageoIk[i]-n_muIk[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAk[]  <- rbinom(Ak[i]-n_ageoAk[i], p_mu[i])
n_Ak_Rk[] <- rbinom(Ak[i]-n_ageoAk[i]-n_muAk[i], p_AR)

n_muRk[]   <- rbinom(Rk[i]-n_ageoRk[i], p_mu[i])
n_Rk_Ak[]  <- rbinom(Rk[i]-n_ageoRk[i]-n_muRk[i], p_RkAk[i])
n_Rk_S[]   <- rbinom(Rk[i]-n_ageoRk[i]-n_muRk[i]-n_Rk_Ak[i], p_RS)
n_Rk_Ejk[] <- rbinom(Rk[i]-n_ageoRk[i]-n_muRk[i]-n_Rk_Ak[i]-n_Rk_S[i], p_RkEjk[i])

# Deaths 
n_allDeath<- 
  sum(n_muM) + 
  sum(n_muG)+
  sum(n_muS)+
  sum(n_muEj)+
  sum(n_muIj)+
  sum(n_muAj)+
  sum(n_muRj)+
  sum(n_muEjk)+
  sum(n_muIjk)+
  sum(n_muAjk)+
  sum(n_muRjk)+
  sum(n_muEkj)+
  sum(n_muIkj)+
  sum(n_muAkj)+
  sum(n_muEk)+
  sum(n_muIk)+
  sum(n_muAk)+
  sum(n_muRk)

# Births to keep stable population equal to deaths
n_bM[] <- if (i==1) n_allDeath*p_birth[1] else 0 
n_bG[] <- if (i==1) n_allDeath*p_birth[2] else 0 
n_bS[] <- if (i==1) n_allDeath*p_birth[3] else 0 


## Initial states:


initial(M[]) <- init[1,i]
initial(G[]) <- init[2,i]
initial(S[]) <- init[3,i]
initial(Ej[]) <- init[4,i]
initial(Ij[]) <- init[5,i]
initial(Aj[]) <- init[6,i]
initial(Rj[]) <- init[7,i]
initial(Ekj[]) <- init[8,i]
initial(Ikj[]) <- init[9,i]
initial(Akj[]) <- init[10,i]
initial(Ejk[]) <- init[11,i]
initial(Ijk[]) <- init[12,i]
initial(Ajk[]) <- init[13,i]
initial(Rjk[]) <- init[14,i]
initial(Ek[]) <- init[15,i]
initial(Ik[]) <- init[16,i]
initial(Ak[]) <- init[17,i]
initial(Rk[]) <- init[18,i]

# Outputs
initial(cases_year_str1[]) <- 0
initial(cases_year_str2[]) <- 0
initial(cases_day_str1[]) <- 0
initial(cases_day_str2[]) <- 0
initial(person_year[]) <- 0
initial(seroprev[]) <-0
initial(seroprev_num[]) <-0
initial(seroprev_den[]) <-1
initial(infections_day_j)<-0
initial(infections_day_k)<-0
initial(reported_wk)<-0
#initial(infections_tot)<-0
# initial(reported_wk0_4)<-0
# initial(reported_wk5_65)<-0
# initial(reported_wk65_p)<-0

## User defined parameters - default in parentheses:
init[,] <- user()

beta_j <- user(0.2)   # transm coefficient
beta_k <- user(0.2)   # transm coefficient
#repfac_04<-user(287)
repfac<- user(287)    # reported to community factor
#repfac_65p<-user(287)
delta <- user(180)  # maternal Ab decay (days)
epsilon <- user(1)   # incubation
theta_5 <- user(2.5)   # duration symptoms
theta_5p <- user(1.5)   # duration symptoms
sigma <- user(15) # duration asymp shedding
tau   <- user(5.1)     # duration immunity
rho   <- user(0.05) # rel infect asymptomatic 
crossp_kj<-user(0.05) # cross_protection prob from j to k
crossp_jk<-user(0.05) # cross_protection prob from k to j
p_nonsecretor<-user(0.2) # Fraction immune genetically
w1_j <-user(0.15) # sesonality
w1_k <-user(0.15) # sesonality
w2 <-user(0) # sesonality
pi <-user(3.141593)
alpha<-user(0)# rel susc in R 
mu[]  <- user()      # mortality rates 
aging_vec[]<-user() # aging transitions matrix
school_step[]<-user()
n_school_steps<-user()
#
# dimensions of arrays
N_age <- user()
dim(school_step)<-user()
dim(init) <-  c(18,N_age)
dim(aging_vec)<- N_age
dim(M) <- N_age
dim(G) <- N_age
dim(S) <- N_age
dim(Ej) <- N_age
dim(Ij) <- N_age
dim(Aj) <- N_age
dim(Rj) <- N_age
dim(Ekj) <- N_age
dim(Ikj) <- N_age
dim(Akj) <- N_age
dim(Ejk) <- N_age
dim(Ijk) <- N_age
dim(Ajk) <- N_age
dim(Rjk) <- N_age
dim(Ek) <- N_age
dim(Ik) <- N_age
dim(Ak) <- N_age
dim(Rk) <- N_age

dim(n_ageoG)<-N_age
dim(n_ageoM)<-N_age
dim(n_ageoS)<-N_age
dim(n_ageoEj)<-N_age
dim(n_ageoIj)<-N_age
dim(n_ageoAj)<-N_age
dim(n_ageoRj)<-N_age
dim(n_ageoEkj)<-N_age
dim(n_ageoIkj)<-N_age
dim(n_ageoAkj)<-N_age
dim(n_ageoEjk)<-N_age
dim(n_ageoIjk)<-N_age
dim(n_ageoAjk)<-N_age
dim(n_ageoRjk)<-N_age
dim(n_ageoEk)<-N_age
dim(n_ageoIk)<-N_age
dim(n_ageoAk)<-N_age
dim(n_ageoRk)<-N_age

dim(n_ageiG)<-N_age
dim(n_ageiM)<-N_age
dim(n_ageiS)<-N_age
dim(n_ageiEj)<-N_age
dim(n_ageiIj)<-N_age
dim(n_ageiAj)<-N_age
dim(n_ageiRj)<-N_age
dim(n_ageiEkj)<-N_age
dim(n_ageiIkj)<-N_age
dim(n_ageiAkj)<-N_age
dim(n_ageiEjk)<-N_age
dim(n_ageiIjk)<-N_age
dim(n_ageiAjk)<-N_age
dim(n_ageiRjk)<-N_age
dim(n_ageiEk)<-N_age
dim(n_ageiIk)<-N_age
dim(n_ageiAk)<-N_age
dim(n_ageiRk)<-N_age



#dim(cumu_inc) <- N_age
dim(seroprev) <- 7
dim(seroprev_num) <- 7
dim(seroprev_den) <- 7
dim(cases_year_str1)<- 5
dim(cases_year_str2)<- 5
dim(cases_day_str1)<- 5
dim(cases_day_str2)<- 5
dim(person_year)<- 5
#dim(n_risk) <- N_age
dim(n_muM) <- N_age
dim(n_muG) <- N_age
dim(n_muS) <- N_age
dim(n_muEj) <- N_age
dim(n_muIj) <- N_age
dim(n_muAj) <- N_age
dim(n_muRj) <- N_age
dim(n_muEkj) <- N_age
dim(n_muIkj) <- N_age
dim(n_muAkj) <- N_age
dim(n_muEjk) <- N_age
dim(n_muIjk) <- N_age
dim(n_muAjk) <- N_age
dim(n_muRjk) <- N_age
dim(n_muEk) <- N_age
dim(n_muIk) <- N_age
dim(n_muAk) <- N_age
dim(n_muRk) <- N_age
dim(n_bM) <- N_age
dim(n_bG) <- N_age
dim(n_bS) <- N_age
dim(n_MS) <- N_age
dim(n_S_Ej)<- N_age 
dim(n_S_Ek) <- N_age 
dim(n_Ej_Ij) <- N_age 
dim(n_Ij_Aj) <- N_age 
dim(n_Aj_Rj) <- N_age 
dim(n_Rj_Aj) <- N_age
dim(n_Rj_S) <- N_age
dim(n_Rj_Ekj) <- N_age 
dim(n_Ekj_Ikj) <- N_age 
dim(n_Ikj_Akj) <- N_age 
dim(n_Akj_Rjk) <- N_age 
dim(n_Ejk_Ijk) <- N_age 
dim(n_Ijk_Ajk) <- N_age 
dim(n_Ajk_Rjk) <- N_age 
dim(n_Rjk_S) <- N_age   
dim(n_Ek_Ik) <- N_age 
dim(n_Ik_Ak) <- N_age 
dim(n_Ak_Rk) <- N_age 
dim(n_Rk_Ak) <- N_age 
dim(n_Rk_S) <- N_age
dim(n_Rk_Ejk ) <- N_age 
dim(p_birth)   <- 3
dim(p_mu) <- N_age
dim(p_aging) <- N_age
dim(p_MS) <- N_age
dim(p_SEj) <- N_age
dim(p_SEk) <- N_age
dim(p_RjAj) <- N_age
dim(p_RkAk) <- N_age
dim(p_RjEkj) <- N_age
dim(p_RkEjk) <- N_age
dim(mu)<-N_age
dim(m) <- c(N_age, N_age)
dim(m_holi) <- c(N_age, N_age)
dim(cj_ij) <- c(N_age, N_age)
dim(ck_ij) <- c(N_age, N_age)
dim(lambda_j) <- N_age
dim(lambda_k) <- N_age

