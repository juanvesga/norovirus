## Definition of the time-step and output as "time"
dt <- user(1)
steps_per_day <- 1/dt
steps_per_week<- 7/dt
steps_per_year<- 365/dt
initial(time) <- 0
update(time) <- (step + 1) * dt

## Equations for transitions between compartments by age group
# Not infected
update(M[]) <- M[i] + n_bM[i] - n_MS[i] - n_muM[i] + n_ageM[i]
update(G[]) <- G[i] + n_bG[i] - n_muG[i] + n_ageG[i]
update(S[]) <- S[i] + n_bS[i] + n_MS[i] + n_Rj_S[i] + n_Rjk_S[i] + n_Rk_S[i] - 
  n_S_Ej[i] - n_S_Ek[i] - n_muS[i] + n_ageS[i] 

# Strain 1infected(GII.4)
update(Ej[]) <- Ej[i] + n_S_Ej[i] - n_Ej_Ij[i] - n_muEj[i] + n_ageEj[i]
update(Ij[]) <- Ij[i] + n_Ej_Ij[i] - n_Ij_Aj[i] - n_muIj[i] + n_ageIj[i]
update(Aj[]) <- Aj[i] + n_Ij_Aj[i] + n_Rj_Aj[i] - n_Aj_Rj[i]  - n_muAj[i] + 
  n_ageAj[i]
update(Rj[]) <- Rj[i] + n_Aj_Rj[i] - n_Rj_S[i] - n_Rj_Ekj[i] - n_muRj[i] - 
  n_Rj_Aj[i]   + n_ageRj[i]

# Strain 2 infected(non-GII.4) with strain 1 protection
update(Ekj[]) <- Ekj[i] + n_Rj_Ekj[i] - n_Ekj_Ikj[i] - n_muEkj[i] + n_ageEkj[i]
update(Ikj[]) <- Ikj[i] + n_Ekj_Ikj[i] - n_Ikj_Akj[i] - n_muIkj[i] + n_ageIkj[i]
update(Akj[]) <- Akj[i] + n_Ikj_Akj[i] - n_Akj_Rjk[i]  - n_muAkj[i] + n_ageAkj[i]

# Strain 1 infected (GII.4) with strain 2 protection
update(Ejk[]) <- Ejk[i] + n_Rk_Ejk[i] - n_Ejk_Ijk[i] - n_muEjk[i] + n_ageEjk[i]
update(Ijk[]) <- Ijk[i] + n_Ejk_Ijk[i] - n_Ijk_Ajk[i] - n_muIjk[i] + n_ageIjk[i]
update(Ajk[]) <- Akj[i] + n_Ijk_Ajk[i] - n_Ajk_Rjk[i]  - n_muAjk[i] + n_ageAjk[i]

# Recovered and transiently protected from all strains
update(Rjk[]) <- Rjk[i] + n_Ajk_Rjk[i] + n_Akj_Rjk[i] - n_Rjk_S[i] - n_muRjk[i] -
  + n_ageRjk[i]

# Strain 2 infected (non-GII.4) 
update(Ek[]) <- Ek[i] + n_S_Ek[i] - n_Ek_Ik[i] - n_muEk[i] + n_ageEk[i]
update(Ik[]) <- Ik[i] + n_Ek_Ik[i] - n_Ik_Ak[i] - n_muIk[i] + n_ageIk[i]
update(Ak[]) <- Ak[i] + n_Ik_Ak[i] + n_Rk_Ak[i] - n_Ak_Rk[i]  - n_muAk[i] + 
  n_ageAk[i]
update(Rk[]) <- Rk[i] + n_Ak_Rk[i] - n_Rk_S[i] - n_Rk_Ejk[i] - n_muRk[i] - 
  n_Rk_Ak[i]   + n_ageRk[i]


######### Outputs

# Daily infections incidence
update(infections_day) <- 
  sum(n_EI) + 
  sum(n_Ekj_Ikj) + 
  sum(n_Ek_Ik) + 
  sum(n_Ejk_Ijk)

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
# 

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
              if(i==2)  cases_year_str2[2] + sum(n_Ek_Ik[2:4]) + sum(n_Ekj_Ijk[2:4]) else
                if(i==3) cases_year_str2[3] + sum(n_Ek_Ik[5:8]) + sum(n_Ekj_Ikj[5:8])  else 
                  if (i==4)  cases_year_str2[4] + sum(n_Ek_Ik[9:13]) + sum(n_Ekj_Ikj[9:13])  else
                    cases_year_str2[5] + sum(n_Ek_Ik[14]) + sum(n_Ekj_Ikj[14]) )


## compute at risk population ( match IDD2 data)
update(person_year) <- if (step %% steps_per_year==0)
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
update(seroprev[]) <- (Rj[i]+M[i]+Rjk[i]+Rk[i])/(M[i]+G[i]+S[i]+
                                                   Ej[i]+Ij[i]+Aj[i]+Rj[i]+
                                                   Ejk[i]+Ijk[i]+Ajk[i]+
                                                   Ekj[i]+Ikj[i]+Akj[i]+
                                                   Ek[i]+Ik[i]+Ak[i]+Rk[i]+
                                                   Rjk[i])


## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_MS[] <- 1 - exp(-(1/delta) * dt)  # M to S
p_SEj[] <- 1 - exp(-lambda_j[i] * dt) # S to E
p_SEk[] <- 1 - exp(-lambda_k[i] * dt) # S to E
p_RjAj[] <- 1 - exp(-lambda_j[i] * (1-alpha) * dt)
p_RkAk[] <- 1 - exp(-lambda_k[i] * (1-alpha) * dt)
p_RjEkj[] <- 1 - exp(-lambda_k[i] * (1-gamma_kj) * dt)
p_RkEjk[] <- 1 - exp(-lambda_k[i] * (1-gamma_jk) * dt)
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

prev   <- (sum(E)+sum(I)+sum(A)+sum(R))/N  
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

beta_j_t <- beta_j *(1 + w1*cos((2*pi*time)/364 + w2*pi))
beta_k_t <- beta_k *(1 + w1*cos((2*pi*time)/364 + w2*pi))
lambda_j[] <- beta_j_t  * sum(cj_ij[i , ])
lambda_k[] <- beta_k_t  * sum(ck_ij[i , ])


# Aging transitions
m_ij[,] <- aging_mat[i,j]*M[j] * dt
g_ij[,] <- aging_mat[i,j]*G[j] * dt
s_ij[,] <- aging_mat[i,j]*S[j] * dt
ej_ij[,] <- aging_mat[i,j]*Ej[j] * dt
ij_ij[,] <- aging_mat[i,j]*Ij[j] * dt
aj_ij[,] <- aging_mat[i,j]*Aj[j] * dt
rj_ij[,] <- aging_mat[i,j]*Rj[j] * dt
ekj_ij[,] <- aging_mat[i,j]*Ekj[j] * dt
ikj_ij[,] <- aging_mat[i,j]*Ikj[j] * dt
akj_ij[,] <- aging_mat[i,j]*Akj[j] * dt
ejk_ij[,] <- aging_mat[i,j]*Ejk[j] * dt
ijk_ij[,] <- aging_mat[i,j]*Ijk[j] * dt
ajk_ij[,] <- aging_mat[i,j]*Ajk[j] * dt
rjk_ij[,] <- aging_mat[i,j]*Rjk[j] * dt
ek_ij[,] <- aging_mat[i,j]*Ek[j] * dt
ik_ij[,] <- aging_mat[i,j]*Ik[j] * dt
ak_ij[,] <- aging_mat[i,j]*Ak[j] * dt
rk_ij[,] <- aging_mat[i,j]*Rk[j] * dt


n_ageM[]<- round(sum(m_ij[,i]))
n_ageG[]<- round(sum(g_ij[,i]))
n_ageS[]<- round(sum(s_ij[,i]))
n_ageEj[]<- round(sum(ej_ij[,i]))
n_ageIj[]<- round(sum(ij_ij[,i]))
n_ageAj[]<- round(sum(aj_ij[,i]))
n_ageRj[]<- round(sum(rj_ij[,i]))
n_ageEjk[]<- round(sum(ejk_ij[,i]))
n_ageIjk[]<- round(sum(ijk_ij[,i]))
n_ageAjk[]<- round(sum(ajk_ij[,i]))
n_ageEkj[]<- round(sum(ekj_ij[,i]))
n_ageIkj[]<- round(sum(ikj_ij[,i]))
n_ageAkj[]<- round(sum(akj_ij[,i]))
n_ageRjk[]<- round(sum(rjk_ij[,i]))
n_ageEk[]<- round(sum(ek_ij[,i]))
n_ageIk[]<- round(sum(ik_ij[,i]))
n_ageAk[]<- round(sum(ak_ij[,i]))
n_ageRk[]<- round(sum(rk_ij[,i]))

########### HERE ::::::::::::::::::::::::


## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i], p_mu[i])

n_muM[]<- rbinom(M[i], p_mu[i])
n_MS[] <- rbinom(M[i]-n_muM[i],p_MS[i])

n_muS[]  <- rbinom(S[i], p_mu[i])
n_S_Ej[] <- rbinom(S[i]-n_muS[i], p_SEj[i])
n_S_Ek[] <- rbinom(S[i]-n_muS[i]-n_S_Ej[i], p_SEk[i])

# j
n_muEj[]<- rbinom(Ej[i], p_mu[i])
n_Ej_Ij[] <- rbinom(Ej[i]-n_muEj[i], p_EI)

n_muIj[]<- rbinom(Ij[i], p_mu[i])
n_Ij_Aj[] <- rbinom(Ij[i]-n_muIj[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAj[]<- rbinom(Aj[i], p_mu[i])
n_Aj_Rj[] <- rbinom(Aj[i]-n_muAj[i], p_AR)

n_muRj[]   <- rbinom(Rj[i], p_mu[i])
n_Rj_Aj[]  <- rbinom(Rj[i]-n_muRj[i], p_RjAj[i])
n_Rj_S[]   <- rbinom(Rj[i]-n_muRj[i]-n_Rj_Aj[i], p_RS)
n_Rj_Ekj[] <- rbinom(Rj[i]-n_muRj[i]-n_Rj_Aj[i]-n_Rj_S[i], p_RjEkj[i])

# kj
n_muEkj[]<- rbinom(Ekj[i], p_mu[i])
n_Ekj_Ikj[] <- rbinom(Ekj[i]-n_muEkj[i], p_EI)

n_muIkj[]<- rbinom(Ikj[i], p_mu[i])
n_Ikj_Akj[] <- rbinom(Ikj[i]-n_muIkj[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAkj[]<- rbinom(Akj[i], p_mu[i])
n_Akj_Rkj[] <- rbinom(Akj[i]-n_muAkj[i], p_AR)

#jk
n_muEjk[]<- rbinom(Ejk[i], p_mu[i])
n_Ejk_Ijk[] <- rbinom(Ejk[i]-n_muEjk[i], p_EI)

n_muIjk[]<- rbinom(Ijk[i], p_mu[i])
n_Ijk_Ajk[] <- rbinom(Ijk[i]-n_muIjk[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAjk[]<- rbinom(Ajk[i], p_mu[i])
n_Ajk_Rjk[] <- rbinom(Ajk[i]-n_muAjk[i], p_AR)

# Rjk
n_muRjk[]  <- rbinom(Rjk[i], p_mu[i])
n_Rj_S[]   <- rbinom(Rjk[i]-n_muRjk[i], p_RS)

#k
n_muEk[]<- rbinom(Ek[i], p_mu[i])
n_Ek_Ik[] <- rbinom(Ek[i]-n_muEk[i], p_EI)

n_muIk[]<- rbinom(Ik[i], p_mu[i])
n_Ik_Ak[] <- rbinom(Ik[i]-n_muIk[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muAk[]<- rbinom(Ak[i], p_mu[i])
n_Ak_Rk[] <- rbinom(Ak[i]-n_muAk[i], p_AR)

n_muRk[]   <- rbinom(Rk[i], p_mu[i])
n_Rk_Ak[]  <- rbinom(Rk[i]-n_muRk[i], p_RkAk[i])
n_Rk_S[]   <- rbinom(Rk[i]-n_muRk[i]-n_Rk_Ak[i], p_RS)
n_Rk_Ejk[] <- rbinom(Rk[i]-n_muRk[i]-n_Rk_Ak[i]-n_Rk_S[i], p_RkEjk[i])

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
  sum(n_muRk)+
  
# Births to keep stable population equal to deaths
n_bM[] <- if (i==1) n_allDeath*p_birth[1] else 0 
n_bG[] <- if (i==1) n_allDeath*p_birth[2] else 0 
n_bS[] <- if (i==1) n_allDeath*p_birth[3] else 0 


## Initial states:
#initial(infections_tot)<-0
initial(infections_day)<-0
initial(reported_wk)<-0
# initial(reported_wk0_4)<-0
# initial(reported_wk5_65)<-0
# initial(reported_wk65_p)<-0

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
initial(Rjk[]) <- init[11,i]
initial(Ekj[]) <- init[12,i]
initial(Ikj[]) <- init[13,i]
initial(Akj[]) <- init[14,i]
initial(Ek[]) <- init[15,i]
initial(Ik[]) <- init[16,i]
initial(Ak[]) <- init[17,i]
initial(Rk[]) <- init[18,i]

#initial(cumu_inc[]) <- 0
initial(cases_year[]) <- 0
initial(person_year[]) <- 0
initial(seroprev[]) <-0
#initial(n_risk[]) <-init[1,i]+init[2,i]+init[3,i]+init[7,i]
#initial(aux)<-0

## User defined parameters - default in parentheses:
init[,] <- user()

beta <- user(0.046)   # transm coefficient
repfac_04<-user(287)
repfac<-user(287)    # reported to community factor
repfac_65p<-user(287)
delta <- user(0.04)  # maternal Ab decay
epsilon <- user(1)   # incubation
theta_5 <- user(2.5)   # duration symptoms
theta_5p <- user(1.5)   # duration symptoms
sigma <- user(15) # duration asymp shedding
tau   <- user(5.1)     # duration immunity
rho   <- user(0.05) # rel infect asymptomatic 
gamma_kj<-user(0.5) # cross_protection prob from j to k
gamma_jk<-user(0.5) # cross_protection prob from k to j
p_nonsecretor<-user(0.2) # Fraction immune genetically
w1 <-user(0.15) # sesonality
w2 <-user(0) # sesonality
pi <-user(3.141593)
alpha<-user(0)# rel susc in R 
mu[]  <- user()      # mortality rates 
aging_mat[,]<-user() # aging transitions matrix
school_step[]<-user()
n_school_steps<-user()
index_idd2<-user(822) # index to start acumulating output
#
# dimensions of arrays
N_age <- user()
dim(school_step)<-user()
dim(init) <-  c(18,N_age)
dim(aging_mat)<-c(N_age, N_age)
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
#dim(cumu_inc) <- N_age
dim(seroprev) <- N_age
dim(cases_year_str1)<- 5
dim(cases_year_str2)<- 5
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
dim(n_ageM) <- N_age
dim(n_ageG) <- N_age
dim(n_ageS) <- N_age
dim(n_ageEj) <- N_age
dim(n_ageIj) <- N_age
dim(n_ageAj) <- N_age
dim(n_ageRj) <- N_age
dim(n_ageEkj) <- N_age
dim(n_ageIkj) <- N_age
dim(n_ageAkj) <- N_age
dim(n_ageEjk) <- N_age
dim(n_ageIjk) <- N_age
dim(n_ageAjk) <- N_age
dim(n_ageRjk) <- N_age
dim(n_ageEk) <- N_age
dim(n_ageIk) <- N_age
dim(n_ageAk) <- N_age
dim(n_ageRk) <- N_age
dim(m_ij) <-c(N_age, N_age)
dim(g_ij) <-c(N_age, N_age)
dim(sj_ij) <-c(N_age, N_age)
dim(ej_ij) <-c(N_age, N_age)
dim(ij_ij) <-c(N_age, N_age)
dim(aj_ij) <-c(N_age, N_age)
dim(rj_ij) <-c(N_age, N_age)
dim(ek_ij) <-c(N_age, N_age)
dim(ik_ij) <-c(N_age, N_age)
dim(ak_ij) <-c(N_age, N_age)
dim(rk_ij) <-c(N_age, N_age)
dim(ejk_ij) <-c(N_age, N_age)
dim(ijk_ij) <-c(N_age, N_age)
dim(ajk_ij) <-c(N_age, N_age)
dim(rjk_ij) <-c(N_age, N_age)
dim(ekj_ij) <-c(N_age, N_age)
dim(ikj_ij) <-c(N_age, N_age)
dim(akj_ij) <-c(N_age, N_age)
dim(n_bM) <- N_age
dim(n_bG) <- N_age
dim(n_bS) <- N_age
dim(n_MS) <- N_age
n_S_Ej<- N_age 
n_S_Ek<- N_age 
n_Ej_Ij<- N_age 
n_Ij_Aj<- N_age 
n_Aj_Rj<- N_age 
n_Rj_Aj<- N_age
n_Rj_S<- N_age
n_Rj_Ekj<- N_age 
n_Ekj_Ikj<- N_age 
n_Ikj_Akj<- N_age 
n_Akj_Rkj<- N_age 
n_Ejk_Ijk<- N_age 
n_Ijk_Ajk<- N_age 
n_Ajk_Rjk<- N_age 
n_Rj_S<- N_age   
n_Ek_Ik<- N_age 
n_Ik_Ak<- N_age 
n_Ak_Rk<- N_age 
n_Rk_Ak<- N_age 
n_Rk_S<- N_age
n_Rk_Ejk <- N_age 
dim(p_birth)   <- 3
dim(mu)<-N_age
dim(m) <- c(N_age, N_age)
dim(m_holi) <- c(N_age, N_age)
dim(cj_ij) <- c(N_age, N_age)
dim(cj_ij) <- c(N_age, N_age)
dim(lambda_j) <- N_age
dim(lambda_k) <- N_age

