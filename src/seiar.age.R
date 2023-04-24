## Definition of the time-step and output as "time"
dt <- user(1)
steps_per_day <- 1/dt
steps_per_week<- 7/dt
steps_per_year<- 365/dt
initial(time) <- 0
update(time) <- (step + 1) * dt

## Equations for transitions between compartments by age group
update(M[]) <- M[i] + n_bM[i] - n_MS[i] - n_muM[i] + n_ageM[i]
update(G[]) <- G[i] + n_bG[i] - n_muG[i] + n_ageG[i]
update(S[]) <- S[i] + n_bS[i] + n_MS[i] + n_RS[i] - n_SE[i] - n_muS[i] + n_ageS[i] 
update(E[]) <- E[i] + n_SE[i] - n_EI[i] - n_muE[i] + n_ageE[i]
update(I[]) <- I[i] + n_EI[i] - n_IA[i] - n_muI[i] + n_ageI[i]
update(A[]) <- A[i] + n_IA[i] + n_RA[i] - n_AR[i]  - n_muA[i] + n_ageA[i]
update(R[]) <- R[i] + n_AR[i] - n_RS[i] - n_muR[i] - n_RA[i]  + n_ageR[i]


######### Outputs

# Daily infections incidence
update(infections_day) <- sum(n_EI) 

# Weekly reported cases (match sgss)
update(reported_wk) <- if (step %% steps_per_week == 0)
  sum(n_EI)*(1/repfac) else reported_wk + sum(n_EI)*(1/repfac) 

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

update(cases_year[]) <- if (step %% steps_per_year==0)
  (if(i==1)  sum(n_EI[1]) else
    if(i==2)  sum(n_EI[2:4])  else 
     if(i==3)  sum(n_EI[5:8])  else 
      if (i==4)  sum(n_EI[9:13])  else
         sum(n_EI[14]) ) else
          (if(i==1)  cases_year[1] + sum(n_EI[1]) else
            if(i==2)  cases_year[2] + sum(n_EI[2:4]) else
              if(i==3) cases_year[3] + sum(n_EI[5:8])  else 
              if (i==4)  cases_year[4] +sum(n_EI[9:13])  else
                cases_year[5] + sum(n_EI[14]) )
                  


## compute at risk population ( match IDD2 data)
update(person_year[]) <- if (step %% steps_per_year==0)
  (if(i==1)  sum(M[1])+sum(G[1])+sum(S[1])+sum(E[1])+sum(A[1])+sum(R[1]) else
    if(i==2)  sum(M[2:4])+sum(G[2:4])+sum(S[2:4])+sum(E[2:4])+sum(A[2:4])+sum(R[2:4])  else 
      if(i==3)  sum(M[5:8])+sum(G[5:8])+sum(S[5:8])+sum(E[5:8])+sum(A[5:8])+sum(R[5:8])  else 
      if (i==4)  sum(M[9:13])+sum(G[9:13])+sum(S[9:13])+sum(E[9:13])+sum(A[9:13])+sum(R[9:13])  else
        sum(M[14])+sum(G[14])+sum(S[14])+sum(E[14])+sum(A[14])+sum(R[14]) ) else
          (if(i==1)  person_year[1] + sum(M[1])+sum(G[1])+sum(S[1])+sum(E[1])+sum(A[1])+sum(R[1]) else
            if(i==2) person_year[2] + sum(M[2:4])+sum(G[2:4])+sum(S[2:4])+sum(E[2:4])+sum(A[2:4])+sum(R[2:4])   else 
             if(i==3) person_year[3] + sum(M[5:8])+sum(G[5:8])+sum(S[5:8])+sum(E[5:8])+sum(A[5:8])+sum(R[5:8])   else 
              if (i==4)  person_year[4] + sum(M[9:13])+sum(G[9:13])+sum(S[9:13])+sum(E[9:13])+sum(A[9:13])+sum(R[9:13])  else
                person_year[5] + sum(M[14])+sum(G[14])+sum(S[14])+sum(E[14])+sum(A[14])+sum(R[14]) )


## compute prevalence 
update(seroprev[]) <- (R[i]+M[i])/(M[i]+G[i]+S[i]+E[i]+I[i]+A[i]+R[i])




## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_MS[] <- 1 - exp(-(1/delta) * dt)  # M to S
p_SE[] <- 1 - exp(-lambda[i] * dt) # S to E
p_RA[] <- 1 - exp(-lambda[i] * (1-alpha) * dt)
p_EI   <- 1 - exp(-(1/epsilon) * dt) # E to I
p_IA_5    <- 1 - exp(-(1/theta_5) * dt) # I to A
p_IA_5p   <- 1 - exp(-(1/theta_5p) * dt) # I to A
p_AR   <- 1 - exp(-(1/sigma) * dt) # A to R
p_RS   <- 1 - exp(- (1/(tau*365)) * dt) # R to S
N      <- sum(M) + sum(G)+sum(S)+sum(E)+sum(I)+sum(A)+sum(R)
prev   <- (sum(E)+sum(I)+sum(A)+sum(R))/N  
p[1]   <- (1-p_nonsecretor)*prev
p[2]   <- p_nonsecretor
p[3]   <- (1-p_nonsecretor)*(1-prev) 

## Force of infection
m[, ] <- user() # age-structured contact matrix
m_holi[,]<-user()
#switcher<-school[as.integer(step)] 

switcher <- if (as.integer(step) >= n_school_steps)
  school_step[n_school_steps]*
  (1- (as.integer(step)-n_school_steps)) else 
    school_step[as.integer(step) + 1] 


c_ij[, ] <- (I[j] + A[j] * rho + E[j] * rho) *
  (switcher*m[i, j]  + (1-switcher)*m_holi[i, j])
 # if (school[as.integer(step)]==1) m[i, j] else m_holi[i, j]

beta_t <- beta *(1 + w1*cos((2*pi*time)/364 + w2*pi))
lambda[] <- beta_t  * sum(c_ij[i , ])

# Aging transitions
m_ij[,] <- aging_mat[i,j]*M[j] * dt
g_ij[,] <- aging_mat[i,j]*G[j] * dt
s_ij[,] <- aging_mat[i,j]*S[j] * dt
e_ij[,] <- aging_mat[i,j]*E[j] * dt
i_ij[,] <- aging_mat[i,j]*I[j] * dt
a_ij[,] <- aging_mat[i,j]*A[j] * dt
r_ij[,] <- aging_mat[i,j]*R[j] * dt

n_ageM[]<- round(sum(m_ij[,i]))
n_ageG[]<- round(sum(g_ij[,i]))
n_ageS[]<- round(sum(s_ij[,i]))
n_ageE[]<- round(sum(e_ij[,i]))
n_ageI[]<- round(sum(i_ij[,i]))
n_ageA[]<- round(sum(a_ij[,i]))
n_ageR[]<- round(sum(r_ij[,i]))


## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i], p_mu[i])

n_muM[]<- rbinom(M[i], p_mu[i])
n_MS[] <- rbinom(M[i]-n_muM[i],p_MS[i])

n_muS[]<- rbinom(S[i], p_mu[i])
n_SE[] <- rbinom(S[i]-n_muS[i], p_SE[i])

n_muE[]<- rbinom(E[i], p_mu[i])
n_EI[] <- rbinom(E[i]-n_muE[i], p_EI)

n_muI[]<- rbinom(I[i], p_mu[i])
n_IA[] <- rbinom(I[i]-n_muI[i], if (i<=5)p_IA_5 else p_IA_5p )

n_muA[]<- rbinom(A[i], p_mu[i])
n_AR[] <- rbinom(A[i]-n_muA[i], p_AR)

n_muR[]<- rbinom(R[i], p_mu[i])
n_RA[] <- rbinom(R[i]-n_muR[i], p_RA[i])
n_RS[] <- rbinom(R[i]-n_muR[i]-n_RA[i], p_RS)


n_allDeath<- 
  sum(n_muM) + 
  sum(n_muG)+
  sum(n_muS)+
  sum(n_muE)+
  sum(n_muI)+
  sum(n_muA)+
  sum(n_muR)

# Births to keep stable population equal to deaths
n_bM[] <- if (i==1) n_allDeath*p[1] else 0 
n_bG[] <- if (i==1) n_allDeath*p[2] else 0 
n_bS[] <- if (i==1) n_allDeath*p[3] else 0 


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
initial(E[]) <- init[4,i]
initial(I[]) <- init[5,i]
initial(A[]) <- init[6,i]
initial(R[]) <- init[7,i]
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
dim(init) <-  c(7,N_age)
dim(aging_mat)<-c(N_age, N_age)
dim(M) <- N_age
dim(G) <- N_age
dim(S) <- N_age
dim(E) <- N_age
dim(I) <- N_age
dim(A) <- N_age
dim(R) <- N_age
#dim(cumu_inc) <- N_age
dim(seroprev) <- N_age
dim(cases_year)<- 5
dim(person_year)<- 5
#dim(n_risk) <- N_age
dim(n_muM) <- N_age
dim(n_muG) <- N_age
dim(n_muS) <- N_age
dim(n_muE) <- N_age
dim(n_muI) <- N_age
dim(n_muA) <- N_age
dim(n_muR) <- N_age
dim(n_ageM) <- N_age
dim(n_ageG) <- N_age
dim(n_ageS) <- N_age
dim(n_ageE) <- N_age
dim(n_ageI) <- N_age
dim(n_ageA) <- N_age
dim(n_ageR) <- N_age
dim(m_ij) <-c(N_age, N_age)
dim(g_ij) <-c(N_age, N_age)
dim(s_ij) <-c(N_age, N_age)
dim(e_ij) <-c(N_age, N_age)
dim(i_ij) <-c(N_age, N_age)
dim(a_ij) <-c(N_age, N_age)
dim(r_ij) <-c(N_age, N_age)
dim(n_bM) <- N_age
dim(n_bG) <- N_age
dim(n_bS) <- N_age
dim(n_MS) <- N_age
dim(n_SE) <- N_age
dim(n_EI) <- N_age
dim(n_IA) <- N_age
dim(n_AR) <- N_age
dim(n_RS) <- N_age
dim(n_RA) <- N_age
dim(p_mu) <- N_age
dim(p_MS) <- N_age
dim(p_SE) <- N_age
dim(p_RA) <- N_age
dim(p)   <- 3
dim(mu)<-N_age
dim(m) <- c(N_age, N_age)
dim(m_holi) <- c(N_age, N_age)
dim(c_ij) <- c(N_age, N_age)
dim(lambda) <- N_age


