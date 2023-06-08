## Definition of the time-step and output as "time"
dt <- user(1)
steps_per_day <- 1/dt
steps_per_week<- 7/dt
initial(time) <- 0
update(time) <- (step + 1) * dt

## Core equations for transitions between compartments:
update(M_tot) <- M_tot + sum(n_bM) - sum(n_MS) - sum(n_MA) - sum(n_muM)
update(G_tot) <- G_tot + sum(n_bG) - sum(n_muG)
update(S_tot) <- S_tot + sum(n_bS) + sum(n_MS) + sum(n_RS) - sum(n_SE) - sum(n_muS) 
update(E_tot) <- E_tot + sum(n_SE) - sum(n_EI) - sum(n_muE)
update(I_tot) <- I_tot + sum(n_EI) - sum(n_IA)  - sum(n_muI)
update(A_tot) <- A_tot + sum(n_IA) + sum(n_RA) - sum(n_AR) - sum(n_muA)
update(R_tot) <- R_tot + sum(n_AR) - sum(n_RS) - sum(n_RA) -sum(n_muR)


## Equations for transitions between compartments by age group

update(M[]) <- if ( ((step %% 365) == 0) && i > 1 ) 
  M[i-1] - n_MS[i-1] - n_MA[i-1] - n_muM[i-1] else 
    M[i] + n_bM[i] - n_MS[i]- n_MA[i] - n_muM[i]

update(G[]) <- if ( ((step %% 365) == 0) && i > 1 )
  G[i-1] - n_muG[i-1]  else
    G[i] + n_bG[i] - n_muG[i] 

update(S[]) <- if ( ((step %% 365) == 0) && i > 1 )
  S[i-1] + n_MS[i-1] + n_RS[i-1] - n_SE[i-1] - n_muS[i-1] else
    S[i] + n_bS[i] + n_MS[i] + n_RS[i] - n_SE[i] - n_muS[i]  

update(E[]) <- if ( ((step %% 365) == 0) && i > 1 )
  E[i-1] + n_SE[i-1] - n_EI[i-1] - n_muE[i-1]  else 
    E[i] + n_SE[i] - n_EI[i] - n_muE[i] 

update(I[]) <- if ( ((step %% 365) == 0) && i > 1 )
  I[i] + n_EI[i] - n_IA[i] - n_muI[i] else 
    I[i-1] + n_EI[i-1] - n_IA[i-1] - n_muI[i-1]  
    
update(A[]) <- if ( ((step %% 365) == 0) && i > 1 )
  A[i-1] + n_IA[i-1] + n_RA[i-1] + n_MA[i-1] - n_AR[i-1]  - n_muA[i-1] else
    A[i] + n_IA[i] + n_RA[i] + n_MA[i]  - n_AR[i]  - n_muA[i] 

update(R[]) <- if ( ((step %% 365) == 0) && i > 1 )
  R[i-1] + n_AR[i-1] - n_RS[i-1] - n_muR[i-1] - n_RA[i-1] else
    R[i] + n_AR[i] - n_RS[i] - n_muR[i] - n_RA[i]   


######### Outputs

# Cumulative infections
update(infections_tot) <- infections_tot + sum(n_EI)

# Daily infections incidence
update(infections_day) <- sum(n_EI) 

# Weekly reported cases (match sgss)
update(reported_wk) <- if (step %% steps_per_week == 0)
  sum(n_EI)*(1/repfac) else  (1/repfac)*(reported_wk + sum(n_EI)) 


# Weekly reported cases in under 4 (match sgss)
update(reported_wk0_4) <- if (step %% steps_per_week == 0)
  sum(n_EI[1:4])*(1/repfac) else  (1/repfac)*(reported_wk0_4 + sum(n_EI[1:4])) 

## compute cumulative incidence ( match IDD2 data)
update(cumu_inc[]) <- if (step <= (2283/dt)) 0 else 
  if(i==1)  sum(cumu_inc[1:5]) + sum(n_EI[1:5]) else
    if(i==2) sum(cumu_inc[6:15]) + sum(n_EI[6:15]) else 
      if (i==3) sum(cumu_inc[16:65]) + sum(n_EI[16:65])  else
        if (i==4) sum(cumu_inc[66:N_age]) + sum(n_EI[66:N_age]) else 0

## compute at risk population ( match IDD2 data)
update(n_risk[]) <- if (step <= (2283/dt)) 0 else 
  if(i==1)  sum(n_risk[1:5]) + sum(M[1:5]) + sum(G[1:5])+ sum(S[1:5])+ sum(R[1:5]) else
    if(i==2)  sum(n_risk[6:15]) + sum(M[6:15]) + sum(G[6:15])+ sum(S[6:15])+ sum(R[6:15]) else
      if(i==3)  sum(n_risk[16:65]) + sum(M[16:65]) + sum(G[16:65])+ sum(S[16:65])+ sum(R[16:65]) else
        if(i==4)  sum(n_risk[66:N_age]) + sum(M[66:N_age]) + sum(G[66:N_age])+ sum(S[66:N_age])+ sum(R[66:N_age]) else 0

## compute prevalence 
update(seroprev[]) <- (M[i]+R[i]+A[i])/(M[i]+G[i]+S[i]+E[i]+A[i]+R[i])


## Aux
update(aux)<-beta_t


## Prevalence of Asymptomatic shedding (IID1)
# update(asymp_pcr[]) <- 
#   if(i==1)    A[i]/(M[i]+G[i]+S[i]+E[i]+A[i]+R[i]) else
#     if(i==2)  sum(A[2:5])/(sum(M[2:5])+sum(G[2:5])+sum(S[2:5])+sum(E[2:5])+sum(A[2:5])+sum(R[2:5])) else
#       if(i==3)  sum(A[6:10])/(sum(M[6:10])+sum(G[6:10])+sum(S[6:10])+sum(E[6:10])+sum(A[6:10])+sum(R[6:10])) else
#         if(i==4)  sum(A[11:20])/(sum(M[11:20])+sum(G[11:20])+sum(S[11:20])+sum(E[11:20])+sum(A[11:20])+sum(R[11:20])) else
#           if(i==5)  sum(A[21:30])/(sum(M[21:30])+sum(G[21:30])+sum(S[21:30])+sum(E[21:30])+sum(A[21:30])+sum(R[21:30])) else
#             if(i==6)  sum(A[31:40])/(sum(M[31:40])+sum(G[31:40])+sum(S[31:40])+sum(E[31:40])+sum(A[31:40])+sum(R[31:40])) else
#               if(i==7)  sum(A[41:50])/(sum(M[41:50])+sum(G[41:50])+sum(S[41:50])+sum(E[41:50])+sum(A[41:50])+sum(R[41:50])) else
#                 if(i==8)  sum(A[51:60])/(sum(M[51:60])+sum(G[51:60])+sum(S[51:60])+sum(E[51:60])+sum(A[51:60])+sum(R[51:60])) else
#                   if(i==9)  sum(A[61:70])/(sum(M[61:70])+sum(G[61:70])+sum(S[61:70])+sum(E[61:70])+sum(A[61:70])+sum(R[61:70])) else
#                     if(i==10)  sum(A[71:N_age])/(sum(M[71:N_age])+sum(G[71:N_age])+sum(S[71:N_age])+sum(E[71:N_age])+sum(A[71:N_age])+sum(R[71:N_age])) else 0
                      



## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_MS[] <- 1 - exp(-(1/delta) * dt)  # M to S
p_SE[] <- 1 - exp(-lambda[i] * dt) # S to E
p_RA[] <- 1 - exp(-lambda[i] * (1-alpha) * dt)
p_EI   <- 1 - exp(-epsilon * dt) # E to I
p_IA   <- 1 - exp(-(1/theta) * dt) # I to A
p_AR   <- 1 - exp(-(1/sigma) * dt) # A to R
p_RS   <- 1 - exp(- (1/(tau*365)) * dt) # R to S
N      <- sum(M) + sum(G)+sum(S)+sum(E)+sum(I)+sum(A)+sum(R)
prev   <- (E_tot+I_tot+A_tot+R_tot)/N  
p[1]   <- (1-p_nonsecretor)*prev
p[2]   <- p_nonsecretor
p[3]   <- (1-p_nonsecretor)*(1-prev) 

## Force of infection
m[, ] <- user() # age-structured contact matrix
m_holi[,]<-user()

c_ij[, ] <- (I[j] + A[j] * rho + E[j] * rho) * 
  if (school[as.integer(step)]==1) m[i, j] else m_holi[i, j]

beta_t <- beta *(1 + w1*cos((2*pi*time)/364 + w2*pi))
lambda[] <- beta_t  * sum(c_ij[i , ])



## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i], p_mu[i])

n_muM[]<- rbinom(M[i], p_mu[i])
n_MA[] <- rbinom(M[i]-n_muM[i], p_RA[i])
n_MS[] <- rbinom(M[i]-n_muM[i]-n_MA[i],p_MS[i])

n_muS[]<- rbinom(S[i], p_mu[i])
n_SE[] <- rbinom(S[i]-n_muS[i], p_SE[i])

n_muE[]<- rbinom(E[i], p_mu[i])
n_EI[] <- rbinom(E[i]-n_muE[i], p_EI)

n_muI[]<- rbinom(I[i], p_mu[i])
n_IA[] <- rbinom(I[i]-n_muI[i], p_IA)

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
initial(M_tot) <- sum(init[1,])
initial(G_tot) <- sum(init[2,])
initial(S_tot) <- sum(init[3,])
initial(E_tot) <- sum(init[4,])
initial(I_tot) <- sum(init[5,])
initial(A_tot) <- sum(init[6,])
initial(R_tot) <- sum(init[7,])
initial(infections_tot)<-0
initial(infections_day)<-0
initial(reported_wk)<-0
initial(reported_wk0_4)<-0

initial(M[]) <- init[1,i]
initial(G[]) <- init[2,i]
initial(S[]) <- init[3,i]
initial(E[]) <- init[4,i]
initial(I[]) <- init[5,i]
initial(A[]) <- init[6,i]
initial(R[]) <- init[7,i]
initial(cumu_inc[]) <- 0
initial(seroprev[]) <-0
initial(n_risk[]) <-init[1,i]+init[2,i]+init[3,i]+init[7,i]
initial(aux)<-0
#initial(asymp_pcr[])<-0

## User defined parameters - default in parentheses:
init[,] <- user()

beta <- user(0.046)   # transm coefficient
repfac<-user(287)    # reported to community factor
delta <- user(0.04)  # maternal Ab decay
epsilon <- user(1)   # incubation
theta <- user(2)   # duration symptoms
sigma <- user(15) # duration asymp shedding
tau   <- user(5.1)     # duration immunity
rho   <- user(0.05) # rel infect asymptomatic 
p_nonsecretor<-user(0.2) # Fraction immune genetically
w1 <-user(0.15) # sesonality
w2 <-user(1/12) # sesonality
pi <-user(3.141593)
alpha<-user(0)# rel susc in R 
mu[]  <- user()      # mortality rates 
school[]<-user()
#
# dimensions of arrays
N_age <- user()
dim(school)<-user()
dim(init) <-  c(7,N_age)
dim(M) <- N_age
dim(G) <- N_age
dim(S) <- N_age
dim(E) <- N_age
dim(I) <- N_age
dim(A) <- N_age
dim(R) <- N_age
dim(cumu_inc) <- N_age
dim(seroprev) <- N_age
dim(n_risk) <- N_age
dim(n_muM) <- N_age
dim(n_muG) <- N_age
dim(n_muS) <- N_age
dim(n_muE) <- N_age
dim(n_muI) <- N_age
dim(n_muA) <- N_age
dim(n_muR) <- N_age
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
dim(n_MA) <- N_age
dim(p_SE) <- N_age
dim(p_RA) <- N_age
dim(p)   <- 3
dim(mu)<-N_age
dim(m) <- c(N_age, N_age)
dim(m_holi) <- c(N_age, N_age)
dim(c_ij)  <- c(N_age, N_age)
dim(lambda) <- N_age


