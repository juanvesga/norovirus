## Definition of the time-step and output as "time"
dt <- user(1)
steps_per_day <- 1/dt
steps_per_week<- 7/dt
initial(time) <- 0
update(time) <- (step + 1) * dt

## Core equations for transitions between compartments:
update(M_tot) <- M_tot + sum(n_bM) - sum(n_MS) - sum(n_muM)
update(G_tot) <- G_tot + sum(n_bG) - sum(n_muG)
update(S_tot) <- S_tot + sum(n_bS) + sum(n_MS) + sum(n_RS) - sum(n_SE) - sum(n_muS) 
update(E_tot) <- E_tot + sum(n_SE) - sum(n_EI) - sum(n_muE)
update(I_tot) <- I_tot + sum(n_EI) - sum(n_IA)  - sum(n_muI)
update(A_tot) <- A_tot + sum(n_IA) + sum(n_RA) - sum(n_AR) - sum(n_muA)
update(R_tot) <- R_tot + sum(n_AR) - sum(n_RS) - sum(n_RA) -sum(n_muR)

# Cumulative infections
update(infections_tot) <- infections_tot + sum(n_EI)

# Daily infections incidence
update(infections_day) <- sum(n_EI) 

# Weekly reported cases (match sgss)
update(reported_wk) <- if (step %% steps_per_week == 0)
  sum(n_EI)*(1/repfac) else  (1/repfac)*(reported_wk + sum(n_EI)) 


## Equations for transitions between compartments by age group
update(M[]) <- M[i] + n_bM[i] - n_MS[i] - n_muM[i] + n_ageM[i]
update(G[]) <- G[i] + n_bG[i] - n_muG[i] + n_ageG[i]
update(S[]) <- S[i] + n_bS[i] + n_MS[i] + n_RS[i] - n_SE[i] - n_muS[i] + n_ageS[i] 
update(E[]) <- E[i] + n_SE[i] - n_EI[i] - n_muE[i] + n_ageE[i]
update(I[]) <- I[i] + n_EI[i] - n_IA[i] - n_muI[i] + n_ageI[i]
update(A[]) <- A[i] + n_IA[i] + n_RA[i] - n_AR[i]  - n_muA[i] + n_ageA[i]
update(R[]) <- R[i] + n_AR[i] - n_RS[i] - n_muR[i] - n_RA[i]  + n_ageR[i]

## compute cumulative incidence ( match IDD2 data)
update(cumu_inc[]) <- if (step <= (2283/dt)) 0 else 
  if(i==1)  sum(cumu_inc[1:5]) + sum(n_EI[1:5]) else
    if(i==2) sum(cumu_inc[6:8]) + sum(n_EI[6:8]) else 
      if (i==3) sum(cumu_inc[9:13]) + sum(n_EI[9:13]) else
        if (i==4) sum(cumu_inc[14:15]) + sum(n_EI[14:15]) else 0

## compute at risk population ( match IDD2 data)
update(n_risk[]) <- if (step <= (2283/dt)) 0 else 
  if(i==1)  sum(n_risk[1:5]) + sum(M[1:5]) + sum(G[1:5])+ sum(S[1:5])+ sum(R[1:5]) else
    if(i==2)  sum(n_risk[6:8]) + sum(M[6:8]) + sum(G[6:8])+ sum(S[6:8])+ sum(R[6:8]) else
      if(i==3)  sum(n_risk[9:13]) + sum(M[9:13]) + sum(G[9:13])+ sum(S[9:13])+ sum(R[9:13]) else
        if(i==4)  sum(n_risk[14:15]) + sum(M[14:15]) + sum(G[14:15])+ sum(S[14:15])+ sum(R[14:15]) else 0

## compute prevalence 
update(seroprev[]) <- (M[i]+R[i])/(M[i]+G[i]+S[i]+E[i]+R[i])


## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_MS[] <- 1 - exp(-delta * dt)  # M to S
p_SE[] <- 1 - exp(-lambda[i] * dt) # S to E
p_RA[] <- 1 - exp(-lambda[i] * alpha * dt)
p_EI   <- 1 - exp(-epsilon * dt) # E to I
p_IA   <- 1 - exp(-theta * dt) # I to A
p_AR   <- 1 - exp(-sigma * dt) # A to R
p_RS   <- 1 - exp(-tau * dt) # R to S
N      <- sum(M) + sum(G)+sum(S)+sum(E)+sum(I)+sum(A)+sum(R)
prev   <- (I_tot+A_tot+R_tot)/N  
p[1]   <- (1-p_nonsecretor)*prev
p[2]   <- p_nonsecretor
p[3]   <- (1-p_nonsecretor)*(1-prev) 

## Force of infection
m[, ] <- user() # age-structured contact matrix
c_ij[, ] <- m[i, j] * (I[i] + A[i] * rho + E[i] * rho)
beta_t <- beta *(1 + w1*cos((2*pi*time)/364 + w2*pi))
lambda[] <- beta_t  * sum(c_ij[ ,i])

## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i], p_mu[i])

n_MS[] <- rbinom(M[i], p_MS[i])
n_muM[]<- rbinom(M[i]-n_MS[i], p_mu[i])

n_SE[] <- rbinom(S[i], sum(p_SE[i]))
n_muS[]<- rbinom(S[i]-n_SE[i], p_mu[i])

n_EI[] <- rbinom(E[i], p_EI)
n_muE[]<- rbinom(E[i]-n_EI[i], p_mu[i])

n_IA[] <- rbinom(I[i], p_IA)
n_muI[]<- rbinom(I[i]-n_IA[i], p_mu[i])

n_AR[] <- rbinom(A[i], p_AR)
n_muA[]<- rbinom(A[i]-n_AR[i], p_mu[i])

n_RA[] <- rbinom(R[i], sum(p_RA[i]))
n_RS[] <- rbinom(R[i]- n_RA[i], p_RS)
n_muR[]<- rbinom(R[i]-n_RS[i]- n_RA[i], p_mu[i])



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


## User defined parameters - default in parentheses:
init[,] <- user()

beta <- user(0.046)   # transm coefficient
repfac<-user(287)    # reported to community factor
delta <- user(0.04)  # maternal Ab decay
epsilon <- user(1)   # incubation
theta <- user(0.5)   # duration symptoms
sigma <- user(1/15) # duration asymp shedding
tau   <- user(1/(5.1*365))     # duration immunity
rho   <- user(0.05) # rel infect asymptomatic 
p_nonsecretor<-user(0.2) # Fraction immune genetically
w1 <-user(0.15) # sesonality
w2 <-user(1/12) # sesonality
pi <-user(3.141593)
alpha<-user(1)# rel susc in R 
mu[]  <- user()      # mortality rates 
aging_mat[,]<-user() # aging transitions matrix
#
# dimensions of arrays
N_age <- user()
dim(init) <-  c(7,N_age)
dim(aging_mat)<-c(N_age, N_age)
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
dim(c_ij) <- c(N_age, N_age)
dim(lambda) <- N_age


