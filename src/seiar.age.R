## Definition of the time-step and output as "time"
dt <- user()
initial(time) <- 0
update(time) <- (step + 1) * dt

## Core equations for transitions between compartments:
update(M_tot) <- M_tot + sum(n_bM) - sum(n_MS) - sum(n_muM)
update(G_tot) <- G_tot + sum(n_bG) - sum(n_muG)
update(S_tot) <- S_tot + sum(n_bS) + sum(n_MS) + sum(n_RS) - sum(n_SE) - sum(n_muS) 
update(E_tot) <- E_tot + sum(n_SE) - sum(n_EI) - sum(n_muE)
update(I_tot) <- I_tot + sum(n_EI) - sum(n_IA)  - sum(n_muI)
update(A_tot) <- A_tot + sum(n_IA) - sum(n_AR) - sum(n_muA)
update(R_tot) <- R_tot + sum(n_AR) - sum(n_RS) - sum(n_muR)

## Equations for transitions between compartments by age group
update(M[]) <- M[i] + n_bM[i] - n_MS[i] - n_muM[i] + n_ageM[i]
update(G[]) <- G[i] + n_bG[i] - n_muG[i] + n_ageG[i]
update(S[]) <- S[i] + n_bS[i] + n_MS[i] + n_RS[i] - n_SE[i] - n_muS[i] + n_ageS[i] 
update(E[]) <- E[i] + n_SE[i] - n_EI[i] - n_muE[i] + n_ageE[i]
update(I[]) <- I[i] + n_EI[i] - n_IA[i] - n_muI[i] + n_ageI[i]
update(A[]) <- A[i] + n_IA[i] - n_AR[i] - n_muA[i] + n_ageA[i]
update(R[]) <- R[i] + n_AR[i] - n_RS[i] - n_muR[i] + n_ageR[i]

## To compute cumulative incidence
update(cumu_inc[]) <- cumu_inc[i] + n_EI[i]

## Individual probabilities of transition:
p_mu[] <- 1 - exp(-mu[i] * dt) # mortality
p_MS[] <- 1 - exp(-delta * dt)  # M to S
p_SE[] <- 1 - exp(-lambda[i] * dt) # S to E
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
lambda[] <- beta * age_beta[i] * sum(c_ij[,i])

## Draws from binomial distributions for numbers changing between
## compartments:

n_muG[] <- rbinom(G[i], p_mu[i])

n_MS[] <- (rbinom(M[i], p_MS[i]))
n_muM[]<- (rbinom(M[i]-n_MS[i], p_mu[i]))

n_SE[] <- (rbinom(S[i], p_SE[i]))
n_muS[]<- (rbinom(S[i]-n_SE[i], p_mu[i]))

n_EI[] <- (rbinom(E[i], p_EI))
n_muE[]<- (rbinom(E[i]-n_EI[i], p_mu[i]))

n_IA[] <- (rbinom(I[i], p_IA))
n_muI[]<- (rbinom(I[i]-n_IA[i], p_mu[i]))

n_AR[] <- (rbinom(A[i], p_AR))
n_muA[]<- (rbinom(A[i]-n_AR[i], p_mu[i]))

n_RS[] <- (rbinom(R[i], p_RS))
n_muR[]<- (rbinom(R[i]-n_RS[i], p_mu[i]))



n_allDeath<- 
  sum(n_muM) + 
  sum(n_muG)+
  sum(n_muS)+
  sum(n_muE)+
  sum(n_muI)+
  sum(n_muA)+
  sum(n_muR)

# Births to keep stable population equal to deaths
n_bM[] <- n_allDeath*p[1]*age_select[i]
n_bG[] <- n_allDeath*p[2]*age_select[i]
n_bS[] <- n_allDeath*p[3]*age_select[i]


# Aging transitions
m_ij[,] <- aging_mat[i,j]*M[j] * dt
g_ij[,] <- aging_mat[i,j]*G[j] * dt
s_ij[,] <- aging_mat[i,j]*S[j] * dt
e_ij[,] <- aging_mat[i,j]*E[j] * dt
i_ij[,] <- aging_mat[i,j]*I[j] * dt
a_ij[,] <- aging_mat[i,j]*A[j] * dt
r_ij[,] <- aging_mat[i,j]*R[j] * dt

n_ageM[]<- round(sum(m_ij[i,]))
n_ageG[]<- round(sum(g_ij[i,]))
n_ageS[]<- round(sum(s_ij[i,]))
n_ageE[]<- round(sum(e_ij[i,]))
n_ageI[]<- round(sum(a_ij[i,]))
n_ageA[]<- round(sum(i_ij[i,]))
n_ageR[]<- round(sum(r_ij[i,]))


## Initial states:
initial(M_tot) <- sum(M_ini)
initial(G_tot) <- sum(G_ini)
initial(S_tot) <- sum(S_ini)
initial(E_tot) <- sum(E_ini)
initial(I_tot) <- sum(I_ini)
initial(A_tot) <- sum(A_ini)
initial(R_tot) <- sum(R_ini)

initial(M[]) <- M_ini[i]
initial(G[]) <- G_ini[i]
initial(S[]) <- S_ini[i]
initial(E[]) <- E_ini[i]
initial(I[]) <- I_ini[i]
initial(A[]) <- A_ini[i]
initial(R[]) <- R_ini[i]
initial(cumu_inc[]) <- 0

## User defined parameters - default in parentheses:
M_ini[] <- user()
G_ini[] <- user()
S_ini[] <- user()
E_ini[] <- user()
I_ini[] <- user()
A_ini[] <- user()
R_ini[] <- user()
age_select[]<-user()

beta <- user(0.19)   # transm coefficient
delta <- user(0.04)  # maternal Ab decay
epsilon <- user(1)   # incubation
theta <- user(0.5)   # duration symptoms
sigma <- user(0.066) # duration asymp shedding
tau   <- user(5.37*10^(-4))     # duration immunity
rho   <- user(0.05) # rel infect asymptomatic 
p_nonsecretor<-user(0.2) # Fraction immune genetically
mu[]  <- user()      # mortality rates 
age_beta[]<-user()   # age-specific infectiousness co factor
aging_mat[,]<-user() # aging transitions matrix
#
# dimensions of arrays
N_age <- user()
dim(M_ini) <- N_age
dim(G_ini) <- N_age
dim(S_ini) <- N_age
dim(E_ini) <- N_age
dim(I_ini) <- N_age
dim(A_ini) <- N_age
dim(R_ini) <- N_age
dim(age_select)<-N_age
dim(age_beta)<-N_age
dim(aging_mat)<-c(N_age, N_age)
dim(M) <- N_age
dim(G) <- N_age
dim(S) <- N_age
dim(E) <- N_age
dim(I) <- N_age
dim(A) <- N_age
dim(R) <- N_age
dim(cumu_inc) <- N_age
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
dim(p_mu) <- N_age
dim(p_MS) <- N_age
dim(p_SE) <- N_age
dim(p)   <- 3
dim(mu)<-N_age
dim(m) <- c(N_age, N_age)
dim(c_ij) <- c(N_age, N_age)
dim(lambda) <- N_age


