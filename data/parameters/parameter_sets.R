library(msm)
library(popbio)

I <- 35 #number of river segments
A <- 4 #number of ages
P <- 20 #number of parameter sets

#moment matching functions:
#Beta distribution
alpha_in_beta_moment <- function(means, sd){
  alpha <- (means^2 - means^3 -(means*sd^2))/sd^2
  return(alpha)
}

beta_in_beta_moment <- function(alpha, means){ #beta = (alpha)*(1-mean)/(mean)
  beta <- (alpha*(1-means))/means
  return(beta)
}

#Gamma distribution
alpha_in_gamma_moment <- function(means, sd){
  alpha <- means^2/sd^2
  return(alpha)
}

beta_in_gamma_moment <- function(means, sd){ 
  beta <- means/sd^2
  return(beta)
}

set.seed(0322)

#### Parameter 1 - capture efficiency####
##### *operating model #####
#operating model values: uniform(0.1, 0.3)
cap_eff <- runif(P, 0.1, 0.3) #from JDO

##### *estimation model ####
# Need to transform uniform distribution to a beta distribution
# We do this by using optim to find alpha and beta shape parameters that 
# minimize the likelihood between the 3 moments (lower range, mean, upper range)

lik_capeff <-function(parms){
  alpha<-parms[1]
  beta<-parms[2]
  
  mean <- alpha/(alpha + beta)
  lower <- qbeta(0.05,alpha,beta,lower.tail = TRUE)
  upper <- qbeta(0.05,alpha,beta,lower.tail = FALSE)
  
  sum <- (mean - 0.3)^2+(lower-0.1)^2+(upper-0.5)^2
}

out_capeff <- optim(c(0.8,1), lik_capeff, method = "L-BFGS-B",lower = 0,upper = Inf)

#hist(rbeta(100000,out_capeff$par[1],out_capeff$par[2]))

#year 1 estimation priors are: 
cap.eff.est.alpha <- out_capeff$par[1]
cap.eff.est.beta <- out_capeff$par[2]

#### Parameter 2 - monthly age survival ####
##### *operating model #####
phi_sd <- 0.1 #low uncertainty
phi_means <- c(0.08, 0.7, 0.45, 0.02)^(1/12) #From Messager & Olden 2018

survival_age <- array(NA, dim = c(A,P))

survival_age[1,] <- rtnorm(P, phi_means[1], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age[2,] <- rtnorm(P, phi_means[2], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age[3,] <- rtnorm(P, phi_means[3], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age[4,] <- rtnorm(P, phi_means[4], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal

##### *estimation model #### 
#using the survival_age data, we need to generate monthly growth rates 
S <- 10000
L_notjune <- array(0, dim = c(4,4,S))
notjune_growth <- rep(NA, S)
survival_age_est <- array(NA, dim = c(4,S))

survival_age_est[1,] <- rtnorm(S, phi_means[1], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age_est[2,] <- rtnorm(S, phi_means[2], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age_est[3,] <- rtnorm(S, phi_means[3], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal
survival_age_est[4,] <- rtnorm(S, phi_means[4], phi_sd, lower = 0.0001, upper = 0.9999) #truncated normal

start <- Sys.time()
for(s in 1:S){
  diag(L_notjune[,,s]) <- survival_age_est[1:4,s]
  notjune_growth[s] <- lambda(L_notjune[,,s])
}

mean.notjunegrowth <- mean(notjune_growth)
sd.notjunegrowth <- sd(notjune_growth)

notjunegrowth.est.alpha <- alpha_in_beta_moment(mean.notjunegrowth, sd.notjunegrowth)
notjunegrowth.est.beta <- beta_in_beta_moment(notjunegrowth.est.alpha, mean.notjunegrowth)

mean(rbeta(10000,notjunegrowth.est.alpha, notjunegrowth.est.beta))
  
#### Parameter 3 - mature females: ####
##### *operating model #####
mature_sd <- 0.1 #low uncertainty
mature_means <- c(0, 0.1, 0.8, 0.9) #From Messager & Olden 2018
mature_age <- array(NA, dim = c(A,P))

mature_age[1,] <- 0 #truncated normal
mature_age[2,] <- rtnorm(P, mature_means[2], mature_sd, lower = 0, upper = 1) #truncated normal
mature_age[3,] <- rtnorm(P, mature_means[3], mature_sd, lower = 0, upper = 1) #truncated normal
mature_age[4,] <- rtnorm(P, mature_means[4], mature_sd, lower = 0, upper = 1) #truncated normal

#### Parameter 4 - fecundity: ####
##### *operating model #####
fecund_sd <- c(0, 10, 20, 40) #Messager & Olden 2018
fecund_means <- c(0, 80, 120, 150) #Messager & Olden 2018
fecund_age <- array(NA, dim = c(A,P))

fecund_age[1,] <- 0 #truncated normal
fecund_age[2,] <- rtnorm(P, fecund_means[2], fecund_sd[2], lower = 0) #truncated normal
fecund_age[3,] <- rtnorm(P, fecund_means[3], fecund_sd[3], lower = 0) #truncated normal
fecund_age[4,] <- rtnorm(P, fecund_means[4], fecund_sd[4], lower = 0) #truncated normal

##### *estimation model #### 
#using mature rates and fecundity rates, find monthly growth rate for june months
L_june <- array(0, dim = c(A,A,S))
june_growth <- rep(NA, S)
fecund_est <- array(NA, dim = c(A,S))

fecund_est[2,] <- rtnorm(S, fecund_means[2], fecund_sd[2], lower = 0)*rtnorm(S, mature_means[2], mature_sd, lower = 0, upper = 1) #truncated normal
fecund_est[3,] <- rtnorm(S, fecund_means[3], fecund_sd[3], lower = 0)*rtnorm(S, mature_means[3], mature_sd, lower = 0, upper = 1) #truncated normal
fecund_est[4,] <- rtnorm(S, fecund_means[4], fecund_sd[4], lower = 0)*rtnorm(S, mature_means[4], mature_sd, lower = 0, upper = 1) #truncated normal

lambda.age<- c(survival_age_est[1,]*fecund_est[2,],
               survival_age_est[2,]*fecund_est[3,],
               survival_age_est[3,]*fecund_est[4,],
               survival_age_est[4,]*fecund_est[4,])

L_june[1,,] <- lambda.age #filling in fecundity for Leslie matrix June
L_june[2,1,] <- survival_age_est[1,]
L_june[3,2,] <- survival_age_est[2,]
L_june[4,3,] <- survival_age_est[3,]
L_june[4,4,] <- survival_age_est[4,]

for(s in 1:S){
  june_growth[s] <- lambda(L_june[,,s])
}

mean.junegrowth <- mean(june_growth)
sd.junegrowth <- sd(june_growth)

junegrowth.est.alpha <- alpha_in_gamma_moment(mean.junegrowth, sd.junegrowth)
junegrowth.est.beta <- beta_in_gamma_moment(mean.junegrowth, sd.junegrowth)

hist(rgamma(10000,junegrowth.est.alpha, junegrowth.est.beta))

###############################################################################
#### Parameter 5 - movement ####
##### *operating model #####
#Movement probabilities: 
kernel <- c(0.85, 0.15)
kernel1 <- kernel
#scaling factor:to incorporate uncertainty, we will modify the stationary (movement) probability
#we will change the probability of staying (first probability in the kernel vector)
#for a scaling rate r we will increase the stationary probability by r%, r/2% 
#and decrease by r%, r/2%. 
r <- 0.05

kernel2 <- c(kernel1[1] + r, 1-(kernel1[1] + r))
kernel3 <- c(kernel1[1] + 2*r, 1-(kernel1[1] + 2*r))
kernel4 <- c(kernel1[1] - r, 1-(kernel1[1] -r))
kernel5 <- c(kernel1[1] - 2*r, 1-(kernel1[1] -2*r))

#see generative parameters script. 
#movement probabilities from kernel 1:
move_prob_kernel1 <- read.csv(here::here("data", "movement", "movementprobs_k1.csv"))[-1]
move_prob_kernel1 <- as.matrix(move_prob_kernel1)
colnames(move_prob_kernel1) <- seq(1,I)
rownames(move_prob_kernel1) <- seq(1,I)

#movement probabilities from kernel 2:
move_prob_kernel2 <- move_prob_kernel1
move_prob_kernel2[move_prob_kernel2 == 0.85] <- kernel2[1]
move_prob_kernel2[move_prob_kernel2 == 0.15] <- kernel2[2]


#movement probabilities from kernel 3:
move_prob_kernel3 <- move_prob_kernel1
move_prob_kernel3[move_prob_kernel3 == 0.85] <- kernel3[1]
move_prob_kernel3[move_prob_kernel3 == 0.15] <- kernel3[2]

#movement probabilities from kernel 4:
move_prob_kernel4 <- move_prob_kernel1
move_prob_kernel4[move_prob_kernel4 == 0.85] <- kernel4[1]
move_prob_kernel4[move_prob_kernel4 == 0.15] <- kernel4[2]

#movement probabilities from kernel 5:
move_prob_kernel5 <- move_prob_kernel1
move_prob_kernel5[move_prob_kernel5 == 0.85] <- kernel5[1]
move_prob_kernel5[move_prob_kernel5 == 0.15] <- kernel5[2]

#put all the kernels together
move_prob_kernels <- array(NA, dim = c(I,I,5))
move_prob_kernels[,,1] <- move_prob_kernel1
move_prob_kernels[,,2] <- move_prob_kernel2
move_prob_kernels[,,3] <- move_prob_kernel3
move_prob_kernels[,,4] <- move_prob_kernel4
move_prob_kernels[,,5] <- move_prob_kernel5

#randomly select one movement probability matrix for each simulation
pi_mats <- array(0, dim = c(I,I,P)) #Movement probability matrix
kernel_sims <- sample(c(1:5), P, replace = T) #selecting movement probability matrix for each sim
for(p in 1:P){
  pi_mats[,,p] <- move_prob_kernels[,,kernel_sims[p]] #fill probability matrix with chosen movement-probability matrix
}


##### *estimation model #### 
move.vals <- c(kernel1[2],kernel2[2], kernel3[2], kernel4[2], kernel5[2])

max(move.vals)
min(move.vals)

lik_stay <-function(parms){
  alpha<-parms[1]
  beta<-parms[2]
  
  mean <- alpha/(alpha + beta)
  lower <- qbeta(0.05,alpha,beta,lower.tail = TRUE)
  upper <- qbeta(0.05,alpha,beta,lower.tail = FALSE)
  
  sum <- (mean - 0.15)^2+(lower-0.05)^2+(upper-.25)^2
  #             0.15 = mean, 0.05 = min, 0.25 = max
}

out_stay <- optim(c(1,1), lik_stay, method = "L-BFGS-B",lower = 0.5,upper = Inf)

#hist(rbeta(100000,out_stay$par[1],out_stay$par[2]))

#year 1 estimation priors are: 
move.est.alpha <- out_stay$par[1]
move.est.beta <- out_stay$par[2]
  
#### Parameter 5 - downstream movement ####
##### *operating model #####
ds <- runif(P,0.5,1) #downstream movement probability 

##### *estimation model #####
lik_direction <-function(parms){
  alpha<-parms[1]
  beta<-parms[2]
  
  mean <- alpha/(alpha + beta)
  lower <- qbeta(0.05,alpha,beta,lower.tail = TRUE)
  upper <- qbeta(0.05,alpha,beta,lower.tail = FALSE)
  
  sum <- (mean - 0.75)^2+(lower-0.5)^2+(upper-1)^2
}


out_direction <- optim(c(1,1), lik_direction, method = "L-BFGS-B",lower = 0.5,upper = Inf)

#hist(rbeta(100000,out_direction$par[1],out_direction$par[2]))

#year 1 estimation priors are: 
ds.move.est.alpha <- out_direction$par[1]
ds.move.est.beta <- out_direction$par[2]

#### Stable distribution ####
#The annual (pre-breeding) leslie matrix will inform the stable age distribution:
L_annual <- array(0, dim = c(4,4,S))
L_annual[1,1,]<- (survival_age_est[1,]^12)*fecund_est[2,]
L_annual[1,2,]<- (survival_age_est[2,]^12)*fecund_est[3,]
L_annual[1,3,]<- (survival_age_est[3,]^12)*fecund_est[4,]
L_annual[1,4,]<- (survival_age_est[4,]^12)*fecund_est[4,]
L_annual[2,1,] <- (survival_age_est[1,]^12)
L_annual[3,2,] <- (survival_age_est[2,]^12)
L_annual[4,3,] <- (survival_age_est[3,]^12)
L_annual[4,4,] <- (survival_age_est[4,]^12)

stable_dist <- array(NA, dim = c(4,S))

for(s in 1:S){
  stable_dist[,s] <- stable.stage(L_annual[,,s]) 
}

mean.stable <- mean(1- stable_dist[1,])
sd.stable <- sd(1- stable_dist[1,])

stable.est.alpha <- alpha_in_beta_moment(mean.stable, sd.stable)
stable.est.beta <- beta_in_beta_moment(stable.est.alpha, mean.stable)

hist(rbeta(10000,stable.est.alpha, stable.est.beta))

#### Save workspace ####
save.image(file = "parameters.RData")

#load("parameters.RData")
