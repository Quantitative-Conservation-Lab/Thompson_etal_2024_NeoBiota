#libraries
library(here)
library(gtools)
library(MCMCvis)
library(abind)
library(tidyverse)
library(boot)
library(coda)
library(plyr)
library(popbio)
library(dplyr)

start.time <- Sys.time()

#------------------------------------------------------------------------------#
#### Change name ####
path <- here::here("results","random", "random_p1")

#------------------------------------------------------------------------------#
#### Data ####
load("parameters.RData")

N.years <- 8 #Number of years that we will be running the simulation
J <- 12*N.years #Number of total initial primary periods
K <- 10 #Number of secondary periods (abundance measurements per site), needs to be larger
I <- 35 #Number of segments -total spatial units
Ages <- 4 #4 age classes
S <- 50 #Number of simulations
P <- 20 #parameter sets

numrem <- c(4,8,16) #segments where removal occurs
Rem <- length(numrem) #segments where removal occurs

#Temperature data:
#loading monthly% of degree days < 6C (for movement)
data.file <- here::here("data", "temperature", "JDR_20km_u6.csv")
u6.temp.dat <- read.csv(data.file, header = T)

#data wrangling temperature data:
u6.temp <- u6.temp.dat %>% arrange(seg)
u6.temp <- u6.temp.dat[-1] #remove segment column and fork column for analysis
u6.temp <- u6.temp %>% dplyr::select(jun,jul,aug,sep,oct,nov,dec,jan,feb,mar,apr,may) #reordering 
u6.temp <- do.call(cbind, replicate((ceiling(J/12)), u6.temp, simplify=FALSE)) #temperature for whole time

#### A. Model set up ####
#### A1: Creating Arrays ####
##----1a: Arrays for removal model----## 
n.trap <- 2000 #number of traps per segment #ignore this for now

Y <- array(0, dim = c(I,J,K,Ages,P,S,Rem)) #removal data (integer)
N.truth <- array(0, dim = c(I,J,K,Ages,P,S,Rem)) #true population abundance
R <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population remaining after removals at j
D <- array(0, dim = c(I,J,Ages,P,S,Rem)) #individuals available for dispersal 
D.excess <- array(0, dim = c(I,J,P,S,Rem)) #used to truncate D by carrying capacity
D.stay <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population that is staying
D.down <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population that moves downstream
D.up <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population that moves upstream
D.fork <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population that moves to a new fork
D.after <- array(0, dim = c(I,J,Ages,P,S,Rem)) #population after movement

#Entering Columbia
D.columbia <- array(0, dim = c(J,Ages,P,S,Rem))

#indexing when trapping occurs (always first 4 months of the year)
time.trap <- c(1,1,1,1,0,0,0,0,0,0,0,0)
time.traps <- rep(time.trap, N.years)

#### param 1 ####
#removal for each age p
p <- cap_eff[1:P]
p2 <- (n.trap)/(8000)*p 

##----1b: Arrays for population change model----## 
phi <- array(0, dim = c(I,J,Ages,P,S,Rem)) #survival
june <- seq(1,J, by = 12) #June indices (when reproduction occurs)

#### param 2 ####
#June growth rate:
L.june <- array(0, dim = c(4,4,P)) #leslie matrix

lambda.age <- array(0, dim = c(Ages,P))

for(p in 1:P){
  
  lambda.age[1,p] <- survival_age[1,p]*mature_age[2,p]*fecund_age[2,p]
  lambda.age[2,p] <- survival_age[2,p]*mature_age[3,p]*fecund_age[3,p]
  lambda.age[3,p] <- survival_age[3,p]*mature_age[4,p]*fecund_age[4,p]
  lambda.age[4,p] <- survival_age[4,p]*mature_age[4,p]*fecund_age[4,p]
  
}

for(p in 1:P){
  L.june[1,,p] <- lambda.age[,p]#filling in fecundity for Leslie matrix June
  L.june[2,1,p] <- survival_age[1,p]
  L.june[3,2,p] <- survival_age[2,p]
  L.june[4,3,p] <- survival_age[3,p]
  L.june[4,4,p] <- survival_age[4,p]
}

#Find growth rate for june:
grow.june <- rep(NA, P)

for(p in 1:P){
  grow.june[p] <- lambda(L.june[,,p])
}


#### param 3 ####
#Not june growth rate:
L.notjune <- array(0, dim = c(4,4,P)) #leslie matrix

for(p in 1:P){
  L.notjune[1,1,p] <- survival_age[1,p]
  L.notjune[2,2,p] <- survival_age[2,p]
  L.notjune[3,3,p] <- survival_age[3,p]
  L.notjune[4,4,p] <- survival_age[4,p] 
}

grow.notjune <- rep(NA, P)

for(p in 1:P){
  grow.notjune[p] <- lambda(L.notjune[,,p])
}


#### Initial N #####
data.file <- here::here("data", "initial_population", "JDR_20km_initpop.csv")
initpop.dat <- read.csv(data.file, header = T)
initpop.dat <- initpop.dat %>% arrange(Segment)
init_density <- initpop.dat$Density
area <- 20000*10 #segment area
popK <- 2*round(max(init_density)*area) #carrying capacity = 2*maximum initial population

initpop <- round(init_density*area) #initial population (summed across age classes)

#The annual (pre-breeding) leslie matrix will inform the stable age distribution:
L.annual <- array(0, dim = c(4,4,P))

for(p in 1:P){
  L.annual[1,1,p]<- (survival_age[1,p]^12)*mature_age[2,p]*fecund_age[2,p]
  L.annual[1,2,p]<- (survival_age[2,p]^12)*mature_age[3,p]*fecund_age[3,p]
  L.annual[1,3,p]<- (survival_age[3,p]^12)*mature_age[4,p]*fecund_age[4,p]
  L.annual[1,4,p]<- (survival_age[4,p]^12)*mature_age[4,p]*fecund_age[4,p]
  L.annual[2,1,p] <- (survival_age[1,p]^12)
  L.annual[3,2,p] <- (survival_age[2,p]^12)
  L.annual[4,3,p] <- (survival_age[3,p]^12)
  L.annual[4,4,p] <- (survival_age[4,p]^12)
}

stable.dist <- array(NA, dim = c(4,P))

for(p in 1:P){
  stable.dist[,p] <- stable.stage(L.annual[,,p]) 
}

for(p in 1:P){
  for(i in 1:I){
    N.truth[i,1,1,1:Ages,p,1:S,1:Rem] <- round(t(stable.dist[,p])* initpop[i])
  }
}

#### Random removal ####
site.traps <- array(NA, dim = c(N.years, numrem[length(numrem)], P, S, Rem))#rep(NA, S)

for(year in 1:N.years){
  for(p in 1:P){
    for(r in 1:Rem){
      for(s in 1:S){
        site.traps[year,1:numrem[r],p, s, r]<- sample(seq(1:I),numrem[r],replace = F)
      }
    }
  }
}

decision.date <- seq(1,6)*12 + 1
yearval <- rep(seq(1,N.years), each = 12)

#### param 4 ####
move <- pi_mats[1,2,] #probability of moving 
ds <- ds #probability of moving downstream given moving

#identifying where downstream movement occurred (ex. for i = 2, moved.down = 1 and for i = 25, moved.down = 24 and 28)
moved.down <- matrix(NA, nrow = I, ncol = 2)
moved.down[,1] <- seq(0,(I-1))
moved.down[6,2] <- 26
moved.down[8,2] <- 33
moved.down[25,2] <- 28
moved.down[31,2] <- 35
moved.down[22,1:2] <- NA #cant move downstream from 22

n.down <- rep(1,I) #number of segments that can move downstream into i
n.down[c(6,8,25,31)] <- 2 #two segments can move downstream into i 

moved.up <- seq(2,I+1) #number of segments that can move upstream into i

moved.up[35] <- 35 #35 does not have any upstream movement from the sam esegment so just set it to 35

ups <- rep(1,I)
ups[c(26, 28, 33, 35)] <- 0 #35,28,33 do not have any upstream movement from the same fork

#in the forking calculation: if fork1 = 1 then we do not divide upstream population by 2, 
fork1 <- rep(1, I)
fork1[c(6,8,25,31)] <- 2

#in the forking calculation: if fork2 = 0 then we do not have forking
fork2 <- rep(0,I)
fork2[c(6,8,25,31)] <- 1

#identifying where bifurcation movement occurred 
bifurcation <- rep(1, I)
bifurcation[26] <- 6
bifurcation[33] <- 8
bifurcation[28] <- 25
bifurcation[35] <- 31

#not edge segments
not.edge <- setdiff(seq(1,I), c(1,22,23,27,29,34))
n.not.edge <- length(not.edge)

#month indices
june <- seq(1,N.years*12, by = 12) 
may <- seq(12,N.years*12, by = 12)

year <- rep(1:N.years, each = 12)
N.decision <- array(NA, dim = c(I,J,P,S,Rem))

N.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
grow.jun <- array(NA, dim = c(P,S,Rem))
grow.notjun <- array(NA, dim = c(P,S,Rem))
D.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
D.stay.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
D.down.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
D.up.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
D.fork.decision <- array(NA, dim = c(I,J+1,P,S,Rem))
D.after.decision <- array(NA, dim = c(I,J+1,P,S,Rem))

growth.decision <- array(NA, dim = c(I,J+1,P,S,Rem))

###################################################################################

#### Simulate ####
for(r in 1:Rem){ #removal locations
for(p in 1:P){ #parameter set
for(s in 1:S){ #simulation
  for(j in 1:J){ #primary period (month)
    
    #### Removal ####
    for(i in 1:I){ #for each segment:
      
      for(a in 2:Ages){ #for ages 2-4
        if(i %in% site.traps[year[j],1:numrem[r],p,s,r]){
          Y[i,j,1,a,p,s,r] <- rbinom(1,N.truth[i,j,1,a,p,s,r],p2[p]) * time.traps[j] #removals
        } else{
          
          Y[i,j,1,a,p,s,r] <- 0
          
        }
        
      }
      
      Y[i,j,1,1,p,s,r] <- 0 #no removals of age class 0
      
      for(k in 2:K){ #for secondary periods 2: K
        for(a in 1:Ages){ #for each age
          #True population abundance = N.truth
          N.truth[i,j,k,a,p,s,r] <- max(0, N.truth[i,j,k-1,a,p,s,r] - Y[i,j,k-1,a,p,s,r]) #True pop = population at previous secondary - removals at previous secondary
        }
        for(a in 2:Ages){
          if(i %in% site.traps[year[j],1:numrem[r],p,s,r]){
            Y[i,j,k,a,p,s,r] <- rbinom(1,N.truth[i,j,k,a,p,s,r],p2[p]) * time.traps[j] #removals
          } else{
            Y[i,j,k,a,p,s,r] <- 0
            
          }
        }
        
        Y[i,j,k,1,p,s,r] <- 0 #no removals of age class 0
      }
      
      for(a in 1:Ages){
        
        #Population remaining at the end of primary removal period j:
        R[i,j,a,p,s,r] <- N.truth[i,j,K,a,p,s,r]
        
      } #ends ages loop
      
      #### Population change #####
      #June population change: growth * population and truncated by carrying capacity
      if(j %in% june){ #if month = june
        D[i,j,,p,s,r] <- round(L.june[,,p] %*% R[i,j,,p,s,r])
        
      } else{ 
        #not June population change: growth * population and truncated by carrying capacity
        D[i,j,,p,s,r] <- round(L.notjune[,,p] %*% R[i,j,,p,s,r])
      }
      
      #update D if above carrying capacity.
      #If D is > carrying capacity, remove age 0 individuals
      for(a in 1:Ages){
        if(sum(D[i,j,1:Ages,p,s,r]) > popK){
          D.excess[i,j,p,s,r] <- sum(D[i,j,1:Ages,p,s,r]) - (popK)
          D[i,j,1,p,s,r] <- D[i,j,1,p,s,r] - D.excess[i,j,p,s,r]
          if(D[i,j,1,p,s,r] < 0){
            D[i,j,2,p,s,r] <- D[i,j,2,p,s,r] - abs(D[i,j,1,p,s,r])
            D[i,j,1,p,s,r] <- 0
          }
          if(D[i,j,2,p,s,r] < 0){
            D[i,j,3,p,s,r] <- D[i,j,3,p,s,r] - abs(D[i,j,2,p,s,r])
            D[i,j,2,p,s,r] <- 0
          }
          if(D[i,j,3,p,s,r] < 0){
            D[i,j,4,p,s,r] <- D[i,j,4,p,s,r] - abs(D[i,j,3,p,s,r])
            D[i,j,3,p,s,r] <- 0
          }
          if(D[i,j,4,p,s,r] < 0){
            D[i,j,4,p,s,r] <- 0
          }
          
        }
      }
      
      
    } #ends I loop
    
    #####  Movement ######  
    for(a in 2:Ages){
      for(i in 1:I){
        #calculate individuals that stay
        D.stay[i,j,a,p,s,r] <- rbinom(1, D[i,j,a,p,s,r], 1- (0.5*move[p]) + (0.5*move[p]*u6.temp[i,j])) 
        
        #calculate individuals that move downstream
        D.down[i,j,a,p,s,r] <- rbinom(1,D[i,j,a,p,s,r] - D.stay[i,j,a,p,s,r], ds[p])
        
        #Calculate individuals that move upstream
        D.up[i,j,a,p,s,r] <- floor((D[i,j,a,p,s,r] - D.stay[i,j,a,p,s,r] - D.down[i,j,a,p,s,r])/fork1[i])
        
        #Calculating individuals that move to another fork upstream (only possible at i = 6, 8, 25, 31)
        D.fork[i,j,a,p,s,r] <- fork2[i]*(D[i,j,a,p,s,r] - D.stay[i,j,a,p,s,r] - D.down[i,j,a,p,s,r] - D.up[i,j,a,p,s,r])
        #if fork2[i] --> 1 bifurcation, if 0 -> no
      }
      
      for(i in 1:n.not.edge){
        D.after[not.edge[i],j,a,p,s,r] <- D.stay[not.edge[i],j,a,p,s,r] + #stay
          sum(D.down[moved.down[not.edge[i],1:n.down[not.edge[i]]],j,a,p,s,r]) + #moved in by going downstream
          ups[not.edge[i]]*D.up[moved.up[not.edge[i]],j,a,p,s,r] + D.fork[bifurcation[not.edge[i]],j,a,p,s,r] #moved in by going upstream
      }
      
      #Edges: can only move upstream from each of these spots
      D.after[1,j,a,p,s,r] <- D.stay[1,j,a,p,s,r] + D.up[2,j,a,p,s,r] + D.up[1,j,a,p,s,r]
      D.after[23,j,a,p,s,r] <- D.stay[23,j,a,p,s,r] + D.up[24,j,a,p,s,r] + D.up[23,j,a,p,s,r]
      D.after[27,j,a,p,s,r] <- D.stay[27,j,a,p,s,r] + D.up[28,j,a,p,s,r] + D.up[27,j,a,p,s,r]
      D.after[29,j,a,p,s,r] <- D.stay[29,j,a,p,s,r] + D.up[30,j,a,p,s,r] + D.up[29,j,a,p,s,r]
      D.after[34,j,a,p,s,r] <- D.stay[34,j,a,p,s,r] + D.up[35,j,a,p,s,r] + D.up[34,j,a,p,s,r]
      
      #Edge: closest to columbia river
      D.after[22,j,a,p,s,r] <- D.stay[22,j,a,p,s,r] + D.down[21,j,a,p,s,r] 
      
      #Entering Columbia
      D.columbia[j,a,p,s,r] <- D.down[22,j,a,p,s,r]
      
    }
    
    D.columbia[j,1,p,s,r] <- 0 #age 0 never moves  
    D.after[1:I,j,1,p,s,r] <- D[1:I,j,1,p,s,r] #age 0 never moves    
    
    #Next primary period abundance 
    for(h in 1:I){
      #abundance at the start of each primary period (k = 1) -except for initial abundance 
      N.truth[h,2:J, 1,1:Ages,p,s,r] <- D.after[h,1:(J-1),1:Ages,p,s,r]
    }
   
    
  } #ends J loop
} #ends simulation
}
}

############################################################################
#### Save DATA ####
rem.rate <- 1
#---------N data ---------#
N_all <- N.truth[,,1,,,,]
N_all <- adply(N_all, c(1,2,3,4,5,6))
colnames(N_all) <- c("segment", "primary", "age","param", "sim", "rem", "count")
N_all$p <- rem.rate
file_name = paste(path, 'N.csv',sep = '/')
write.csv(N_all,file_name)

#--------- D After ---------#
D_all <- adply(D, c(1,2,3,4,5,6))
colnames(D_all) <- c("segment", "primary", "age", "param", "sim","rem", "count")
D_all$p <- rem.rate
file_name = paste(path, 'D.csv',sep = '/')
write.csv(D_all,file_name)

#--------- Removal data ---------#
Y_all <- adply(Y, c(1,2,3,4,5,6,7))
colnames(Y_all) <- c("segment", "primary", "secondary", "age", "param", "sim", "rem", "count")
Y_all$p <- rem.rate
file_name = paste(path, 'Y.csv',sep = '/')
write.csv(Y_all,file_name)

#--------- Sites visited ---------#
site.df <- adply(site.traps, c(1,2,3,4,5))
colnames(site.df) <- c("year", "removal.num", "param", "sim", "rem", "site")
site.df$p <- rem.rate
file_name = paste(path, 'site_visit.csv',sep = '/')
write.csv(site.df,file_name)

#--------- distance traveled ---------#
d.traveled <- array(NA, c(N.years, P, S, Rem))

load("data/parameters/d.matrix.RData")

for(year in 1:N.years){
  for(p in 1:P){
    for(s in 1:S){
      for(r in 1:Rem){
        
        d.traveled[year,p,s,r] <- d.matrix[site.traps[year,1,p,s,r], site.traps[year,2,p,s,r]]
        
        for(v in 2:(numrem[r]-1)){
          d.traveled[year,p,s,r] <- d.traveled[year,p,s,r] + d.matrix[site.traps[year,v,p,s,r], site.traps[year,v+1,p,s,r]]
        }
        
      }
    }
  }
}

dist.travel <- adply(d.traveled[1:N.years,1:P, 1:S,1:Rem], c(1,2,3,4))
colnames(dist.travel) <- c("year", "param", "sim", "rem", "distance")
site.df$p <- rem.rate
file_name = paste(path, 'site_visit.csv',sep = '/')
write.csv(dist.travel,file_name)

#---- timing ---- #
end.time <- Sys.time()
time.taken <- end.time - start.time
file_name = paste(path, 'time.txt',sep = '/')
write.table(time.taken,file_name)

