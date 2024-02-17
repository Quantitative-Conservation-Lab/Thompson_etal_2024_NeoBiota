#Comparing the decision scenarios: 
library(tidyverse)
library(here)
library(abind)
library(plyr)
library(kableExtra)
library(gganimate)
library(gridExtra)
library(MCMCvis)
library(latex2exp)
library(coda)
library(scales)
library(data.table)

path <- here::here("results")

####No control####  
path2 <- "nocontrol"
file_name = paste(path, path2,'DR_nocontrol_N_all.csv',sep = '/')
N_allsegssims_nocontrol1 <- fread(file_name)
N_allsegssims_nocontrol1 <- data.frame(N_allsegssims_nocontrol1)[-1]

#Dafter
file_name = paste(path, path2,'DR_nocontrol_D_all.csv',sep = '/')
Dafter_allsegssims_nocontrol1 <- fread(file_name)
Dafter_allsegssims_nocontrol1 <- data.frame(Dafter_allsegssims_nocontrol1)[-1]

#Initial pop: 0
Ninit_allsegssims_nocontrol1 <- filter(N_allsegssims_nocontrol1, primary == 1)
Ninit_allsegssims_nocontrol1$primary <- 0

#remove age = 0
Ninit_allsegssims_nocontrol1$age <- Ninit_allsegssims_nocontrol1$age -1
Ninit_allsegssims_nocontrol1 <- filter(Ninit_allsegssims_nocontrol1, age > 0)

Dafter_allsegssims_nocontrol1$age <- Dafter_allsegssims_nocontrol1$age -1
Dafter_allsegssims_nocontrol1 <- filter(Dafter_allsegssims_nocontrol1, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_nocontrol1 <- rbind(Ninit_allsegssims_nocontrol1, Dafter_allsegssims_nocontrol1)

N_allsims <- Dafter_mean_allseg_nocontrol1
head(N_allsims)

N_allsims <- N_allsims %>% filter(primary <= 84) #7 years of simulation
N_allsims <- N_allsims %>% filter(param == 2)

#sum across ages 
N_allsims_sumage  <- aggregate(count ~ segment + primary + sim, 
                               data = as.data.frame(N_allsims), 
                               FUN = sum)

nocontrol_p2 <- N_allsims_sumage

nocontrol_p2$dr <- "nocontrol"

####Truly rand ####  
path2 <- "trulyrandom"
file_name = paste(path, path2,'DR_rand_N_all.csv',sep = '/')
N_allsegssims_rand1 <- fread(file_name)
N_allsegssims_rand1 <- data.frame(N_allsegssims_rand1)[-1]

#Dafter
file_name = paste(path, path2,'DR_rand_D_all.csv',sep = '/')
Dafter_allsegssims_rand1 <- fread(file_name)
Dafter_allsegssims_rand1 <- data.frame(Dafter_allsegssims_rand1)[-1]

#Initial pop: 0
Ninit_allsegssims_rand1 <- filter(N_allsegssims_rand1, primary == 1)
Ninit_allsegssims_rand1$primary <- 0

#remove age = 0
Ninit_allsegssims_rand1$age <- Ninit_allsegssims_rand1$age -1
Ninit_allsegssims_rand1 <- filter(Ninit_allsegssims_rand1, age > 0)

Dafter_allsegssims_rand1$age <- Dafter_allsegssims_rand1$age -1
Dafter_allsegssims_rand1 <- filter(Dafter_allsegssims_rand1, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_rand1 <- rbind(Ninit_allsegssims_rand1, Dafter_allsegssims_rand1)

N_allsims <- Dafter_mean_allseg_rand1
head(N_allsims)

N_allsims <- N_allsims %>% filter(primary <= 84) #7 years of simulation
N_allsims <- N_allsims %>% filter(param == 2)

#sum across ages 
N_allsims_sumage  <- aggregate(count ~ segment + primary + sim, 
                           data = as.data.frame(N_allsims), 
                           FUN = sum)

trulyrand_p2 <- N_allsims_sumage

trulyrand_p2$dr <- "random"

#### edge_old ####  
path2 <- "edge_old/edge_p2_s1"
file_name = paste(path, path2,'DR_edge_N_all.csv',sep = '/')
N_allsegssims_edge1 <- fread(file_name)
N_allsegssims_edge1 <- data.frame(N_allsegssims_edge1)[-1]

#Dafter
file_name = paste(path, path2,'DR_edge_D_all.csv',sep = '/')
Dafter_allsegssims_edge1 <- fread(file_name)
Dafter_allsegssims_edge1 <- data.frame(Dafter_allsegssims_edge1)[-1]

#Initial pop: 0
Ninit_allsegssims_edge1 <- filter(N_allsegssims_edge1, primary == 1)
Ninit_allsegssims_edge1$primary <- 0

#remove age = 0
Ninit_allsegssims_edge1$age <- Ninit_allsegssims_edge1$age -1
Ninit_allsegssims_edge1 <- filter(Ninit_allsegssims_edge1, age > 0)

Dafter_allsegssims_edge1$age <- Dafter_allsegssims_edge1$age -1
Dafter_allsegssims_edge1 <- filter(Dafter_allsegssims_edge1, age > 0)


#### edge_old s2 ####  
path2 <- "edge_old/edge_p2_s2"
file_name = paste(path, path2,'DR_edge_N_all.csv',sep = '/')
N_allsegssims_edge2 <- fread(file_name)
N_allsegssims_edge2 <- data.frame(N_allsegssims_edge2)[-1]
N_allsegssims_edge2$sim <- N_allsegssims_edge2$sim + 5

#Dafter
file_name = paste(path, path2,'DR_edge_D_all.csv',sep = '/')
Dafter_allsegssims_edge2 <- fread(file_name)
Dafter_allsegssims_edge2 <- data.frame(Dafter_allsegssims_edge2)[-1]
Dafter_allsegssims_edge2$sim <- Dafter_allsegssims_edge2$sim + 5

#Initial pop: 0
Ninit_allsegssims_edge2 <- filter(N_allsegssims_edge2, primary == 1)
Ninit_allsegssims_edge2$primary <- 0

#remove age = 0
Ninit_allsegssims_edge2$age <- Ninit_allsegssims_edge2$age -1
Ninit_allsegssims_edge2 <- filter(Ninit_allsegssims_edge2, age > 0)

Dafter_allsegssims_edge2$age <- Dafter_allsegssims_edge2$age -1
Dafter_allsegssims_edge2 <- filter(Dafter_allsegssims_edge2, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_edges <- rbind(Ninit_allsegssims_edge1, Dafter_allsegssims_edge1,
                                  Ninit_allsegssims_edge2, Dafter_allsegssims_edge2)

N_allsims <- Dafter_mean_allseg_edges
head(N_allsims)

N_allsims <- N_allsims %>% filter(primary <= 84) #7 years of simulation

#sum across ages 
edge_p2  <- aggregate(count ~ segment + primary + sim, 
                               data = as.data.frame(N_allsims), 
                               FUN = sum)

edge_p2$dr <- 'edge'

#### grow_old ####  
path2 <- "grow_old/grow_p2_s1"
file_name = paste(path, path2,'DR_grow_N_all.csv',sep = '/')
N_allsegssims_grow1 <- fread(file_name)
N_allsegssims_grow1 <- data.frame(N_allsegssims_grow1)[-1]

#Dafter
file_name = paste(path, path2,'DR_grow_D_all.csv',sep = '/')
Dafter_allsegssims_grow1 <- fread(file_name)
Dafter_allsegssims_grow1 <- data.frame(Dafter_allsegssims_grow1)[-1]

#Initial pop: 0
Ninit_allsegssims_grow1 <- filter(N_allsegssims_grow1, primary == 1)
Ninit_allsegssims_grow1$primary <- 0

#remove age = 0
Ninit_allsegssims_grow1$age <- Ninit_allsegssims_grow1$age -1
Ninit_allsegssims_grow1 <- filter(Ninit_allsegssims_grow1, age > 0)

Dafter_allsegssims_grow1$age <- Dafter_allsegssims_grow1$age -1
Dafter_allsegssims_grow1 <- filter(Dafter_allsegssims_grow1, age > 0)


#### grow_old s2 ####  
path2 <- "grow_old/grow_p2_s2"
file_name = paste(path, path2,'DR_grow_N_all.csv',sep = '/')
N_allsegssims_grow2 <- fread(file_name)
N_allsegssims_grow2 <- data.frame(N_allsegssims_grow2)[-1]
N_allsegssims_grow2$sim <- N_allsegssims_grow2$sim + 5

#Dafter
file_name = paste(path, path2,'DR_grow_D_all.csv',sep = '/')
Dafter_allsegssims_grow2 <- fread(file_name)
Dafter_allsegssims_grow2 <- data.frame(Dafter_allsegssims_grow2)[-1]
Dafter_allsegssims_grow2$sim <- Dafter_allsegssims_grow2$sim + 5

#Initial pop: 0
Ninit_allsegssims_grow2 <- filter(N_allsegssims_grow2, primary == 1)
Ninit_allsegssims_grow2$primary <- 0

#remove age = 0
Ninit_allsegssims_grow2$age <- Ninit_allsegssims_grow2$age -1
Ninit_allsegssims_grow2 <- filter(Ninit_allsegssims_grow2, age > 0)

Dafter_allsegssims_grow2$age <- Dafter_allsegssims_grow2$age -1
Dafter_allsegssims_grow2 <- filter(Dafter_allsegssims_grow2, age > 0)

#Combine init with Dafter
Dafter_mean_allseg_grows <- rbind(Ninit_allsegssims_grow1, Dafter_allsegssims_grow1,
                                  Ninit_allsegssims_grow2, Dafter_allsegssims_grow2)

N_allsims <- Dafter_mean_allseg_grows
head(N_allsims)

N_allsims <- N_allsims %>% filter(primary <= 84) #7 years of simulation

#sum across ages 
grow_p2  <- aggregate(count ~ segment + primary + sim, 
                      data = as.data.frame(N_allsims), 
                      FUN = sum)

grow_p2$dr <- 'grow'

#### COMBINE ####
N_p2 <- rbind(trulyrand_p2, edge_p2, grow_p2, nocontrol_p2)

N_p2_fin <- N_p2 %>% filter(primary == max(N_p2$primary))

##### 
N_p2_fin_segs_avgs <- aggregate(count ~ segment + primary + dr, 
                                data = as.data.frame(N_p2_fin), 
                                FUN = mean)


N_p2_fin_avgs <- aggregate(count ~ dr, 
                           data = as.data.frame(N_p2_fin_segs_avgs), 
                           FUN = sum)

#### MINIMIZE ABUNDANCE ####
min_abund <- aggregate(count ~ dr + sim, 
                                data = as.data.frame(N_p2_fin), 
                                FUN = sum)
ggplot(min_abund)+
  geom_boxplot(aes(x = dr, y = count))


#### MINIMIZE SPATIAL EXTENT ####
min_spatial <- N_p2_fin

head(min_spatial)

min_spatial$pop <- NA
min_spatial$pop <- replace(min_spatial$pop, min_spatial$count > 0 , 1)
min_spatial$pop <- replace(min_spatial$pop, min_spatial$count == 0 , 0)

#sum the number of segments where count == 0 for each simulation (max = 35)
min_spatial  <- aggregate(pop ~ sim + dr, 
                         data = as.data.frame(min_spatial), 
                         FUN = sum) #sum across ages

ggplot(min_spatial)+
  geom_boxplot(aes(x = dr, y = pop/35))
