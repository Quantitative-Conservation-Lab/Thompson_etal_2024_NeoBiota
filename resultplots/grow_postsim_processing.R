library(tidyverse)
library(here)
library(plyr)
library(data.table)

#### grow data  ####
#Might have to change E: to E: depending on the number of external drives used

##### grow_1rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p1_s1 <- fread(file_name)
N_grow_1_p1_s1 <- data.frame(N_grow_1_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p1_s1 <- fread(file_name)
D_grow_1_p1_s1 <- data.frame(D_grow_1_p1_s1)

#Initial pop: 0
N_grow_1_p1_s1_1 <- filter(N_grow_1_p1_s1, primary == 1)
N_grow_1_p1_s1_1$primary <- 0

#remove age = 0
N_grow_1_p1_s1_1$age <- N_grow_1_p1_s1_1$age -1
N_grow_1_p1_s1_1 <- filter(N_grow_1_p1_s1_1, age > 0)

D_grow_1_p1_s1$age <- D_grow_1_p1_s1$age -1
D_grow_1_p1_s1 <- filter(D_grow_1_p1_s1, age > 0)

#Combine init with Dafter
N_grow_1_p1_s1 <- rbind(N_grow_1_p1_s1_1, D_grow_1_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p1_s2 <- fread(file_name)
N_grow_1_p1_s2 <- data.frame(N_grow_1_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p1_s2 <- fread(file_name)
D_grow_1_p1_s2 <- data.frame(D_grow_1_p1_s2)

#Initial pop: 0
N_grow_1_p1_s2_1 <- filter(N_grow_1_p1_s2, primary == 1)
N_grow_1_p1_s2_1$primary <- 0

#remove age = 0
N_grow_1_p1_s2_1$age <- N_grow_1_p1_s2_1$age -1
N_grow_1_p1_s2_1 <- filter(N_grow_1_p1_s2_1, age > 0)

D_grow_1_p1_s2$age <- D_grow_1_p1_s2$age -1
D_grow_1_p1_s2 <- filter(D_grow_1_p1_s2, age > 0)

#Combine init with Dafter
N_grow_1_p1_s2 <- rbind(N_grow_1_p1_s2_1, D_grow_1_p1_s2)

N_grow_1_p1_allages <- rbind(N_grow_1_p1_s1, N_grow_1_p1_s2)

#Sum ages 
N_grow_1_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                         data = as.data.frame(N_grow_1_p1_allages), 
                         FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_1_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                              data = as.data.frame(N_grow_1_p1), 
                              FUN = mean)

###### Average final N @ sites #####
N_grow_1_p1_segfin <- N_grow_1_p1_mean %>% filter(primary == max(N_grow_1_p1_mean$primary))

#sum segments
N_grow_1_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                              data = as.data.frame(N_grow_1_p1), 
                              FUN = sum)
###### Final total N #####
N_grow_1_p1_Nfin <- N_grow_1_p1_Nsum %>% filter(primary == max(N_grow_1_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_1_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                   data = as.data.frame(N_grow_1_p1_Nsum), 
                                   FUN = mean)

N_grow_1_p1_CIl <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p1_Nsum), 
                             function(x) quantile(x, probs = 0.1))

colnames(N_grow_1_p1_CIl)[4] <- 'low.1'

N_grow_1_p1_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p1_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_grow_1_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_1_p1_Nvtime <- cbind(N_grow_1_p1_Nsum_mean,
                          low.1 = N_grow_1_p1_CIl$low.1,
                          high.9 = N_grow_1_p1_CIh$high.9)

# ggplot(grow_1_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p1_s1 <- fread(file_name)
Dcol_grow_1_p1_s1 <- data.frame(Dcol_grow_1_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p1_s2 <- fread(file_name)
Dcol_grow_1_p1_s2 <- data.frame(Dcol_grow_1_p1_s2)

Dcol_grow_1_p1 <- rbind(Dcol_grow_1_p1_s1, Dcol_grow_1_p1_s2)

###### D Columbia ######
Dcol_grow_1_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                data = as.data.frame(Dcol_grow_1_p1), 
                                FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p1_s1 <- fread(file_name)
Dtrav_grow_1_p1_s1 <- data.frame(Dtrav_grow_1_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p1_s2 <- fread(file_name)
Dtrav_grow_1_p1_s2 <- data.frame(Dtrav_grow_1_p1_s2)

Dtrav_grow_1_p1 <- rbind(Dtrav_grow_1_p1_s1, Dtrav_grow_1_p1_s2)

###### Distance traveled ######
Dtrav_grow_1_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_grow_1_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_1_p1_fin <- N_grow_1_p1 %>% filter(primary == max(N_grow_1_p1$primary))

Ninvade_grow_1_p1 <- N_grow_1_p1_fin
colnames(Ninvade_grow_1_p1)[7] <- 'invade'
Ninvade_grow_1_p1$invade[Ninvade_grow_1_p1$invade <= 1000] <- 0
Ninvade_grow_1_p1$invade[Ninvade_grow_1_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_1_p1 <- aggregate(invade ~  param + sim +p + rem , 
                               data = as.data.frame(Ninvade_grow_1_p1), 
                               FUN = sum)


################################################################################
##### grow_1rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p2_s1 <- fread(file_name)
N_grow_1_p2_s1 <- data.frame(N_grow_1_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p2_s1 <- fread(file_name)
D_grow_1_p2_s1 <- data.frame(D_grow_1_p2_s1)

#Initial pop: 0
N_grow_1_p2_s1_1 <- filter(N_grow_1_p2_s1, primary == 1)
N_grow_1_p2_s1_1$primary <- 0

#remove age = 0
N_grow_1_p2_s1_1$age <- N_grow_1_p2_s1_1$age -1
N_grow_1_p2_s1_1 <- filter(N_grow_1_p2_s1_1, age > 0)

D_grow_1_p2_s1$age <- D_grow_1_p2_s1$age -1
D_grow_1_p2_s1 <- filter(D_grow_1_p2_s1, age > 0)

#Combine init with Dafter
N_grow_1_p2_s1 <- rbind(N_grow_1_p2_s1_1, D_grow_1_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p2_s2 <- fread(file_name)
N_grow_1_p2_s2 <- data.frame(N_grow_1_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p2_s2 <- fread(file_name)
D_grow_1_p2_s2 <- data.frame(D_grow_1_p2_s2)

#Initial pop: 0
N_grow_1_p2_s2_1 <- filter(N_grow_1_p2_s2, primary == 1)
N_grow_1_p2_s2_1$primary <- 0

#remove age = 0
N_grow_1_p2_s2_1$age <- N_grow_1_p2_s2_1$age -1
N_grow_1_p2_s2_1 <- filter(N_grow_1_p2_s2_1, age > 0)

D_grow_1_p2_s2$age <- D_grow_1_p2_s2$age -1
D_grow_1_p2_s2 <- filter(D_grow_1_p2_s2, age > 0)

#Combine init with Dafter
N_grow_1_p2_s2 <- rbind(N_grow_1_p2_s2_1, D_grow_1_p2_s2)

N_grow_1_p2_allages <- rbind(N_grow_1_p2_s1, N_grow_1_p2_s2)

#Sum ages 
N_grow_1_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                         data = as.data.frame(N_grow_1_p2_allages), 
                         FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_1_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                              data = as.data.frame(N_grow_1_p2), 
                              FUN = mean)

###### Average final N @ sites #####
N_grow_1_p2_segfin <- N_grow_1_p2_mean %>% filter(primary == max(N_grow_1_p2_mean$primary))

#sum segments
N_grow_1_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                              data = as.data.frame(N_grow_1_p2), 
                              FUN = sum)
###### Final total N #####
N_grow_1_p2_Nfin <- N_grow_1_p2_Nsum %>% filter(primary == max(N_grow_1_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_1_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                   data = as.data.frame(N_grow_1_p2_Nsum), 
                                   FUN = mean)

N_grow_1_p2_CIl <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p2_Nsum), 
                             function(x) quantile(x, probs = 0.1))

colnames(N_grow_1_p2_CIl)[4] <- 'low.1'

N_grow_1_p2_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p2_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_grow_1_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_1_p2_Nvtime <- cbind(N_grow_1_p2_Nsum_mean,
                          low.1 = N_grow_1_p2_CIl$low.1,
                          high.9 = N_grow_1_p2_CIh$high.9)

# ggplot(grow_1_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p2_s1 <- fread(file_name)
Dcol_grow_1_p2_s1 <- data.frame(Dcol_grow_1_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p2_s2 <- fread(file_name)
Dcol_grow_1_p2_s2 <- data.frame(Dcol_grow_1_p2_s2)

Dcol_grow_1_p2 <- rbind(Dcol_grow_1_p2_s1, Dcol_grow_1_p2_s2)

###### D Columbia ######
Dcol_grow_1_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                data = as.data.frame(Dcol_grow_1_p2), 
                                FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p2_s1 <- fread(file_name)
Dtrav_grow_1_p2_s1 <- data.frame(Dtrav_grow_1_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p2_s2 <- fread(file_name)
Dtrav_grow_1_p2_s2 <- data.frame(Dtrav_grow_1_p2_s2)

Dtrav_grow_1_p2 <- rbind(Dtrav_grow_1_p2_s1, Dtrav_grow_1_p2_s2)

###### Distance traveled ######
Dtrav_grow_1_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_grow_1_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_1_p2_fin <- N_grow_1_p2 %>% filter(primary == max(N_grow_1_p2$primary))

Ninvade_grow_1_p2 <- N_grow_1_p2_fin
colnames(Ninvade_grow_1_p2)[7] <- 'invade'
Ninvade_grow_1_p2$invade[Ninvade_grow_1_p2$invade <= 1000] <- 0
Ninvade_grow_1_p2$invade[Ninvade_grow_1_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_1_p2 <- aggregate(invade ~  param + sim +p + rem , 
                               data = as.data.frame(Ninvade_grow_1_p2), 
                               FUN = sum)

################################################################################
##### grow_1rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p3_s1 <- fread(file_name)
N_grow_1_p3_s1 <- data.frame(N_grow_1_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p3_s1 <- fread(file_name)
D_grow_1_p3_s1 <- data.frame(D_grow_1_p3_s1)

#Initial pop: 0
N_grow_1_p3_s1_1 <- filter(N_grow_1_p3_s1, primary == 1)
N_grow_1_p3_s1_1$primary <- 0

#remove age = 0
N_grow_1_p3_s1_1$age <- N_grow_1_p3_s1_1$age -1
N_grow_1_p3_s1_1 <- filter(N_grow_1_p3_s1_1, age > 0)

D_grow_1_p3_s1$age <- D_grow_1_p3_s1$age -1
D_grow_1_p3_s1 <- filter(D_grow_1_p3_s1, age > 0)

#Combine init with Dafter
N_grow_1_p3_s1 <- rbind(N_grow_1_p3_s1_1, D_grow_1_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_1_p3_s2 <- fread(file_name)
N_grow_1_p3_s2 <- data.frame(N_grow_1_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_1_p3_s2 <- fread(file_name)
D_grow_1_p3_s2 <- data.frame(D_grow_1_p3_s2)

#Initial pop: 0
N_grow_1_p3_s2_1 <- filter(N_grow_1_p3_s2, primary == 1)
N_grow_1_p3_s2_1$primary <- 0

#remove age = 0
N_grow_1_p3_s2_1$age <- N_grow_1_p3_s2_1$age -1
N_grow_1_p3_s2_1 <- filter(N_grow_1_p3_s2_1, age > 0)

D_grow_1_p3_s2$age <- D_grow_1_p3_s2$age -1
D_grow_1_p3_s2 <- filter(D_grow_1_p3_s2, age > 0)

#Combine init with Dafter
N_grow_1_p3_s2 <- rbind(N_grow_1_p3_s2_1, D_grow_1_p3_s2)

N_grow_1_p3_allages <- rbind(N_grow_1_p3_s1, N_grow_1_p3_s2)

#Sum ages 
N_grow_1_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                         data = as.data.frame(N_grow_1_p3_allages), 
                         FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_1_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                              data = as.data.frame(N_grow_1_p3), 
                              FUN = mean)

###### Average final N @ sites #####
N_grow_1_p3_segfin <- N_grow_1_p3_mean %>% filter(primary == max(N_grow_1_p3_mean$primary))

#sum segments
N_grow_1_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                              data = as.data.frame(N_grow_1_p3), 
                              FUN = sum)
###### Final total N #####
N_grow_1_p3_Nfin <- N_grow_1_p3_Nsum %>% filter(primary == max(N_grow_1_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_1_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                   data = as.data.frame(N_grow_1_p3_Nsum), 
                                   FUN = mean)

N_grow_1_p3_CIl <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p3_Nsum), 
                             function(x) quantile(x, probs = 0.1))

colnames(N_grow_1_p3_CIl)[4] <- 'low.1'

N_grow_1_p3_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_1_p3_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_grow_1_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_1_p3_Nvtime <- cbind(N_grow_1_p3_Nsum_mean,
                          low.1 = N_grow_1_p3_CIl$low.1,
                          high.9 = N_grow_1_p3_CIh$high.9)

# ggplot(grow_1_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p3_s1 <- fread(file_name)
Dcol_grow_1_p3_s1 <- data.frame(Dcol_grow_1_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_1_p3_s2 <- fread(file_name)
Dcol_grow_1_p3_s2 <- data.frame(Dcol_grow_1_p3_s2)

Dcol_grow_1_p3 <- rbind(Dcol_grow_1_p3_s1, Dcol_grow_1_p3_s2)

###### D Columbia ######
Dcol_grow_1_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                data = as.data.frame(Dcol_grow_1_p3), 
                                FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p3_s1 <- fread(file_name)
Dtrav_grow_1_p3_s1 <- data.frame(Dtrav_grow_1_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_1rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_1_p3_s2 <- fread(file_name)
Dtrav_grow_1_p3_s2 <- data.frame(Dtrav_grow_1_p3_s2)

Dtrav_grow_1_p3 <- rbind(Dtrav_grow_1_p3_s1, Dtrav_grow_1_p3_s2)

###### Distance traveled ######
Dtrav_grow_1_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_grow_1_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_1_p3_fin <- N_grow_1_p3 %>% filter(primary == max(N_grow_1_p3$primary))

Ninvade_grow_1_p3 <- N_grow_1_p3_fin
colnames(Ninvade_grow_1_p3)[7] <- 'invade'
Ninvade_grow_1_p3$invade[Ninvade_grow_1_p3$invade <= 1000] <- 0
Ninvade_grow_1_p3$invade[Ninvade_grow_1_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_1_p3 <- aggregate(invade ~  param + sim +p + rem , 
                               data = as.data.frame(Ninvade_grow_1_p3), 
                               FUN = sum)

################################################################################

##### grow_4rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p1_s1 <- fread(file_name)
N_grow_4_p1_s1 <- data.frame(N_grow_4_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p1_s1 <- fread(file_name)
D_grow_4_p1_s1 <- data.frame(D_grow_4_p1_s1)

#Initial pop: 0
N_grow_4_p1_s1_1 <- filter(N_grow_4_p1_s1, primary == 1)
N_grow_4_p1_s1_1$primary <- 0

#remove age = 0
N_grow_4_p1_s1_1$age <- N_grow_4_p1_s1_1$age -1
N_grow_4_p1_s1_1 <- filter(N_grow_4_p1_s1_1, age > 0)

D_grow_4_p1_s1$age <- D_grow_4_p1_s1$age -1
D_grow_4_p1_s1 <- filter(D_grow_4_p1_s1, age > 0)

#Combine init with Dafter
N_grow_4_p1_s1 <- rbind(N_grow_4_p1_s1_1, D_grow_4_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p1_s2 <- fread(file_name)
N_grow_4_p1_s2 <- data.frame(N_grow_4_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p1_s2 <- fread(file_name)
D_grow_4_p1_s2 <- data.frame(D_grow_4_p1_s2)

#Initial pop: 0
N_grow_4_p1_s2_1 <- filter(N_grow_4_p1_s2, primary == 1)
N_grow_4_p1_s2_1$primary <- 0

#remove age = 0
N_grow_4_p1_s2_1$age <- N_grow_4_p1_s2_1$age -1
N_grow_4_p1_s2_1 <- filter(N_grow_4_p1_s2_1, age > 0)

D_grow_4_p1_s2$age <- D_grow_4_p1_s2$age -1
D_grow_4_p1_s2 <- filter(D_grow_4_p1_s2, age > 0)

#Combine init with Dafter
N_grow_4_p1_s2 <- rbind(N_grow_4_p1_s2_1, D_grow_4_p1_s2)

N_grow_4_p1_allages <- rbind(N_grow_4_p1_s1, N_grow_4_p1_s2)

#Sum ages 
N_grow_4_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
          data = as.data.frame(N_grow_4_p1_allages), 
          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_4_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_4_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_4_p1_segfin <- N_grow_4_p1_mean %>% filter(primary == max(N_grow_4_p1_mean$primary))

#sum segments
N_grow_4_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_4_p1), 
                               FUN = sum)
###### Final total N #####
N_grow_4_p1_Nfin <- N_grow_4_p1_Nsum %>% filter(primary == max(N_grow_4_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_4_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_grow_4_p1_Nsum), 
                               FUN = mean)

N_grow_4_p1_CIl <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_grow_4_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_4_p1_CIl)[4] <- 'low.1'

N_grow_4_p1_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_grow_4_p1_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_grow_4_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_4_p1_Nvtime <- cbind(N_grow_4_p1_Nsum_mean,
                              low.1 = N_grow_4_p1_CIl$low.1,
                              high.9 = N_grow_4_p1_CIh$high.9)

# ggplot(grow_4_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p1_s1 <- fread(file_name)
Dcol_grow_4_p1_s1 <- data.frame(Dcol_grow_4_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p1_s2 <- fread(file_name)
Dcol_grow_4_p1_s2 <- data.frame(Dcol_grow_4_p1_s2)

Dcol_grow_4_p1 <- rbind(Dcol_grow_4_p1_s1, Dcol_grow_4_p1_s2)

###### D Columbia ######
Dcol_grow_4_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                    data = as.data.frame(Dcol_grow_4_p1), 
                                    FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p1_s1 <- fread(file_name)
Dtrav_grow_4_p1_s1 <- data.frame(Dtrav_grow_4_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p1_s2 <- fread(file_name)
Dtrav_grow_4_p1_s2 <- data.frame(Dtrav_grow_4_p1_s2)

Dtrav_grow_4_p1 <- rbind(Dtrav_grow_4_p1_s1, Dtrav_grow_4_p1_s2)

###### Distance traveled ######
Dtrav_grow_4_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_grow_4_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_4_p1_fin <- N_grow_4_p1 %>% filter(primary == max(N_grow_4_p1$primary))

Ninvade_grow_4_p1 <- N_grow_4_p1_fin
colnames(Ninvade_grow_4_p1)[7] <- 'invade'
Ninvade_grow_4_p1$invade[Ninvade_grow_4_p1$invade <= 1000] <- 0
Ninvade_grow_4_p1$invade[Ninvade_grow_4_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_4_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                  data = as.data.frame(Ninvade_grow_4_p1), 
                                  FUN = sum)


################################################################################
##### grow_4rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p2_s1 <- fread(file_name)
N_grow_4_p2_s1 <- data.frame(N_grow_4_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p2_s1 <- fread(file_name)
D_grow_4_p2_s1 <- data.frame(D_grow_4_p2_s1)

#Initial pop: 0
N_grow_4_p2_s1_1 <- filter(N_grow_4_p2_s1, primary == 1)
N_grow_4_p2_s1_1$primary <- 0

#remove age = 0
N_grow_4_p2_s1_1$age <- N_grow_4_p2_s1_1$age -1
N_grow_4_p2_s1_1 <- filter(N_grow_4_p2_s1_1, age > 0)

D_grow_4_p2_s1$age <- D_grow_4_p2_s1$age -1
D_grow_4_p2_s1 <- filter(D_grow_4_p2_s1, age > 0)

#Combine init with Dafter
N_grow_4_p2_s1 <- rbind(N_grow_4_p2_s1_1, D_grow_4_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p2_s2 <- fread(file_name)
N_grow_4_p2_s2 <- data.frame(N_grow_4_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p2_s2 <- fread(file_name)
D_grow_4_p2_s2 <- data.frame(D_grow_4_p2_s2)

#Initial pop: 0
N_grow_4_p2_s2_1 <- filter(N_grow_4_p2_s2, primary == 1)
N_grow_4_p2_s2_1$primary <- 0

#remove age = 0
N_grow_4_p2_s2_1$age <- N_grow_4_p2_s2_1$age -1
N_grow_4_p2_s2_1 <- filter(N_grow_4_p2_s2_1, age > 0)

D_grow_4_p2_s2$age <- D_grow_4_p2_s2$age -1
D_grow_4_p2_s2 <- filter(D_grow_4_p2_s2, age > 0)

#Combine init with Dafter
N_grow_4_p2_s2 <- rbind(N_grow_4_p2_s2_1, D_grow_4_p2_s2)

N_grow_4_p2_allages <- rbind(N_grow_4_p2_s1, N_grow_4_p2_s2)

#Sum ages 
N_grow_4_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_4_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_4_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_4_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_4_p2_segfin <- N_grow_4_p2_mean %>% filter(primary == max(N_grow_4_p2_mean$primary))

#sum segments
N_grow_4_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_4_p2), 
                               FUN = sum)
###### Final total N #####
N_grow_4_p2_Nfin <- N_grow_4_p2_Nsum %>% filter(primary == max(N_grow_4_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_4_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_4_p2_Nsum), 
                                    FUN = mean)

N_grow_4_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_4_p2_CIl)[4] <- 'low.1'

N_grow_4_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_4_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_4_p2_Nvtime <- cbind(N_grow_4_p2_Nsum_mean,
                           low.1 = N_grow_4_p2_CIl$low.1,
                           high.9 = N_grow_4_p2_CIh$high.9)

# ggplot(grow_4_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p2_s1 <- fread(file_name)
Dcol_grow_4_p2_s1 <- data.frame(Dcol_grow_4_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p2_s2 <- fread(file_name)
Dcol_grow_4_p2_s2 <- data.frame(Dcol_grow_4_p2_s2)

Dcol_grow_4_p2 <- rbind(Dcol_grow_4_p2_s1, Dcol_grow_4_p2_s2)

###### D Columbia ######
Dcol_grow_4_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_4_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p2_s1 <- fread(file_name)
Dtrav_grow_4_p2_s1 <- data.frame(Dtrav_grow_4_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p2_s2 <- fread(file_name)
Dtrav_grow_4_p2_s2 <- data.frame(Dtrav_grow_4_p2_s2)

Dtrav_grow_4_p2 <- rbind(Dtrav_grow_4_p2_s1, Dtrav_grow_4_p2_s2)

###### Distance traveled ######
Dtrav_grow_4_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_4_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_4_p2_fin <- N_grow_4_p2 %>% filter(primary == max(N_grow_4_p2$primary))

Ninvade_grow_4_p2 <- N_grow_4_p2_fin
colnames(Ninvade_grow_4_p2)[7] <- 'invade'
Ninvade_grow_4_p2$invade[Ninvade_grow_4_p2$invade <= 1000] <- 0
Ninvade_grow_4_p2$invade[Ninvade_grow_4_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_4_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_4_p2), 
                                FUN = sum)

################################################################################
##### grow_4rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p3_s1 <- fread(file_name)
N_grow_4_p3_s1 <- data.frame(N_grow_4_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p3_s1 <- fread(file_name)
D_grow_4_p3_s1 <- data.frame(D_grow_4_p3_s1)

#Initial pop: 0
N_grow_4_p3_s1_1 <- filter(N_grow_4_p3_s1, primary == 1)
N_grow_4_p3_s1_1$primary <- 0

#remove age = 0
N_grow_4_p3_s1_1$age <- N_grow_4_p3_s1_1$age -1
N_grow_4_p3_s1_1 <- filter(N_grow_4_p3_s1_1, age > 0)

D_grow_4_p3_s1$age <- D_grow_4_p3_s1$age -1
D_grow_4_p3_s1 <- filter(D_grow_4_p3_s1, age > 0)

#Combine init with Dafter
N_grow_4_p3_s1 <- rbind(N_grow_4_p3_s1_1, D_grow_4_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_4_p3_s2 <- fread(file_name)
N_grow_4_p3_s2 <- data.frame(N_grow_4_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_4_p3_s2 <- fread(file_name)
D_grow_4_p3_s2 <- data.frame(D_grow_4_p3_s2)

#Initial pop: 0
N_grow_4_p3_s2_1 <- filter(N_grow_4_p3_s2, primary == 1)
N_grow_4_p3_s2_1$primary <- 0

#remove age = 0
N_grow_4_p3_s2_1$age <- N_grow_4_p3_s2_1$age -1
N_grow_4_p3_s2_1 <- filter(N_grow_4_p3_s2_1, age > 0)

D_grow_4_p3_s2$age <- D_grow_4_p3_s2$age -1
D_grow_4_p3_s2 <- filter(D_grow_4_p3_s2, age > 0)

#Combine init with Dafter
N_grow_4_p3_s2 <- rbind(N_grow_4_p3_s2_1, D_grow_4_p3_s2)

N_grow_4_p3_allages <- rbind(N_grow_4_p3_s1, N_grow_4_p3_s2)

#Sum ages 
N_grow_4_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_4_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_4_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_4_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_4_p3_segfin <- N_grow_4_p3_mean %>% filter(primary == max(N_grow_4_p3_mean$primary))

#sum segments
N_grow_4_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_4_p3), 
                               FUN = sum)
###### Final total N #####
N_grow_4_p3_Nfin <- N_grow_4_p3_Nsum %>% filter(primary == max(N_grow_4_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_4_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_4_p3_Nsum), 
                                    FUN = mean)

N_grow_4_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_4_p3_CIl)[4] <- 'low.1'

N_grow_4_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_4_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_4_p3_Nvtime <- cbind(N_grow_4_p3_Nsum_mean,
                           low.1 = N_grow_4_p3_CIl$low.1,
                           high.9 = N_grow_4_p3_CIh$high.9)

# ggplot(grow_4_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p3_s1 <- fread(file_name)
Dcol_grow_4_p3_s1 <- data.frame(Dcol_grow_4_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_4_p3_s2 <- fread(file_name)
Dcol_grow_4_p3_s2 <- data.frame(Dcol_grow_4_p3_s2)

Dcol_grow_4_p3 <- rbind(Dcol_grow_4_p3_s1, Dcol_grow_4_p3_s2)

###### D Columbia ######
Dcol_grow_4_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_4_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p3_s1 <- fread(file_name)
Dtrav_grow_4_p3_s1 <- data.frame(Dtrav_grow_4_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_4rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_4_p3_s2 <- fread(file_name)
Dtrav_grow_4_p3_s2 <- data.frame(Dtrav_grow_4_p3_s2)

Dtrav_grow_4_p3 <- rbind(Dtrav_grow_4_p3_s1, Dtrav_grow_4_p3_s2)

###### Distance traveled ######
Dtrav_grow_4_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_4_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_4_p3_fin <- N_grow_4_p3 %>% filter(primary == max(N_grow_4_p3$primary))

Ninvade_grow_4_p3 <- N_grow_4_p3_fin
colnames(Ninvade_grow_4_p3)[7] <- 'invade'
Ninvade_grow_4_p3$invade[Ninvade_grow_4_p3$invade <= 1000] <- 0
Ninvade_grow_4_p3$invade[Ninvade_grow_4_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_4_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_4_p3), 
                                FUN = sum)

################################################################################
##### grow_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p1_s1 <- fread(file_name)
N_grow_8_p1_s1 <- data.frame(N_grow_8_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p1_s1 <- fread(file_name)
D_grow_8_p1_s1 <- data.frame(D_grow_8_p1_s1)

#Initial pop: 0
N_grow_8_p1_s1_1 <- filter(N_grow_8_p1_s1, primary == 1)
N_grow_8_p1_s1_1$primary <- 0

#remove age = 0
N_grow_8_p1_s1_1$age <- N_grow_8_p1_s1_1$age -1
N_grow_8_p1_s1_1 <- filter(N_grow_8_p1_s1_1, age > 0)

D_grow_8_p1_s1$age <- D_grow_8_p1_s1$age -1
D_grow_8_p1_s1 <- filter(D_grow_8_p1_s1, age > 0)

#Combine init with Dafter
N_grow_8_p1_s1 <- rbind(N_grow_8_p1_s1_1, D_grow_8_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p1_s2 <- fread(file_name)
N_grow_8_p1_s2 <- data.frame(N_grow_8_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p1_s2 <- fread(file_name)
D_grow_8_p1_s2 <- data.frame(D_grow_8_p1_s2)

#Initial pop: 0
N_grow_8_p1_s2_1 <- filter(N_grow_8_p1_s2, primary == 1)
N_grow_8_p1_s2_1$primary <- 0

#remove age = 0
N_grow_8_p1_s2_1$age <- N_grow_8_p1_s2_1$age -1
N_grow_8_p1_s2_1 <- filter(N_grow_8_p1_s2_1, age > 0)

D_grow_8_p1_s2$age <- D_grow_8_p1_s2$age -1
D_grow_8_p1_s2 <- filter(D_grow_8_p1_s2, age > 0)

#Combine init with Dafter
N_grow_8_p1_s2 <- rbind(N_grow_8_p1_s2_1, D_grow_8_p1_s2)

N_grow_8_p1_allages <- rbind(N_grow_8_p1_s1, N_grow_8_p1_s2)

#Sum ages 
N_grow_8_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_8_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_8_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_8_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_8_p1_segfin <- N_grow_8_p1_mean %>% filter(primary == max(N_grow_8_p1_mean$primary))

#sum segments
N_grow_8_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_8_p1), 
                               FUN = sum)
###### Final total N #####
N_grow_8_p1_Nfin <- N_grow_8_p1_Nsum %>% filter(primary == max(N_grow_8_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_8_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_8_p1_Nsum), 
                                    FUN = mean)

N_grow_8_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_8_p1_CIl)[4] <- 'low.1'

N_grow_8_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_8_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_8_p1_Nvtime <- cbind(N_grow_8_p1_Nsum_mean,
                           low.1 = N_grow_8_p1_CIl$low.1,
                           high.9 = N_grow_8_p1_CIh$high.9)

# ggplot(grow_8_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p1_s1 <- fread(file_name)
Dcol_grow_8_p1_s1 <- data.frame(Dcol_grow_8_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p1_s2 <- fread(file_name)
Dcol_grow_8_p1_s2 <- data.frame(Dcol_grow_8_p1_s2)

Dcol_grow_8_p1 <- rbind(Dcol_grow_8_p1_s1, Dcol_grow_8_p1_s2)

###### D Columbia ######
Dcol_grow_8_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_8_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p1_s1 <- fread(file_name)
Dtrav_grow_8_p1_s1 <- data.frame(Dtrav_grow_8_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p1_s2 <- fread(file_name)
Dtrav_grow_8_p1_s2 <- data.frame(Dtrav_grow_8_p1_s2)

Dtrav_grow_8_p1 <- rbind(Dtrav_grow_8_p1_s1, Dtrav_grow_8_p1_s2)

###### Distance traveled ######
Dtrav_grow_8_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_8_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_8_p1_fin <- N_grow_8_p1 %>% filter(primary == max(N_grow_8_p1$primary))

Ninvade_grow_8_p1 <- N_grow_8_p1_fin
colnames(Ninvade_grow_8_p1)[7] <- 'invade'
Ninvade_grow_8_p1$invade[Ninvade_grow_8_p1$invade <= 1000] <- 0
Ninvade_grow_8_p1$invade[Ninvade_grow_8_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_8_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_8_p1), 
                                FUN = sum)


################################################################################
##### grow_8rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p2_s1 <- fread(file_name)
N_grow_8_p2_s1 <- data.frame(N_grow_8_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p2_s1 <- fread(file_name)
D_grow_8_p2_s1 <- data.frame(D_grow_8_p2_s1)

#Initial pop: 0
N_grow_8_p2_s1_1 <- filter(N_grow_8_p2_s1, primary == 1)
N_grow_8_p2_s1_1$primary <- 0

#remove age = 0
N_grow_8_p2_s1_1$age <- N_grow_8_p2_s1_1$age -1
N_grow_8_p2_s1_1 <- filter(N_grow_8_p2_s1_1, age > 0)

D_grow_8_p2_s1$age <- D_grow_8_p2_s1$age -1
D_grow_8_p2_s1 <- filter(D_grow_8_p2_s1, age > 0)

#Combine init with Dafter
N_grow_8_p2_s1 <- rbind(N_grow_8_p2_s1_1, D_grow_8_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p2_s2 <- fread(file_name)
N_grow_8_p2_s2 <- data.frame(N_grow_8_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p2_s2 <- fread(file_name)
D_grow_8_p2_s2 <- data.frame(D_grow_8_p2_s2)

#Initial pop: 0
N_grow_8_p2_s2_1 <- filter(N_grow_8_p2_s2, primary == 1)
N_grow_8_p2_s2_1$primary <- 0

#remove age = 0
N_grow_8_p2_s2_1$age <- N_grow_8_p2_s2_1$age -1
N_grow_8_p2_s2_1 <- filter(N_grow_8_p2_s2_1, age > 0)

D_grow_8_p2_s2$age <- D_grow_8_p2_s2$age -1
D_grow_8_p2_s2 <- filter(D_grow_8_p2_s2, age > 0)

#Combine init with Dafter
N_grow_8_p2_s2 <- rbind(N_grow_8_p2_s2_1, D_grow_8_p2_s2)

N_grow_8_p2_allages <- rbind(N_grow_8_p2_s1, N_grow_8_p2_s2)

#Sum ages 
N_grow_8_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_8_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_8_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_8_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_8_p2_segfin <- N_grow_8_p2_mean %>% filter(primary == max(N_grow_8_p2_mean$primary))

#sum segments
N_grow_8_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_8_p2), 
                               FUN = sum)
###### Final total N #####
N_grow_8_p2_Nfin <- N_grow_8_p2_Nsum %>% filter(primary == max(N_grow_8_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_8_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_8_p2_Nsum), 
                                    FUN = mean)

N_grow_8_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_8_p2_CIl)[4] <- 'low.1'

N_grow_8_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_8_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_8_p2_Nvtime <- cbind(N_grow_8_p2_Nsum_mean,
                           low.1 = N_grow_8_p2_CIl$low.1,
                           high.9 = N_grow_8_p2_CIh$high.9)

# ggplot(grow_8_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p2_s1 <- fread(file_name)
Dcol_grow_8_p2_s1 <- data.frame(Dcol_grow_8_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p2_s2 <- fread(file_name)
Dcol_grow_8_p2_s2 <- data.frame(Dcol_grow_8_p2_s2)

Dcol_grow_8_p2 <- rbind(Dcol_grow_8_p2_s1, Dcol_grow_8_p2_s2)

###### D Columbia ######
Dcol_grow_8_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_8_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p2_s1 <- fread(file_name)
Dtrav_grow_8_p2_s1 <- data.frame(Dtrav_grow_8_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p2_s2 <- fread(file_name)
Dtrav_grow_8_p2_s2 <- data.frame(Dtrav_grow_8_p2_s2)

Dtrav_grow_8_p2 <- rbind(Dtrav_grow_8_p2_s1, Dtrav_grow_8_p2_s2)

###### Distance traveled ######
Dtrav_grow_8_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_8_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_8_p2_fin <- N_grow_8_p2 %>% filter(primary == max(N_grow_8_p2$primary))

Ninvade_grow_8_p2 <- N_grow_8_p2_fin
colnames(Ninvade_grow_8_p2)[7] <- 'invade'
Ninvade_grow_8_p2$invade[Ninvade_grow_8_p2$invade <= 1000] <- 0
Ninvade_grow_8_p2$invade[Ninvade_grow_8_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_8_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_8_p2), 
                                FUN = sum)

################################################################################
##### grow_8rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p3_s1 <- fread(file_name)
N_grow_8_p3_s1 <- data.frame(N_grow_8_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p3_s1 <- fread(file_name)
D_grow_8_p3_s1 <- data.frame(D_grow_8_p3_s1)

#Initial pop: 0
N_grow_8_p3_s1_1 <- filter(N_grow_8_p3_s1, primary == 1)
N_grow_8_p3_s1_1$primary <- 0

#remove age = 0
N_grow_8_p3_s1_1$age <- N_grow_8_p3_s1_1$age -1
N_grow_8_p3_s1_1 <- filter(N_grow_8_p3_s1_1, age > 0)

D_grow_8_p3_s1$age <- D_grow_8_p3_s1$age -1
D_grow_8_p3_s1 <- filter(D_grow_8_p3_s1, age > 0)

#Combine init with Dafter
N_grow_8_p3_s1 <- rbind(N_grow_8_p3_s1_1, D_grow_8_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_8_p3_s2 <- fread(file_name)
N_grow_8_p3_s2 <- data.frame(N_grow_8_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_8_p3_s2 <- fread(file_name)
D_grow_8_p3_s2 <- data.frame(D_grow_8_p3_s2)

#Initial pop: 0
N_grow_8_p3_s2_1 <- filter(N_grow_8_p3_s2, primary == 1)
N_grow_8_p3_s2_1$primary <- 0

#remove age = 0
N_grow_8_p3_s2_1$age <- N_grow_8_p3_s2_1$age -1
N_grow_8_p3_s2_1 <- filter(N_grow_8_p3_s2_1, age > 0)

D_grow_8_p3_s2$age <- D_grow_8_p3_s2$age -1
D_grow_8_p3_s2 <- filter(D_grow_8_p3_s2, age > 0)

#Combine init with Dafter
N_grow_8_p3_s2 <- rbind(N_grow_8_p3_s2_1, D_grow_8_p3_s2)

N_grow_8_p3_allages <- rbind(N_grow_8_p3_s1, N_grow_8_p3_s2)

#Sum ages 
N_grow_8_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_8_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_8_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_8_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_8_p3_segfin <- N_grow_8_p3_mean %>% filter(primary == max(N_grow_8_p3_mean$primary))

#sum segments
N_grow_8_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_8_p3), 
                               FUN = sum)
###### Final total N #####
N_grow_8_p3_Nfin <- N_grow_8_p3_Nsum %>% filter(primary == max(N_grow_8_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_8_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_8_p3_Nsum), 
                                    FUN = mean)

N_grow_8_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_8_p3_CIl)[4] <- 'low.1'

N_grow_8_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_8_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_8_p3_Nvtime <- cbind(N_grow_8_p3_Nsum_mean,
                           low.1 = N_grow_8_p3_CIl$low.1,
                           high.9 = N_grow_8_p3_CIh$high.9)

# ggplot(grow_8_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p3_s1 <- fread(file_name)
Dcol_grow_8_p3_s1 <- data.frame(Dcol_grow_8_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_8_p3_s2 <- fread(file_name)
Dcol_grow_8_p3_s2 <- data.frame(Dcol_grow_8_p3_s2)

Dcol_grow_8_p3 <- rbind(Dcol_grow_8_p3_s1, Dcol_grow_8_p3_s2)

###### D Columbia ######
Dcol_grow_8_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_8_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p3_s1 <- fread(file_name)
Dtrav_grow_8_p3_s1 <- data.frame(Dtrav_grow_8_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_8rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_8_p3_s2 <- fread(file_name)
Dtrav_grow_8_p3_s2 <- data.frame(Dtrav_grow_8_p3_s2)

Dtrav_grow_8_p3 <- rbind(Dtrav_grow_8_p3_s1, Dtrav_grow_8_p3_s2)

###### Distance traveled ######
Dtrav_grow_8_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_8_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_8_p3_fin <- N_grow_8_p3 %>% filter(primary == max(N_grow_8_p3$primary))

Ninvade_grow_8_p3 <- N_grow_8_p3_fin
colnames(Ninvade_grow_8_p3)[7] <- 'invade'
Ninvade_grow_8_p3$invade[Ninvade_grow_8_p3$invade <= 1000] <- 0
Ninvade_grow_8_p3$invade[Ninvade_grow_8_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_8_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_8_p3), 
                                FUN = sum)

################################################################################
##### grow_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p1_s1 <- fread(file_name)
N_grow_16_p1_s1 <- data.frame(N_grow_16_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p1_s1 <- fread(file_name)
D_grow_16_p1_s1 <- data.frame(D_grow_16_p1_s1)

#Initial pop: 0
N_grow_16_p1_s1_1 <- filter(N_grow_16_p1_s1, primary == 1)
N_grow_16_p1_s1_1$primary <- 0

#remove age = 0
N_grow_16_p1_s1_1$age <- N_grow_16_p1_s1_1$age -1
N_grow_16_p1_s1_1 <- filter(N_grow_16_p1_s1_1, age > 0)

D_grow_16_p1_s1$age <- D_grow_16_p1_s1$age -1
D_grow_16_p1_s1 <- filter(D_grow_16_p1_s1, age > 0)

#Combine init with Dafter
N_grow_16_p1_s1 <- rbind(N_grow_16_p1_s1_1, D_grow_16_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p1_s2 <- fread(file_name)
N_grow_16_p1_s2 <- data.frame(N_grow_16_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p1_s2 <- fread(file_name)
D_grow_16_p1_s2 <- data.frame(D_grow_16_p1_s2)

#Initial pop: 0
N_grow_16_p1_s2_1 <- filter(N_grow_16_p1_s2, primary == 1)
N_grow_16_p1_s2_1$primary <- 0

#remove age = 0
N_grow_16_p1_s2_1$age <- N_grow_16_p1_s2_1$age -1
N_grow_16_p1_s2_1 <- filter(N_grow_16_p1_s2_1, age > 0)

D_grow_16_p1_s2$age <- D_grow_16_p1_s2$age -1
D_grow_16_p1_s2 <- filter(D_grow_16_p1_s2, age > 0)

#Combine init with Dafter
N_grow_16_p1_s2 <- rbind(N_grow_16_p1_s2_1, D_grow_16_p1_s2)

N_grow_16_p1_allages <- rbind(N_grow_16_p1_s1, N_grow_16_p1_s2)

#Sum ages 
N_grow_16_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_16_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_16_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_16_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_16_p1_segfin <- N_grow_16_p1_mean %>% filter(primary == max(N_grow_16_p1_mean$primary))

#sum segments
N_grow_16_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_16_p1), 
                               FUN = sum)
###### Final total N #####
N_grow_16_p1_Nfin <- N_grow_16_p1_Nsum %>% filter(primary == max(N_grow_16_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_16_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_16_p1_Nsum), 
                                    FUN = mean)

N_grow_16_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_16_p1_CIl)[4] <- 'low.1'

N_grow_16_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_16_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_16_p1_Nvtime <- cbind(N_grow_16_p1_Nsum_mean,
                           low.1 = N_grow_16_p1_CIl$low.1,
                           high.9 = N_grow_16_p1_CIh$high.9)

# ggplot(grow_16_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p1_s1 <- fread(file_name)
Dcol_grow_16_p1_s1 <- data.frame(Dcol_grow_16_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p1_s2 <- fread(file_name)
Dcol_grow_16_p1_s2 <- data.frame(Dcol_grow_16_p1_s2)

Dcol_grow_16_p1 <- rbind(Dcol_grow_16_p1_s1, Dcol_grow_16_p1_s2)

###### D Columbia ######
Dcol_grow_16_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_16_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p1_s1 <- fread(file_name)
Dtrav_grow_16_p1_s1 <- data.frame(Dtrav_grow_16_p1_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p1_s2 <- fread(file_name)
Dtrav_grow_16_p1_s2 <- data.frame(Dtrav_grow_16_p1_s2)

Dtrav_grow_16_p1 <- rbind(Dtrav_grow_16_p1_s1, Dtrav_grow_16_p1_s2)

###### Distance traveled ######
Dtrav_grow_16_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_16_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_16_p1_fin <- N_grow_16_p1 %>% filter(primary == max(N_grow_16_p1$primary))

Ninvade_grow_16_p1 <- N_grow_16_p1_fin
colnames(Ninvade_grow_16_p1)[7] <- 'invade'
Ninvade_grow_16_p1$invade[Ninvade_grow_16_p1$invade <= 1000] <- 0
Ninvade_grow_16_p1$invade[Ninvade_grow_16_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_16_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_16_p1), 
                                FUN = sum)


################################################################################
##### grow_16rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p2_s1 <- fread(file_name)
N_grow_16_p2_s1 <- data.frame(N_grow_16_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p2_s1 <- fread(file_name)
D_grow_16_p2_s1 <- data.frame(D_grow_16_p2_s1)

#Initial pop: 0
N_grow_16_p2_s1_1 <- filter(N_grow_16_p2_s1, primary == 1)
N_grow_16_p2_s1_1$primary <- 0

#remove age = 0
N_grow_16_p2_s1_1$age <- N_grow_16_p2_s1_1$age -1
N_grow_16_p2_s1_1 <- filter(N_grow_16_p2_s1_1, age > 0)

D_grow_16_p2_s1$age <- D_grow_16_p2_s1$age -1
D_grow_16_p2_s1 <- filter(D_grow_16_p2_s1, age > 0)

#Combine init with Dafter
N_grow_16_p2_s1 <- rbind(N_grow_16_p2_s1_1, D_grow_16_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p2_s2 <- fread(file_name)
N_grow_16_p2_s2 <- data.frame(N_grow_16_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p2_s2 <- fread(file_name)
D_grow_16_p2_s2 <- data.frame(D_grow_16_p2_s2)

#Initial pop: 0
N_grow_16_p2_s2_1 <- filter(N_grow_16_p2_s2, primary == 1)
N_grow_16_p2_s2_1$primary <- 0

#remove age = 0
N_grow_16_p2_s2_1$age <- N_grow_16_p2_s2_1$age -1
N_grow_16_p2_s2_1 <- filter(N_grow_16_p2_s2_1, age > 0)

D_grow_16_p2_s2$age <- D_grow_16_p2_s2$age -1
D_grow_16_p2_s2 <- filter(D_grow_16_p2_s2, age > 0)

#Combine init with Dafter
N_grow_16_p2_s2 <- rbind(N_grow_16_p2_s2_1, D_grow_16_p2_s2)

N_grow_16_p2_allages <- rbind(N_grow_16_p2_s1, N_grow_16_p2_s2)

#Sum ages 
N_grow_16_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_16_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_16_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_16_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_16_p2_segfin <- N_grow_16_p2_mean %>% filter(primary == max(N_grow_16_p2_mean$primary))

#sum segments
N_grow_16_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_16_p2), 
                               FUN = sum)
###### Final total N #####
N_grow_16_p2_Nfin <- N_grow_16_p2_Nsum %>% filter(primary == max(N_grow_16_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_16_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_16_p2_Nsum), 
                                    FUN = mean)

N_grow_16_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_16_p2_CIl)[4] <- 'low.1'

N_grow_16_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_16_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_16_p2_Nvtime <- cbind(N_grow_16_p2_Nsum_mean,
                           low.1 = N_grow_16_p2_CIl$low.1,
                           high.9 = N_grow_16_p2_CIh$high.9)

# ggplot(grow_16_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p2_s1 <- fread(file_name)
Dcol_grow_16_p2_s1 <- data.frame(Dcol_grow_16_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p2_s2 <- fread(file_name)
Dcol_grow_16_p2_s2 <- data.frame(Dcol_grow_16_p2_s2)

Dcol_grow_16_p2 <- rbind(Dcol_grow_16_p2_s1, Dcol_grow_16_p2_s2)

###### D Columbia ######
Dcol_grow_16_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_16_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p2_s1 <- fread(file_name)
Dtrav_grow_16_p2_s1 <- data.frame(Dtrav_grow_16_p2_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p2_s2 <- fread(file_name)
Dtrav_grow_16_p2_s2 <- data.frame(Dtrav_grow_16_p2_s2)

Dtrav_grow_16_p2 <- rbind(Dtrav_grow_16_p2_s1, Dtrav_grow_16_p2_s2)

###### Distance traveled ######
Dtrav_grow_16_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_16_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_16_p2_fin <- N_grow_16_p2 %>% filter(primary == max(N_grow_16_p2$primary))

Ninvade_grow_16_p2 <- N_grow_16_p2_fin
colnames(Ninvade_grow_16_p2)[7] <- 'invade'
Ninvade_grow_16_p2$invade[Ninvade_grow_16_p2$invade <= 1000] <- 0
Ninvade_grow_16_p2$invade[Ninvade_grow_16_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_16_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_16_p2), 
                                FUN = sum)

################################################################################
##### grow_16rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p3_s1 <- fread(file_name)
N_grow_16_p3_s1 <- data.frame(N_grow_16_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p3_s1 <- fread(file_name)
D_grow_16_p3_s1 <- data.frame(D_grow_16_p3_s1)

#Initial pop: 0
N_grow_16_p3_s1_1 <- filter(N_grow_16_p3_s1, primary == 1)
N_grow_16_p3_s1_1$primary <- 0

#remove age = 0
N_grow_16_p3_s1_1$age <- N_grow_16_p3_s1_1$age -1
N_grow_16_p3_s1_1 <- filter(N_grow_16_p3_s1_1, age > 0)

D_grow_16_p3_s1$age <- D_grow_16_p3_s1$age -1
D_grow_16_p3_s1 <- filter(D_grow_16_p3_s1, age > 0)

#Combine init with Dafter
N_grow_16_p3_s1 <- rbind(N_grow_16_p3_s1_1, D_grow_16_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_grow_16_p3_s2 <- fread(file_name)
N_grow_16_p3_s2 <- data.frame(N_grow_16_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_grow_16_p3_s2 <- fread(file_name)
D_grow_16_p3_s2 <- data.frame(D_grow_16_p3_s2)

#Initial pop: 0
N_grow_16_p3_s2_1 <- filter(N_grow_16_p3_s2, primary == 1)
N_grow_16_p3_s2_1$primary <- 0

#remove age = 0
N_grow_16_p3_s2_1$age <- N_grow_16_p3_s2_1$age -1
N_grow_16_p3_s2_1 <- filter(N_grow_16_p3_s2_1, age > 0)

D_grow_16_p3_s2$age <- D_grow_16_p3_s2$age -1
D_grow_16_p3_s2 <- filter(D_grow_16_p3_s2, age > 0)

#Combine init with Dafter
N_grow_16_p3_s2 <- rbind(N_grow_16_p3_s2_1, D_grow_16_p3_s2)

N_grow_16_p3_allages <- rbind(N_grow_16_p3_s1, N_grow_16_p3_s2)

#Sum ages 
N_grow_16_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_grow_16_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_grow_16_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_grow_16_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_grow_16_p3_segfin <- N_grow_16_p3_mean %>% filter(primary == max(N_grow_16_p3_mean$primary))

#sum segments
N_grow_16_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_grow_16_p3), 
                               FUN = sum)
###### Final total N #####
N_grow_16_p3_Nfin <- N_grow_16_p3_Nsum %>% filter(primary == max(N_grow_16_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_grow_16_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_grow_16_p3_Nsum), 
                                    FUN = mean)

N_grow_16_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_grow_16_p3_CIl)[4] <- 'low.1'

N_grow_16_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_grow_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_grow_16_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
grow_16_p3_Nvtime <- cbind(N_grow_16_p3_Nsum_mean,
                           low.1 = N_grow_16_p3_CIl$low.1,
                           high.9 = N_grow_16_p3_CIh$high.9)

# ggplot(grow_16_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p3_s1 <- fread(file_name)
Dcol_grow_16_p3_s1 <- data.frame(Dcol_grow_16_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_grow_16_p3_s2 <- fread(file_name)
Dcol_grow_16_p3_s2 <- data.frame(Dcol_grow_16_p3_s2)

Dcol_grow_16_p3 <- rbind(Dcol_grow_16_p3_s1, Dcol_grow_16_p3_s2)

###### D Columbia ######
Dcol_grow_16_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_grow_16_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p3_s1 <- fread(file_name)
Dtrav_grow_16_p3_s1 <- data.frame(Dtrav_grow_16_p3_s1)

path <- 'E:\\Chapter2\\results\\grow\\grow_16rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_grow_16_p3_s2 <- fread(file_name)
Dtrav_grow_16_p3_s2 <- data.frame(Dtrav_grow_16_p3_s2)

Dtrav_grow_16_p3 <- rbind(Dtrav_grow_16_p3_s1, Dtrav_grow_16_p3_s2)

###### Distance traveled ######
Dtrav_grow_16_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_grow_16_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_grow_16_p3_fin <- N_grow_16_p3 %>% filter(primary == max(N_grow_16_p3$primary))

Ninvade_grow_16_p3 <- N_grow_16_p3_fin
colnames(Ninvade_grow_16_p3)[7] <- 'invade'
Ninvade_grow_16_p3$invade[Ninvade_grow_16_p3$invade <= 1000] <- 0
Ninvade_grow_16_p3$invade[Ninvade_grow_16_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_grow_16_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_grow_16_p3), 
                                FUN = sum)

###############################################################################
##### Combined data #####
path <- 'E:\\Chapter2\\results\\grow'

#------------------- final N @ sites -----------------------------------#
grow_segfin_sum  <- rbind(N_grow_1_p1,N_grow_1_p2,N_grow_1_p3,
                          N_grow_4_p1,N_grow_4_p2,N_grow_4_p3,
                           N_grow_8_p1,N_grow_8_p2,N_grow_8_p3,
                           N_grow_16_p1,N_grow_16_p2,N_grow_16_p3)

grow_segfin_sum$location <- 'grow'

file_name = paste(path, 'grow_segfin_summary.csv',sep = '/')
fwrite(grow_segfin_sum,file_name)

#-------------------average final N @ sites -----------------------------------#
grow_segfin  <- rbind(N_grow_1_p1_segfin,N_grow_1_p2_segfin,N_grow_1_p3_segfin,
                      N_grow_4_p1_segfin,N_grow_4_p2_segfin,N_grow_4_p3_segfin,
                       N_grow_8_p1_segfin,N_grow_8_p2_segfin,N_grow_8_p3_segfin,
                       N_grow_16_p1_segfin,N_grow_16_p2_segfin,N_grow_16_p3_segfin)

grow_segfin$location <- 'grow'

file_name = paste(path, 'grow_Nseg.csv',sep = '/')
fwrite(grow_segfin,file_name)

#------------------- final N total -----------------------------------#
grow_Nfin <- rbind(N_grow_1_p1_Nfin,N_grow_1_p2_Nfin,N_grow_1_p3_Nfin,
                   N_grow_4_p1_Nfin,N_grow_4_p2_Nfin,N_grow_4_p3_Nfin,
                    N_grow_8_p1_Nfin,N_grow_8_p2_Nfin,N_grow_8_p3_Nfin,
                    N_grow_16_p1_Nfin,N_grow_16_p2_Nfin,N_grow_16_p3_Nfin)

grow_Nfin$location <- 'grow'
file_name = paste(path, 'grow_Ntotal.csv',sep = '/')
fwrite(grow_Nfin,file_name)

# ggplot(grow_Nfin)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))

#------------------- N vs time -----------------------------------#
grow_Nvtime <- rbind(grow_1_p1_Nvtime,grow_1_p2_Nvtime,grow_1_p3_Nvtime,
                     grow_4_p1_Nvtime,grow_4_p2_Nvtime,grow_4_p3_Nvtime,
                      grow_8_p1_Nvtime,grow_8_p2_Nvtime,grow_8_p3_Nvtime,
                      grow_16_p1_Nvtime,grow_16_p2_Nvtime,grow_16_p3_Nvtime)

grow_Nvtime$location <- 'grow'
file_name = paste(path, 'grow_Nvtime.csv',sep = '/')
fwrite(grow_Nvtime,file_name)

#------------------- D Columbia  -----------------------------------#
grow_Dcol <- rbind(Dcol_grow_4_p1_sum,Dcol_grow_4_p2_sum,Dcol_grow_4_p3_sum,
                    Dcol_grow_8_p1_sum,Dcol_grow_8_p2_sum,Dcol_grow_8_p3_sum,
                    Dcol_grow_16_p1_sum,Dcol_grow_16_p2_sum,Dcol_grow_16_p3_sum)

grow_Dcol$location <- 'grow'
file_name = paste(path, 'grow_Dcol.csv',sep = '/')
fwrite(grow_Dcol,file_name)

# ggplot(grow_Dcol)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))
# 

#------------------- distance traveled  -----------------------------------#
grow_Dtrav <- rbind(Dtrav_grow_1_p1_sum,Dtrav_grow_1_p2_sum,Dtrav_grow_1_p3_sum,
                    Dtrav_grow_4_p1_sum,Dtrav_grow_4_p2_sum,Dtrav_grow_4_p3_sum,
                     Dtrav_grow_8_p1_sum,Dtrav_grow_8_p2_sum,Dtrav_grow_8_p3_sum,
                     Dtrav_grow_16_p1_sum,Dtrav_grow_16_p2_sum,Dtrav_grow_16_p3_sum)

grow_Dtrav$location <- 'grow'
file_name = paste(path, 'grow_Dtrav.csv',sep = '/')
fwrite(grow_Dtrav,file_name)

# ggplot(grow_Dtrav)+
#   geom_boxplot(aes(x = p, y = distance, group = interaction(p,rem), col = rem))

#------------------- segs invaded  -----------------------------------#
grow_Ninvade <- rbind(Ninvade_grow_1_p1,Ninvade_grow_1_p2,Ninvade_grow_1_p3,
                      Ninvade_grow_4_p1,Ninvade_grow_4_p2,Ninvade_grow_4_p3,
                       Ninvade_grow_8_p1,Ninvade_grow_8_p2,Ninvade_grow_8_p3,
                       Ninvade_grow_16_p1,Ninvade_grow_16_p2,Ninvade_grow_16_p3)

grow_Ninvade$location <- 'grow'
file_name = paste(path, 'grow_Ninvade.csv',sep = '/')
fwrite(grow_Ninvade,file_name)

# ggplot(grow_Ninvade)+
#   geom_boxplot(aes(x = p, y = invade, group = interaction(p,rem), col = rem))
# 



