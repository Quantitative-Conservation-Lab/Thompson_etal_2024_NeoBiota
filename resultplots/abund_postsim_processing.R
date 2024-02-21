library(tidyverse)
library(here)
library(plyr)
library(data.table)

#### Abund data  ####
#Might have to change D: to E: depending on the number of external drives used

##### abund_4rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p1_s1 <- fread(file_name)
N_abund_4_p1_s1 <- data.frame(N_abund_4_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p1_s1 <- fread(file_name)
D_abund_4_p1_s1 <- data.frame(D_abund_4_p1_s1)

#Initial pop: 0
N_abund_4_p1_s1_1 <- filter(N_abund_4_p1_s1, primary == 1)
N_abund_4_p1_s1_1$primary <- 0

#remove age = 0
N_abund_4_p1_s1_1$age <- N_abund_4_p1_s1_1$age -1
N_abund_4_p1_s1_1 <- filter(N_abund_4_p1_s1_1, age > 0)

D_abund_4_p1_s1$age <- D_abund_4_p1_s1$age -1
D_abund_4_p1_s1 <- filter(D_abund_4_p1_s1, age > 0)

#Combine init with Dafter
N_abund_4_p1_s1 <- rbind(N_abund_4_p1_s1_1, D_abund_4_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p1_s2 <- fread(file_name)
N_abund_4_p1_s2 <- data.frame(N_abund_4_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p1_s2 <- fread(file_name)
D_abund_4_p1_s2 <- data.frame(D_abund_4_p1_s2)

#Initial pop: 0
N_abund_4_p1_s2_1 <- filter(N_abund_4_p1_s2, primary == 1)
N_abund_4_p1_s2_1$primary <- 0

#remove age = 0
N_abund_4_p1_s2_1$age <- N_abund_4_p1_s2_1$age -1
N_abund_4_p1_s2_1 <- filter(N_abund_4_p1_s2_1, age > 0)

D_abund_4_p1_s2$age <- D_abund_4_p1_s2$age -1
D_abund_4_p1_s2 <- filter(D_abund_4_p1_s2, age > 0)

#Combine init with Dafter
N_abund_4_p1_s2 <- rbind(N_abund_4_p1_s2_1, D_abund_4_p1_s2)

N_abund_4_p1_allages <- rbind(N_abund_4_p1_s1, N_abund_4_p1_s2)

#Sum ages 
N_abund_4_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
          data = as.data.frame(N_abund_4_p1_allages), 
          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_4_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_4_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_4_p1_segfin <- N_abund_4_p1_mean %>% filter(primary == max(N_abund_4_p1_mean$primary))

#sum segments
N_abund_4_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_4_p1), 
                               FUN = sum)
###### Final total N #####
N_abund_4_p1_Nfin <- N_abund_4_p1_Nsum %>% filter(primary == max(N_abund_4_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_4_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_abund_4_p1_Nsum), 
                               FUN = mean)

N_abund_4_p1_CIl <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_abund_4_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_4_p1_CIl)[4] <- 'low.1'

N_abund_4_p1_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_abund_4_p1_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_abund_4_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_4_p1_Nvtime <- cbind(N_abund_4_p1_Nsum_mean,
                              low.1 = N_abund_4_p1_CIl$low.1,
                              high.9 = N_abund_4_p1_CIh$high.9)

# ggplot(abund_4_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p1_s1 <- fread(file_name)
Dcol_abund_4_p1_s1 <- data.frame(Dcol_abund_4_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p1_s2 <- fread(file_name)
Dcol_abund_4_p1_s2 <- data.frame(Dcol_abund_4_p1_s2)

Dcol_abund_4_p1 <- rbind(Dcol_abund_4_p1_s1, Dcol_abund_4_p1_s2)

###### D Columbia ######
Dcol_abund_4_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                    data = as.data.frame(Dcol_abund_4_p1), 
                                    FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p1_s1 <- fread(file_name)
Dtrav_abund_4_p1_s1 <- data.frame(Dtrav_abund_4_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p1_s2 <- fread(file_name)
Dtrav_abund_4_p1_s2 <- data.frame(Dtrav_abund_4_p1_s2)

Dtrav_abund_4_p1 <- rbind(Dtrav_abund_4_p1_s1, Dtrav_abund_4_p1_s2)

###### Distance traveled ######
Dtrav_abund_4_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_abund_4_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_4_p1_fin <- N_abund_4_p1 %>% filter(primary == max(N_abund_4_p1$primary))

Ninvade_abund_4_p1 <- N_abund_4_p1_fin
colnames(Ninvade_abund_4_p1)[7] <- 'invade'
Ninvade_abund_4_p1$invade[Ninvade_abund_4_p1$invade <= 1000] <- 0
Ninvade_abund_4_p1$invade[Ninvade_abund_4_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_4_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                  data = as.data.frame(Ninvade_abund_4_p1), 
                                  FUN = sum)


################################################################################
##### abund_4rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p2_s1 <- fread(file_name)
N_abund_4_p2_s1 <- data.frame(N_abund_4_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p2_s1 <- fread(file_name)
D_abund_4_p2_s1 <- data.frame(D_abund_4_p2_s1)

#Initial pop: 0
N_abund_4_p2_s1_1 <- filter(N_abund_4_p2_s1, primary == 1)
N_abund_4_p2_s1_1$primary <- 0

#remove age = 0
N_abund_4_p2_s1_1$age <- N_abund_4_p2_s1_1$age -1
N_abund_4_p2_s1_1 <- filter(N_abund_4_p2_s1_1, age > 0)

D_abund_4_p2_s1$age <- D_abund_4_p2_s1$age -1
D_abund_4_p2_s1 <- filter(D_abund_4_p2_s1, age > 0)

#Combine init with Dafter
N_abund_4_p2_s1 <- rbind(N_abund_4_p2_s1_1, D_abund_4_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p2_s2 <- fread(file_name)
N_abund_4_p2_s2 <- data.frame(N_abund_4_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p2_s2 <- fread(file_name)
D_abund_4_p2_s2 <- data.frame(D_abund_4_p2_s2)

#Initial pop: 0
N_abund_4_p2_s2_1 <- filter(N_abund_4_p2_s2, primary == 1)
N_abund_4_p2_s2_1$primary <- 0

#remove age = 0
N_abund_4_p2_s2_1$age <- N_abund_4_p2_s2_1$age -1
N_abund_4_p2_s2_1 <- filter(N_abund_4_p2_s2_1, age > 0)

D_abund_4_p2_s2$age <- D_abund_4_p2_s2$age -1
D_abund_4_p2_s2 <- filter(D_abund_4_p2_s2, age > 0)

#Combine init with Dafter
N_abund_4_p2_s2 <- rbind(N_abund_4_p2_s2_1, D_abund_4_p2_s2)

N_abund_4_p2_allages <- rbind(N_abund_4_p2_s1, N_abund_4_p2_s2)

#Sum ages 
N_abund_4_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_4_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_4_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_4_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_4_p2_segfin <- N_abund_4_p2_mean %>% filter(primary == max(N_abund_4_p2_mean$primary))

#sum segments
N_abund_4_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_4_p2), 
                               FUN = sum)
###### Final total N #####
N_abund_4_p2_Nfin <- N_abund_4_p2_Nsum %>% filter(primary == max(N_abund_4_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_4_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_4_p2_Nsum), 
                                    FUN = mean)

N_abund_4_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_4_p2_CIl)[4] <- 'low.1'

N_abund_4_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_4_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_4_p2_Nvtime <- cbind(N_abund_4_p2_Nsum_mean,
                           low.1 = N_abund_4_p2_CIl$low.1,
                           high.9 = N_abund_4_p2_CIh$high.9)

# ggplot(abund_4_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p2_s1 <- fread(file_name)
Dcol_abund_4_p2_s1 <- data.frame(Dcol_abund_4_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p2_s2 <- fread(file_name)
Dcol_abund_4_p2_s2 <- data.frame(Dcol_abund_4_p2_s2)

Dcol_abund_4_p2 <- rbind(Dcol_abund_4_p2_s1, Dcol_abund_4_p2_s2)

###### D Columbia ######
Dcol_abund_4_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_4_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p2_s1 <- fread(file_name)
Dtrav_abund_4_p2_s1 <- data.frame(Dtrav_abund_4_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p2_s2 <- fread(file_name)
Dtrav_abund_4_p2_s2 <- data.frame(Dtrav_abund_4_p2_s2)

Dtrav_abund_4_p2 <- rbind(Dtrav_abund_4_p2_s1, Dtrav_abund_4_p2_s2)

###### Distance traveled ######
Dtrav_abund_4_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_4_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_4_p2_fin <- N_abund_4_p2 %>% filter(primary == max(N_abund_4_p2$primary))

Ninvade_abund_4_p2 <- N_abund_4_p2_fin
colnames(Ninvade_abund_4_p2)[7] <- 'invade'
Ninvade_abund_4_p2$invade[Ninvade_abund_4_p2$invade <= 1000] <- 0
Ninvade_abund_4_p2$invade[Ninvade_abund_4_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_4_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_4_p2), 
                                FUN = sum)

################################################################################
##### abund_4rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p3_s1 <- fread(file_name)
N_abund_4_p3_s1 <- data.frame(N_abund_4_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p3_s1 <- fread(file_name)
D_abund_4_p3_s1 <- data.frame(D_abund_4_p3_s1)

#Initial pop: 0
N_abund_4_p3_s1_1 <- filter(N_abund_4_p3_s1, primary == 1)
N_abund_4_p3_s1_1$primary <- 0

#remove age = 0
N_abund_4_p3_s1_1$age <- N_abund_4_p3_s1_1$age -1
N_abund_4_p3_s1_1 <- filter(N_abund_4_p3_s1_1, age > 0)

D_abund_4_p3_s1$age <- D_abund_4_p3_s1$age -1
D_abund_4_p3_s1 <- filter(D_abund_4_p3_s1, age > 0)

#Combine init with Dafter
N_abund_4_p3_s1 <- rbind(N_abund_4_p3_s1_1, D_abund_4_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_4_p3_s2 <- fread(file_name)
N_abund_4_p3_s2 <- data.frame(N_abund_4_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_4_p3_s2 <- fread(file_name)
D_abund_4_p3_s2 <- data.frame(D_abund_4_p3_s2)

#Initial pop: 0
N_abund_4_p3_s2_1 <- filter(N_abund_4_p3_s2, primary == 1)
N_abund_4_p3_s2_1$primary <- 0

#remove age = 0
N_abund_4_p3_s2_1$age <- N_abund_4_p3_s2_1$age -1
N_abund_4_p3_s2_1 <- filter(N_abund_4_p3_s2_1, age > 0)

D_abund_4_p3_s2$age <- D_abund_4_p3_s2$age -1
D_abund_4_p3_s2 <- filter(D_abund_4_p3_s2, age > 0)

#Combine init with Dafter
N_abund_4_p3_s2 <- rbind(N_abund_4_p3_s2_1, D_abund_4_p3_s2)

N_abund_4_p3_allages <- rbind(N_abund_4_p3_s1, N_abund_4_p3_s2)

#Sum ages 
N_abund_4_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_4_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_4_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_4_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_4_p3_segfin <- N_abund_4_p3_mean %>% filter(primary == max(N_abund_4_p3_mean$primary))

#sum segments
N_abund_4_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_4_p3), 
                               FUN = sum)
###### Final total N #####
N_abund_4_p3_Nfin <- N_abund_4_p3_Nsum %>% filter(primary == max(N_abund_4_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_4_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_4_p3_Nsum), 
                                    FUN = mean)

N_abund_4_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_4_p3_CIl)[4] <- 'low.1'

N_abund_4_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_4_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_4_p3_Nvtime <- cbind(N_abund_4_p3_Nsum_mean,
                           low.1 = N_abund_4_p3_CIl$low.1,
                           high.9 = N_abund_4_p3_CIh$high.9)

# ggplot(abund_4_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p3_s1 <- fread(file_name)
Dcol_abund_4_p3_s1 <- data.frame(Dcol_abund_4_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p3_s2 <- fread(file_name)
Dcol_abund_4_p3_s2 <- data.frame(Dcol_abund_4_p3_s2)

Dcol_abund_4_p3 <- rbind(Dcol_abund_4_p3_s1, Dcol_abund_4_p3_s2)

###### D Columbia ######
Dcol_abund_4_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_4_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p3_s1 <- fread(file_name)
Dtrav_abund_4_p3_s1 <- data.frame(Dtrav_abund_4_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_4rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_4_p3_s2 <- fread(file_name)
Dtrav_abund_4_p3_s2 <- data.frame(Dtrav_abund_4_p3_s2)

Dtrav_abund_4_p3 <- rbind(Dtrav_abund_4_p3_s1, Dtrav_abund_4_p3_s2)

###### Distance traveled ######
Dtrav_abund_4_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_4_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_4_p3_fin <- N_abund_4_p3 %>% filter(primary == max(N_abund_4_p3$primary))

Ninvade_abund_4_p3 <- N_abund_4_p3_fin
colnames(Ninvade_abund_4_p3)[7] <- 'invade'
Ninvade_abund_4_p3$invade[Ninvade_abund_4_p3$invade <= 1000] <- 0
Ninvade_abund_4_p3$invade[Ninvade_abund_4_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_4_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_4_p3), 
                                FUN = sum)

################################################################################
##### abund_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p1_s1 <- fread(file_name)
N_abund_8_p1_s1 <- data.frame(N_abund_8_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p1_s1 <- fread(file_name)
D_abund_8_p1_s1 <- data.frame(D_abund_8_p1_s1)

#Initial pop: 0
N_abund_8_p1_s1_1 <- filter(N_abund_8_p1_s1, primary == 1)
N_abund_8_p1_s1_1$primary <- 0

#remove age = 0
N_abund_8_p1_s1_1$age <- N_abund_8_p1_s1_1$age -1
N_abund_8_p1_s1_1 <- filter(N_abund_8_p1_s1_1, age > 0)

D_abund_8_p1_s1$age <- D_abund_8_p1_s1$age -1
D_abund_8_p1_s1 <- filter(D_abund_8_p1_s1, age > 0)

#Combine init with Dafter
N_abund_8_p1_s1 <- rbind(N_abund_8_p1_s1_1, D_abund_8_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p1_s2 <- fread(file_name)
N_abund_8_p1_s2 <- data.frame(N_abund_8_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p1_s2 <- fread(file_name)
D_abund_8_p1_s2 <- data.frame(D_abund_8_p1_s2)

#Initial pop: 0
N_abund_8_p1_s2_1 <- filter(N_abund_8_p1_s2, primary == 1)
N_abund_8_p1_s2_1$primary <- 0

#remove age = 0
N_abund_8_p1_s2_1$age <- N_abund_8_p1_s2_1$age -1
N_abund_8_p1_s2_1 <- filter(N_abund_8_p1_s2_1, age > 0)

D_abund_8_p1_s2$age <- D_abund_8_p1_s2$age -1
D_abund_8_p1_s2 <- filter(D_abund_8_p1_s2, age > 0)

#Combine init with Dafter
N_abund_8_p1_s2 <- rbind(N_abund_8_p1_s2_1, D_abund_8_p1_s2)

N_abund_8_p1_allages <- rbind(N_abund_8_p1_s1, N_abund_8_p1_s2)

#Sum ages 
N_abund_8_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_8_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_8_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_8_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_8_p1_segfin <- N_abund_8_p1_mean %>% filter(primary == max(N_abund_8_p1_mean$primary))

#sum segments
N_abund_8_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_8_p1), 
                               FUN = sum)
###### Final total N #####
N_abund_8_p1_Nfin <- N_abund_8_p1_Nsum %>% filter(primary == max(N_abund_8_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_8_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_8_p1_Nsum), 
                                    FUN = mean)

N_abund_8_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_8_p1_CIl)[4] <- 'low.1'

N_abund_8_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_8_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_8_p1_Nvtime <- cbind(N_abund_8_p1_Nsum_mean,
                           low.1 = N_abund_8_p1_CIl$low.1,
                           high.9 = N_abund_8_p1_CIh$high.9)

# ggplot(abund_8_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p1_s1 <- fread(file_name)
Dcol_abund_8_p1_s1 <- data.frame(Dcol_abund_8_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p1_s2 <- fread(file_name)
Dcol_abund_8_p1_s2 <- data.frame(Dcol_abund_8_p1_s2)

Dcol_abund_8_p1 <- rbind(Dcol_abund_8_p1_s1, Dcol_abund_8_p1_s2)

###### D Columbia ######
Dcol_abund_8_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_8_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p1_s1 <- fread(file_name)
Dtrav_abund_8_p1_s1 <- data.frame(Dtrav_abund_8_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p1_s2 <- fread(file_name)
Dtrav_abund_8_p1_s2 <- data.frame(Dtrav_abund_8_p1_s2)

Dtrav_abund_8_p1 <- rbind(Dtrav_abund_8_p1_s1, Dtrav_abund_8_p1_s2)

###### Distance traveled ######
Dtrav_abund_8_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_8_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_8_p1_fin <- N_abund_8_p1 %>% filter(primary == max(N_abund_8_p1$primary))

Ninvade_abund_8_p1 <- N_abund_8_p1_fin
colnames(Ninvade_abund_8_p1)[7] <- 'invade'
Ninvade_abund_8_p1$invade[Ninvade_abund_8_p1$invade <= 1000] <- 0
Ninvade_abund_8_p1$invade[Ninvade_abund_8_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_8_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_8_p1), 
                                FUN = sum)


################################################################################
##### abund_8rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p2_s1 <- fread(file_name)
N_abund_8_p2_s1 <- data.frame(N_abund_8_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p2_s1 <- fread(file_name)
D_abund_8_p2_s1 <- data.frame(D_abund_8_p2_s1)

#Initial pop: 0
N_abund_8_p2_s1_1 <- filter(N_abund_8_p2_s1, primary == 1)
N_abund_8_p2_s1_1$primary <- 0

#remove age = 0
N_abund_8_p2_s1_1$age <- N_abund_8_p2_s1_1$age -1
N_abund_8_p2_s1_1 <- filter(N_abund_8_p2_s1_1, age > 0)

D_abund_8_p2_s1$age <- D_abund_8_p2_s1$age -1
D_abund_8_p2_s1 <- filter(D_abund_8_p2_s1, age > 0)

#Combine init with Dafter
N_abund_8_p2_s1 <- rbind(N_abund_8_p2_s1_1, D_abund_8_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p2_s2 <- fread(file_name)
N_abund_8_p2_s2 <- data.frame(N_abund_8_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p2_s2 <- fread(file_name)
D_abund_8_p2_s2 <- data.frame(D_abund_8_p2_s2)

#Initial pop: 0
N_abund_8_p2_s2_1 <- filter(N_abund_8_p2_s2, primary == 1)
N_abund_8_p2_s2_1$primary <- 0

#remove age = 0
N_abund_8_p2_s2_1$age <- N_abund_8_p2_s2_1$age -1
N_abund_8_p2_s2_1 <- filter(N_abund_8_p2_s2_1, age > 0)

D_abund_8_p2_s2$age <- D_abund_8_p2_s2$age -1
D_abund_8_p2_s2 <- filter(D_abund_8_p2_s2, age > 0)

#Combine init with Dafter
N_abund_8_p2_s2 <- rbind(N_abund_8_p2_s2_1, D_abund_8_p2_s2)

N_abund_8_p2_allages <- rbind(N_abund_8_p2_s1, N_abund_8_p2_s2)

#Sum ages 
N_abund_8_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_8_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_8_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_8_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_8_p2_segfin <- N_abund_8_p2_mean %>% filter(primary == max(N_abund_8_p2_mean$primary))

#sum segments
N_abund_8_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_8_p2), 
                               FUN = sum)
###### Final total N #####
N_abund_8_p2_Nfin <- N_abund_8_p2_Nsum %>% filter(primary == max(N_abund_8_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_8_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_8_p2_Nsum), 
                                    FUN = mean)

N_abund_8_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_8_p2_CIl)[4] <- 'low.1'

N_abund_8_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_8_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_8_p2_Nvtime <- cbind(N_abund_8_p2_Nsum_mean,
                           low.1 = N_abund_8_p2_CIl$low.1,
                           high.9 = N_abund_8_p2_CIh$high.9)

# ggplot(abund_8_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p2_s1 <- fread(file_name)
Dcol_abund_8_p2_s1 <- data.frame(Dcol_abund_8_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p2_s2 <- fread(file_name)
Dcol_abund_8_p2_s2 <- data.frame(Dcol_abund_8_p2_s2)

Dcol_abund_8_p2 <- rbind(Dcol_abund_8_p2_s1, Dcol_abund_8_p2_s2)

###### D Columbia ######
Dcol_abund_8_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_8_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p2_s1 <- fread(file_name)
Dtrav_abund_8_p2_s1 <- data.frame(Dtrav_abund_8_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p2_s2 <- fread(file_name)
Dtrav_abund_8_p2_s2 <- data.frame(Dtrav_abund_8_p2_s2)

Dtrav_abund_8_p2 <- rbind(Dtrav_abund_8_p2_s1, Dtrav_abund_8_p2_s2)

###### Distance traveled ######
Dtrav_abund_8_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_8_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_8_p2_fin <- N_abund_8_p2 %>% filter(primary == max(N_abund_8_p2$primary))

Ninvade_abund_8_p2 <- N_abund_8_p2_fin
colnames(Ninvade_abund_8_p2)[7] <- 'invade'
Ninvade_abund_8_p2$invade[Ninvade_abund_8_p2$invade <= 1000] <- 0
Ninvade_abund_8_p2$invade[Ninvade_abund_8_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_8_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_8_p2), 
                                FUN = sum)

################################################################################
##### abund_8rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p3_s1 <- fread(file_name)
N_abund_8_p3_s1 <- data.frame(N_abund_8_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p3_s1 <- fread(file_name)
D_abund_8_p3_s1 <- data.frame(D_abund_8_p3_s1)

#Initial pop: 0
N_abund_8_p3_s1_1 <- filter(N_abund_8_p3_s1, primary == 1)
N_abund_8_p3_s1_1$primary <- 0

#remove age = 0
N_abund_8_p3_s1_1$age <- N_abund_8_p3_s1_1$age -1
N_abund_8_p3_s1_1 <- filter(N_abund_8_p3_s1_1, age > 0)

D_abund_8_p3_s1$age <- D_abund_8_p3_s1$age -1
D_abund_8_p3_s1 <- filter(D_abund_8_p3_s1, age > 0)

#Combine init with Dafter
N_abund_8_p3_s1 <- rbind(N_abund_8_p3_s1_1, D_abund_8_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_8_p3_s2 <- fread(file_name)
N_abund_8_p3_s2 <- data.frame(N_abund_8_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_8_p3_s2 <- fread(file_name)
D_abund_8_p3_s2 <- data.frame(D_abund_8_p3_s2)

#Initial pop: 0
N_abund_8_p3_s2_1 <- filter(N_abund_8_p3_s2, primary == 1)
N_abund_8_p3_s2_1$primary <- 0

#remove age = 0
N_abund_8_p3_s2_1$age <- N_abund_8_p3_s2_1$age -1
N_abund_8_p3_s2_1 <- filter(N_abund_8_p3_s2_1, age > 0)

D_abund_8_p3_s2$age <- D_abund_8_p3_s2$age -1
D_abund_8_p3_s2 <- filter(D_abund_8_p3_s2, age > 0)

#Combine init with Dafter
N_abund_8_p3_s2 <- rbind(N_abund_8_p3_s2_1, D_abund_8_p3_s2)

N_abund_8_p3_allages <- rbind(N_abund_8_p3_s1, N_abund_8_p3_s2)

#Sum ages 
N_abund_8_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_8_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_8_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_8_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_8_p3_segfin <- N_abund_8_p3_mean %>% filter(primary == max(N_abund_8_p3_mean$primary))

#sum segments
N_abund_8_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_8_p3), 
                               FUN = sum)
###### Final total N #####
N_abund_8_p3_Nfin <- N_abund_8_p3_Nsum %>% filter(primary == max(N_abund_8_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_8_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_8_p3_Nsum), 
                                    FUN = mean)

N_abund_8_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_8_p3_CIl)[4] <- 'low.1'

N_abund_8_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_8_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_8_p3_Nvtime <- cbind(N_abund_8_p3_Nsum_mean,
                           low.1 = N_abund_8_p3_CIl$low.1,
                           high.9 = N_abund_8_p3_CIh$high.9)

# ggplot(abund_8_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p3_s1 <- fread(file_name)
Dcol_abund_8_p3_s1 <- data.frame(Dcol_abund_8_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_8_p3_s2 <- fread(file_name)
Dcol_abund_8_p3_s2 <- data.frame(Dcol_abund_8_p3_s2)

Dcol_abund_8_p3 <- rbind(Dcol_abund_8_p3_s1, Dcol_abund_8_p3_s2)

###### D Columbia ######
Dcol_abund_8_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_8_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p3_s1 <- fread(file_name)
Dtrav_abund_8_p3_s1 <- data.frame(Dtrav_abund_8_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_8rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_8_p3_s2 <- fread(file_name)
Dtrav_abund_8_p3_s2 <- data.frame(Dtrav_abund_8_p3_s2)

Dtrav_abund_8_p3 <- rbind(Dtrav_abund_8_p3_s1, Dtrav_abund_8_p3_s2)

###### Distance traveled ######
Dtrav_abund_8_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_8_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_8_p3_fin <- N_abund_8_p3 %>% filter(primary == max(N_abund_8_p3$primary))

Ninvade_abund_8_p3 <- N_abund_8_p3_fin
colnames(Ninvade_abund_8_p3)[7] <- 'invade'
Ninvade_abund_8_p3$invade[Ninvade_abund_8_p3$invade <= 1000] <- 0
Ninvade_abund_8_p3$invade[Ninvade_abund_8_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_8_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_8_p3), 
                                FUN = sum)

################################################################################
##### abund_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p1_s1 <- fread(file_name)
N_abund_16_p1_s1 <- data.frame(N_abund_16_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p1_s1 <- fread(file_name)
D_abund_16_p1_s1 <- data.frame(D_abund_16_p1_s1)

#Initial pop: 0
N_abund_16_p1_s1_1 <- filter(N_abund_16_p1_s1, primary == 1)
N_abund_16_p1_s1_1$primary <- 0

#remove age = 0
N_abund_16_p1_s1_1$age <- N_abund_16_p1_s1_1$age -1
N_abund_16_p1_s1_1 <- filter(N_abund_16_p1_s1_1, age > 0)

D_abund_16_p1_s1$age <- D_abund_16_p1_s1$age -1
D_abund_16_p1_s1 <- filter(D_abund_16_p1_s1, age > 0)

#Combine init with Dafter
N_abund_16_p1_s1 <- rbind(N_abund_16_p1_s1_1, D_abund_16_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p1_s2 <- fread(file_name)
N_abund_16_p1_s2 <- data.frame(N_abund_16_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p1_s2 <- fread(file_name)
D_abund_16_p1_s2 <- data.frame(D_abund_16_p1_s2)

#Initial pop: 0
N_abund_16_p1_s2_1 <- filter(N_abund_16_p1_s2, primary == 1)
N_abund_16_p1_s2_1$primary <- 0

#remove age = 0
N_abund_16_p1_s2_1$age <- N_abund_16_p1_s2_1$age -1
N_abund_16_p1_s2_1 <- filter(N_abund_16_p1_s2_1, age > 0)

D_abund_16_p1_s2$age <- D_abund_16_p1_s2$age -1
D_abund_16_p1_s2 <- filter(D_abund_16_p1_s2, age > 0)

#Combine init with Dafter
N_abund_16_p1_s2 <- rbind(N_abund_16_p1_s2_1, D_abund_16_p1_s2)

N_abund_16_p1_allages <- rbind(N_abund_16_p1_s1, N_abund_16_p1_s2)

#Sum ages 
N_abund_16_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_16_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_16_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_16_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_16_p1_segfin <- N_abund_16_p1_mean %>% filter(primary == max(N_abund_16_p1_mean$primary))

#sum segments
N_abund_16_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_16_p1), 
                               FUN = sum)
###### Final total N #####
N_abund_16_p1_Nfin <- N_abund_16_p1_Nsum %>% filter(primary == max(N_abund_16_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_16_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_16_p1_Nsum), 
                                    FUN = mean)

N_abund_16_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_16_p1_CIl)[4] <- 'low.1'

N_abund_16_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_16_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_16_p1_Nvtime <- cbind(N_abund_16_p1_Nsum_mean,
                           low.1 = N_abund_16_p1_CIl$low.1,
                           high.9 = N_abund_16_p1_CIh$high.9)

# ggplot(abund_16_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p1_s1 <- fread(file_name)
Dcol_abund_16_p1_s1 <- data.frame(Dcol_abund_16_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p1_s2 <- fread(file_name)
Dcol_abund_16_p1_s2 <- data.frame(Dcol_abund_16_p1_s2)

Dcol_abund_16_p1 <- rbind(Dcol_abund_16_p1_s1, Dcol_abund_16_p1_s2)

###### D Columbia ######
Dcol_abund_16_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_16_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p1_s1 <- fread(file_name)
Dtrav_abund_16_p1_s1 <- data.frame(Dtrav_abund_16_p1_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p1_s2 <- fread(file_name)
Dtrav_abund_16_p1_s2 <- data.frame(Dtrav_abund_16_p1_s2)

Dtrav_abund_16_p1 <- rbind(Dtrav_abund_16_p1_s1, Dtrav_abund_16_p1_s2)

###### Distance traveled ######
Dtrav_abund_16_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_16_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_16_p1_fin <- N_abund_16_p1 %>% filter(primary == max(N_abund_16_p1$primary))

Ninvade_abund_16_p1 <- N_abund_16_p1_fin
colnames(Ninvade_abund_16_p1)[7] <- 'invade'
Ninvade_abund_16_p1$invade[Ninvade_abund_16_p1$invade <= 1000] <- 0
Ninvade_abund_16_p1$invade[Ninvade_abund_16_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_16_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_16_p1), 
                                FUN = sum)


################################################################################
##### abund_16rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p2_s1 <- fread(file_name)
N_abund_16_p2_s1 <- data.frame(N_abund_16_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p2_s1 <- fread(file_name)
D_abund_16_p2_s1 <- data.frame(D_abund_16_p2_s1)

#Initial pop: 0
N_abund_16_p2_s1_1 <- filter(N_abund_16_p2_s1, primary == 1)
N_abund_16_p2_s1_1$primary <- 0

#remove age = 0
N_abund_16_p2_s1_1$age <- N_abund_16_p2_s1_1$age -1
N_abund_16_p2_s1_1 <- filter(N_abund_16_p2_s1_1, age > 0)

D_abund_16_p2_s1$age <- D_abund_16_p2_s1$age -1
D_abund_16_p2_s1 <- filter(D_abund_16_p2_s1, age > 0)

#Combine init with Dafter
N_abund_16_p2_s1 <- rbind(N_abund_16_p2_s1_1, D_abund_16_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p2_s2 <- fread(file_name)
N_abund_16_p2_s2 <- data.frame(N_abund_16_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p2_s2 <- fread(file_name)
D_abund_16_p2_s2 <- data.frame(D_abund_16_p2_s2)

#Initial pop: 0
N_abund_16_p2_s2_1 <- filter(N_abund_16_p2_s2, primary == 1)
N_abund_16_p2_s2_1$primary <- 0

#remove age = 0
N_abund_16_p2_s2_1$age <- N_abund_16_p2_s2_1$age -1
N_abund_16_p2_s2_1 <- filter(N_abund_16_p2_s2_1, age > 0)

D_abund_16_p2_s2$age <- D_abund_16_p2_s2$age -1
D_abund_16_p2_s2 <- filter(D_abund_16_p2_s2, age > 0)

#Combine init with Dafter
N_abund_16_p2_s2 <- rbind(N_abund_16_p2_s2_1, D_abund_16_p2_s2)

N_abund_16_p2_allages <- rbind(N_abund_16_p2_s1, N_abund_16_p2_s2)

#Sum ages 
N_abund_16_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_16_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_16_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_16_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_16_p2_segfin <- N_abund_16_p2_mean %>% filter(primary == max(N_abund_16_p2_mean$primary))

#sum segments
N_abund_16_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_16_p2), 
                               FUN = sum)
###### Final total N #####
N_abund_16_p2_Nfin <- N_abund_16_p2_Nsum %>% filter(primary == max(N_abund_16_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_16_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_16_p2_Nsum), 
                                    FUN = mean)

N_abund_16_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_16_p2_CIl)[4] <- 'low.1'

N_abund_16_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_16_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_16_p2_Nvtime <- cbind(N_abund_16_p2_Nsum_mean,
                           low.1 = N_abund_16_p2_CIl$low.1,
                           high.9 = N_abund_16_p2_CIh$high.9)

# ggplot(abund_16_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p2_s1 <- fread(file_name)
Dcol_abund_16_p2_s1 <- data.frame(Dcol_abund_16_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p2_s2 <- fread(file_name)
Dcol_abund_16_p2_s2 <- data.frame(Dcol_abund_16_p2_s2)

Dcol_abund_16_p2 <- rbind(Dcol_abund_16_p2_s1, Dcol_abund_16_p2_s2)

###### D Columbia ######
Dcol_abund_16_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_16_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p2_s1 <- fread(file_name)
Dtrav_abund_16_p2_s1 <- data.frame(Dtrav_abund_16_p2_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p2_s2 <- fread(file_name)
Dtrav_abund_16_p2_s2 <- data.frame(Dtrav_abund_16_p2_s2)

Dtrav_abund_16_p2 <- rbind(Dtrav_abund_16_p2_s1, Dtrav_abund_16_p2_s2)

###### Distance traveled ######
Dtrav_abund_16_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_16_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_16_p2_fin <- N_abund_16_p2 %>% filter(primary == max(N_abund_16_p2$primary))

Ninvade_abund_16_p2 <- N_abund_16_p2_fin
colnames(Ninvade_abund_16_p2)[7] <- 'invade'
Ninvade_abund_16_p2$invade[Ninvade_abund_16_p2$invade <= 1000] <- 0
Ninvade_abund_16_p2$invade[Ninvade_abund_16_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_16_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_16_p2), 
                                FUN = sum)

################################################################################
##### abund_16rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p3_s1 <- fread(file_name)
N_abund_16_p3_s1 <- data.frame(N_abund_16_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p3_s1 <- fread(file_name)
D_abund_16_p3_s1 <- data.frame(D_abund_16_p3_s1)

#Initial pop: 0
N_abund_16_p3_s1_1 <- filter(N_abund_16_p3_s1, primary == 1)
N_abund_16_p3_s1_1$primary <- 0

#remove age = 0
N_abund_16_p3_s1_1$age <- N_abund_16_p3_s1_1$age -1
N_abund_16_p3_s1_1 <- filter(N_abund_16_p3_s1_1, age > 0)

D_abund_16_p3_s1$age <- D_abund_16_p3_s1$age -1
D_abund_16_p3_s1 <- filter(D_abund_16_p3_s1, age > 0)

#Combine init with Dafter
N_abund_16_p3_s1 <- rbind(N_abund_16_p3_s1_1, D_abund_16_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_abund_16_p3_s2 <- fread(file_name)
N_abund_16_p3_s2 <- data.frame(N_abund_16_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_abund_16_p3_s2 <- fread(file_name)
D_abund_16_p3_s2 <- data.frame(D_abund_16_p3_s2)

#Initial pop: 0
N_abund_16_p3_s2_1 <- filter(N_abund_16_p3_s2, primary == 1)
N_abund_16_p3_s2_1$primary <- 0

#remove age = 0
N_abund_16_p3_s2_1$age <- N_abund_16_p3_s2_1$age -1
N_abund_16_p3_s2_1 <- filter(N_abund_16_p3_s2_1, age > 0)

D_abund_16_p3_s2$age <- D_abund_16_p3_s2$age -1
D_abund_16_p3_s2 <- filter(D_abund_16_p3_s2, age > 0)

#Combine init with Dafter
N_abund_16_p3_s2 <- rbind(N_abund_16_p3_s2_1, D_abund_16_p3_s2)

N_abund_16_p3_allages <- rbind(N_abund_16_p3_s1, N_abund_16_p3_s2)

#Sum ages 
N_abund_16_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_abund_16_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_abund_16_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_16_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_abund_16_p3_segfin <- N_abund_16_p3_mean %>% filter(primary == max(N_abund_16_p3_mean$primary))

#sum segments
N_abund_16_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_16_p3), 
                               FUN = sum)
###### Final total N #####
N_abund_16_p3_Nfin <- N_abund_16_p3_Nsum %>% filter(primary == max(N_abund_16_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_abund_16_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_abund_16_p3_Nsum), 
                                    FUN = mean)

N_abund_16_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_abund_16_p3_CIl)[4] <- 'low.1'

N_abund_16_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_abund_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_abund_16_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
abund_16_p3_Nvtime <- cbind(N_abund_16_p3_Nsum_mean,
                           low.1 = N_abund_16_p3_CIl$low.1,
                           high.9 = N_abund_16_p3_CIh$high.9)

# ggplot(abund_16_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p3_s1 <- fread(file_name)
Dcol_abund_16_p3_s1 <- data.frame(Dcol_abund_16_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_16_p3_s2 <- fread(file_name)
Dcol_abund_16_p3_s2 <- data.frame(Dcol_abund_16_p3_s2)

Dcol_abund_16_p3 <- rbind(Dcol_abund_16_p3_s1, Dcol_abund_16_p3_s2)

###### D Columbia ######
Dcol_abund_16_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_abund_16_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p3_s1 <- fread(file_name)
Dtrav_abund_16_p3_s1 <- data.frame(Dtrav_abund_16_p3_s1)

path <- 'D:\\Chapter2\\results\\abund\\abund_16rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_abund_16_p3_s2 <- fread(file_name)
Dtrav_abund_16_p3_s2 <- data.frame(Dtrav_abund_16_p3_s2)

Dtrav_abund_16_p3 <- rbind(Dtrav_abund_16_p3_s1, Dtrav_abund_16_p3_s2)

###### Distance traveled ######
Dtrav_abund_16_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_abund_16_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_abund_16_p3_fin <- N_abund_16_p3 %>% filter(primary == max(N_abund_16_p3$primary))

Ninvade_abund_16_p3 <- N_abund_16_p3_fin
colnames(Ninvade_abund_16_p3)[7] <- 'invade'
Ninvade_abund_16_p3$invade[Ninvade_abund_16_p3$invade <= 1000] <- 0
Ninvade_abund_16_p3$invade[Ninvade_abund_16_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_abund_16_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_abund_16_p3), 
                                FUN = sum)

###############################################################################
##### Combined data #####
path <- 'D:\\Chapter2\\results\\abund'

#-------------------average final N @ sites -----------------------------------#
abund_segfin  <- rbind(N_abund_4_p1_segfin,N_abund_4_p2_segfin,N_abund_4_p3_segfin,
                       N_abund_8_p1_segfin,N_abund_8_p2_segfin,N_abund_8_p3_segfin,
                       N_abund_16_p1_segfin,N_abund_16_p2_segfin,N_abund_16_p3_segfin)

abund_segfin$location <- 'abund'

file_name = paste(path, 'abund_Nseg.csv',sep = '/')
fwrite(abund_segfin,file_name)

#------------------- final N total -----------------------------------#
abund_Nfin <- rbind(N_abund_4_p1_Nfin,N_abund_4_p2_Nfin,N_abund_4_p3_Nfin,
                    N_abund_8_p1_Nfin,N_abund_8_p2_Nfin,N_abund_8_p3_Nfin,
                    N_abund_16_p1_Nfin,N_abund_16_p2_Nfin,N_abund_16_p3_Nfin)

abund_Nfin$location <- 'abund'
file_name = paste(path, 'abund_Ntotal.csv',sep = '/')
fwrite(abund_Nfin,file_name)

# ggplot(abund_Nfin)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))

#------------------- N vs time -----------------------------------#
abund_Nvtime <- rbind(abund_4_p1_Nvtime,abund_4_p2_Nvtime,abund_4_p3_Nvtime,
                      abund_8_p1_Nvtime,abund_8_p2_Nvtime,abund_8_p3_Nvtime,
                      abund_16_p1_Nvtime,abund_16_p2_Nvtime,abund_16_p3_Nvtime)

abund_Nvtime$location <- 'abund'
file_name = paste(path, 'abund_Nvtime.csv',sep = '/')
fwrite(abund_Nvtime,file_name)

#------------------- D Columbia  -----------------------------------#
abund_Dcol <- rbind(Dcol_abund_4_p1_sum,Dcol_abund_4_p2_sum,Dcol_abund_4_p3_sum,
                    Dcol_abund_8_p1_sum,Dcol_abund_8_p2_sum,Dcol_abund_8_p3_sum,
                    Dcol_abund_16_p1_sum,Dcol_abund_16_p2_sum,Dcol_abund_16_p3_sum)

abund_Dcol$location <- 'abund'
file_name = paste(path, 'abund_Dcol.csv',sep = '/')
fwrite(abund_Dcol,file_name)

# ggplot(abund_Dcol)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))
# 

#------------------- distance traveled  -----------------------------------#
abund_Dtrav <- rbind(Dtrav_abund_4_p1_sum,Dtrav_abund_4_p2_sum,Dtrav_abund_4_p3_sum,
                     Dtrav_abund_8_p1_sum,Dtrav_abund_8_p2_sum,Dtrav_abund_8_p3_sum,
                     Dtrav_abund_16_p1_sum,Dtrav_abund_16_p2_sum,Dtrav_abund_16_p3_sum)

abund_Dtrav$location <- 'abund'
file_name = paste(path, 'abund_Dtrav.csv',sep = '/')
fwrite(abund_Dtrav,file_name)

# ggplot(abund_Dtrav)+
#   geom_boxplot(aes(x = p, y = distance, group = interaction(p,rem), col = rem))

#------------------- segs invaded  -----------------------------------#
abund_Ninvade <- rbind(Ninvade_abund_4_p1,Ninvade_abund_4_p2,Ninvade_abund_4_p3,
                       Ninvade_abund_8_p1,Ninvade_abund_8_p2,Ninvade_abund_8_p3,
                       Ninvade_abund_16_p1,Ninvade_abund_16_p2,Ninvade_abund_16_p3)

abund_Ninvade$location <- 'abund'
file_name = paste(path, 'abund_Ninvade.csv',sep = '/')
fwrite(abund_Ninvade,file_name)

# ggplot(abund_Ninvade)+
#   geom_boxplot(aes(x = p, y = invade, group = interaction(p,rem), col = rem))
# 



