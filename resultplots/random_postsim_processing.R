library(tidyverse)
library(here)
library(plyr)
library(data.table)

#### random data  ####
#Might have to change D: to E: depending on the number of external drives used

##### random_4rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p1_s1 <- fread(file_name)
N_random_4_p1_s1 <- data.frame(N_random_4_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p1_s1 <- fread(file_name)
D_random_4_p1_s1 <- data.frame(D_random_4_p1_s1)

#Initial pop: 0
N_random_4_p1_s1_1 <- filter(N_random_4_p1_s1, primary == 1)
N_random_4_p1_s1_1$primary <- 0

#remove age = 0
N_random_4_p1_s1_1$age <- N_random_4_p1_s1_1$age -1
N_random_4_p1_s1_1 <- filter(N_random_4_p1_s1_1, age > 0)

D_random_4_p1_s1$age <- D_random_4_p1_s1$age -1
D_random_4_p1_s1 <- filter(D_random_4_p1_s1, age > 0)

#Combine init with Dafter
N_random_4_p1_s1 <- rbind(N_random_4_p1_s1_1, D_random_4_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p1_s2 <- fread(file_name)
N_random_4_p1_s2 <- data.frame(N_random_4_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p1_s2 <- fread(file_name)
D_random_4_p1_s2 <- data.frame(D_random_4_p1_s2)

#Initial pop: 0
N_random_4_p1_s2_1 <- filter(N_random_4_p1_s2, primary == 1)
N_random_4_p1_s2_1$primary <- 0

#remove age = 0
N_random_4_p1_s2_1$age <- N_random_4_p1_s2_1$age -1
N_random_4_p1_s2_1 <- filter(N_random_4_p1_s2_1, age > 0)

D_random_4_p1_s2$age <- D_random_4_p1_s2$age -1
D_random_4_p1_s2 <- filter(D_random_4_p1_s2, age > 0)

#Combine init with Dafter
N_random_4_p1_s2 <- rbind(N_random_4_p1_s2_1, D_random_4_p1_s2)

N_random_4_p1_allages <- rbind(N_random_4_p1_s1, N_random_4_p1_s2)

#Sum ages 
N_random_4_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
          data = as.data.frame(N_random_4_p1_allages), 
          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_4_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_4_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_4_p1_segfin <- N_random_4_p1_mean %>% filter(primary == max(N_random_4_p1_mean$primary))

#sum segments
N_random_4_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_4_p1), 
                               FUN = sum)
###### Final total N #####
N_random_4_p1_Nfin <- N_random_4_p1_Nsum %>% filter(primary == max(N_random_4_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_4_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_random_4_p1_Nsum), 
                               FUN = mean)

N_random_4_p1_CIl <- aggregate(count ~  primary +p + rem , 
                               data = as.data.frame(N_random_4_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_4_p1_CIl)[4] <- 'low.1'

N_random_4_p1_CIh <- aggregate(count ~  primary +p + rem , 
                             data = as.data.frame(N_random_4_p1_Nsum), 
                             function(x) quantile(x, probs = 0.9))
colnames(N_random_4_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_4_p1_Nvtime <- cbind(N_random_4_p1_Nsum_mean,
                              low.1 = N_random_4_p1_CIl$low.1,
                              high.9 = N_random_4_p1_CIh$high.9)

# ggplot(random_4_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p1_s1 <- fread(file_name)
Dcol_random_4_p1_s1 <- data.frame(Dcol_random_4_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p1_s2 <- fread(file_name)
Dcol_random_4_p1_s2 <- data.frame(Dcol_random_4_p1_s2)

Dcol_random_4_p1 <- rbind(Dcol_random_4_p1_s1, Dcol_random_4_p1_s2)

###### D Columbia ######
Dcol_random_4_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                    data = as.data.frame(Dcol_random_4_p1), 
                                    FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p1_s1 <- fread(file_name)
Dtrav_random_4_p1_s1 <- data.frame(Dtrav_random_4_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p1_s2 <- fread(file_name)
Dtrav_random_4_p1_s2 <- data.frame(Dtrav_random_4_p1_s2)

Dtrav_random_4_p1 <- rbind(Dtrav_random_4_p1_s1, Dtrav_random_4_p1_s2)

###### Distance traveled ######
Dtrav_random_4_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                 data = as.data.frame(Dtrav_random_4_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_random_4_p1_fin <- N_random_4_p1 %>% filter(primary == max(N_random_4_p1$primary))

Ninvade_random_4_p1 <- N_random_4_p1_fin
colnames(Ninvade_random_4_p1)[7] <- 'invade'
Ninvade_random_4_p1$invade[Ninvade_random_4_p1$invade <= 1000] <- 0
Ninvade_random_4_p1$invade[Ninvade_random_4_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_4_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                  data = as.data.frame(Ninvade_random_4_p1), 
                                  FUN = sum)


################################################################################
##### random_4rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p2_s1 <- fread(file_name)
N_random_4_p2_s1 <- data.frame(N_random_4_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p2_s1 <- fread(file_name)
D_random_4_p2_s1 <- data.frame(D_random_4_p2_s1)

#Initial pop: 0
N_random_4_p2_s1_1 <- filter(N_random_4_p2_s1, primary == 1)
N_random_4_p2_s1_1$primary <- 0

#remove age = 0
N_random_4_p2_s1_1$age <- N_random_4_p2_s1_1$age -1
N_random_4_p2_s1_1 <- filter(N_random_4_p2_s1_1, age > 0)

D_random_4_p2_s1$age <- D_random_4_p2_s1$age -1
D_random_4_p2_s1 <- filter(D_random_4_p2_s1, age > 0)

#Combine init with Dafter
N_random_4_p2_s1 <- rbind(N_random_4_p2_s1_1, D_random_4_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p2_s2 <- fread(file_name)
N_random_4_p2_s2 <- data.frame(N_random_4_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p2_s2 <- fread(file_name)
D_random_4_p2_s2 <- data.frame(D_random_4_p2_s2)

#Initial pop: 0
N_random_4_p2_s2_1 <- filter(N_random_4_p2_s2, primary == 1)
N_random_4_p2_s2_1$primary <- 0

#remove age = 0
N_random_4_p2_s2_1$age <- N_random_4_p2_s2_1$age -1
N_random_4_p2_s2_1 <- filter(N_random_4_p2_s2_1, age > 0)

D_random_4_p2_s2$age <- D_random_4_p2_s2$age -1
D_random_4_p2_s2 <- filter(D_random_4_p2_s2, age > 0)

#Combine init with Dafter
N_random_4_p2_s2 <- rbind(N_random_4_p2_s2_1, D_random_4_p2_s2)

N_random_4_p2_allages <- rbind(N_random_4_p2_s1, N_random_4_p2_s2)

#Sum ages 
N_random_4_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_4_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_4_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_4_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_4_p2_segfin <- N_random_4_p2_mean %>% filter(primary == max(N_random_4_p2_mean$primary))

#sum segments
N_random_4_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_4_p2), 
                               FUN = sum)
###### Final total N #####
N_random_4_p2_Nfin <- N_random_4_p2_Nsum %>% filter(primary == max(N_random_4_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_4_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_4_p2_Nsum), 
                                    FUN = mean)

N_random_4_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_4_p2_CIl)[4] <- 'low.1'

N_random_4_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_4_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_4_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_4_p2_Nvtime <- cbind(N_random_4_p2_Nsum_mean,
                           low.1 = N_random_4_p2_CIl$low.1,
                           high.9 = N_random_4_p2_CIh$high.9)

# ggplot(random_4_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p2_s1 <- fread(file_name)
Dcol_random_4_p2_s1 <- data.frame(Dcol_random_4_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p2_s2 <- fread(file_name)
Dcol_random_4_p2_s2 <- data.frame(Dcol_random_4_p2_s2)

Dcol_random_4_p2 <- rbind(Dcol_random_4_p2_s1, Dcol_random_4_p2_s2)

###### D Columbia ######
Dcol_random_4_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_4_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p2_s1 <- fread(file_name)
Dtrav_random_4_p2_s1 <- data.frame(Dtrav_random_4_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p2_s2 <- fread(file_name)
Dtrav_random_4_p2_s2 <- data.frame(Dtrav_random_4_p2_s2)

Dtrav_random_4_p2 <- rbind(Dtrav_random_4_p2_s1, Dtrav_random_4_p2_s2)

###### Distance traveled ######
Dtrav_random_4_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_4_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_4_p2_fin <- N_random_4_p2 %>% filter(primary == max(N_random_4_p2$primary))

Ninvade_random_4_p2 <- N_random_4_p2_fin
colnames(Ninvade_random_4_p2)[7] <- 'invade'
Ninvade_random_4_p2$invade[Ninvade_random_4_p2$invade <= 1000] <- 0
Ninvade_random_4_p2$invade[Ninvade_random_4_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_4_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_4_p2), 
                                FUN = sum)

################################################################################
##### random_4rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p3_s1 <- fread(file_name)
N_random_4_p3_s1 <- data.frame(N_random_4_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p3_s1 <- fread(file_name)
D_random_4_p3_s1 <- data.frame(D_random_4_p3_s1)

#Initial pop: 0
N_random_4_p3_s1_1 <- filter(N_random_4_p3_s1, primary == 1)
N_random_4_p3_s1_1$primary <- 0

#remove age = 0
N_random_4_p3_s1_1$age <- N_random_4_p3_s1_1$age -1
N_random_4_p3_s1_1 <- filter(N_random_4_p3_s1_1, age > 0)

D_random_4_p3_s1$age <- D_random_4_p3_s1$age -1
D_random_4_p3_s1 <- filter(D_random_4_p3_s1, age > 0)

#Combine init with Dafter
N_random_4_p3_s1 <- rbind(N_random_4_p3_s1_1, D_random_4_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_4_p3_s2 <- fread(file_name)
N_random_4_p3_s2 <- data.frame(N_random_4_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_4_p3_s2 <- fread(file_name)
D_random_4_p3_s2 <- data.frame(D_random_4_p3_s2)

#Initial pop: 0
N_random_4_p3_s2_1 <- filter(N_random_4_p3_s2, primary == 1)
N_random_4_p3_s2_1$primary <- 0

#remove age = 0
N_random_4_p3_s2_1$age <- N_random_4_p3_s2_1$age -1
N_random_4_p3_s2_1 <- filter(N_random_4_p3_s2_1, age > 0)

D_random_4_p3_s2$age <- D_random_4_p3_s2$age -1
D_random_4_p3_s2 <- filter(D_random_4_p3_s2, age > 0)

#Combine init with Dafter
N_random_4_p3_s2 <- rbind(N_random_4_p3_s2_1, D_random_4_p3_s2)

N_random_4_p3_allages <- rbind(N_random_4_p3_s1, N_random_4_p3_s2)

#Sum ages 
N_random_4_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_4_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_4_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_4_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_4_p3_segfin <- N_random_4_p3_mean %>% filter(primary == max(N_random_4_p3_mean$primary))

#sum segments
N_random_4_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_4_p3), 
                               FUN = sum)
###### Final total N #####
N_random_4_p3_Nfin <- N_random_4_p3_Nsum %>% filter(primary == max(N_random_4_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_4_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_4_p3_Nsum), 
                                    FUN = mean)

N_random_4_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_4_p3_CIl)[4] <- 'low.1'

N_random_4_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_4_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_4_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_4_p3_Nvtime <- cbind(N_random_4_p3_Nsum_mean,
                           low.1 = N_random_4_p3_CIl$low.1,
                           high.9 = N_random_4_p3_CIh$high.9)

# ggplot(random_4_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p3_s1 <- fread(file_name)
Dcol_random_4_p3_s1 <- data.frame(Dcol_random_4_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_4_p3_s2 <- fread(file_name)
Dcol_random_4_p3_s2 <- data.frame(Dcol_random_4_p3_s2)

Dcol_random_4_p3 <- rbind(Dcol_random_4_p3_s1, Dcol_random_4_p3_s2)

###### D Columbia ######
Dcol_random_4_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_4_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p3_s1 <- fread(file_name)
Dtrav_random_4_p3_s1 <- data.frame(Dtrav_random_4_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_4rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_4_p3_s2 <- fread(file_name)
Dtrav_random_4_p3_s2 <- data.frame(Dtrav_random_4_p3_s2)

Dtrav_random_4_p3 <- rbind(Dtrav_random_4_p3_s1, Dtrav_random_4_p3_s2)

###### Distance traveled ######
Dtrav_random_4_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_4_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_4_p3_fin <- N_random_4_p3 %>% filter(primary == max(N_random_4_p3$primary))

Ninvade_random_4_p3 <- N_random_4_p3_fin
colnames(Ninvade_random_4_p3)[7] <- 'invade'
Ninvade_random_4_p3$invade[Ninvade_random_4_p3$invade <= 1000] <- 0
Ninvade_random_4_p3$invade[Ninvade_random_4_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_4_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_4_p3), 
                                FUN = sum)

################################################################################
##### random_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p1_s1 <- fread(file_name)
N_random_8_p1_s1 <- data.frame(N_random_8_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p1_s1 <- fread(file_name)
D_random_8_p1_s1 <- data.frame(D_random_8_p1_s1)

#Initial pop: 0
N_random_8_p1_s1_1 <- filter(N_random_8_p1_s1, primary == 1)
N_random_8_p1_s1_1$primary <- 0

#remove age = 0
N_random_8_p1_s1_1$age <- N_random_8_p1_s1_1$age -1
N_random_8_p1_s1_1 <- filter(N_random_8_p1_s1_1, age > 0)

D_random_8_p1_s1$age <- D_random_8_p1_s1$age -1
D_random_8_p1_s1 <- filter(D_random_8_p1_s1, age > 0)

#Combine init with Dafter
N_random_8_p1_s1 <- rbind(N_random_8_p1_s1_1, D_random_8_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p1_s2 <- fread(file_name)
N_random_8_p1_s2 <- data.frame(N_random_8_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p1_s2 <- fread(file_name)
D_random_8_p1_s2 <- data.frame(D_random_8_p1_s2)

#Initial pop: 0
N_random_8_p1_s2_1 <- filter(N_random_8_p1_s2, primary == 1)
N_random_8_p1_s2_1$primary <- 0

#remove age = 0
N_random_8_p1_s2_1$age <- N_random_8_p1_s2_1$age -1
N_random_8_p1_s2_1 <- filter(N_random_8_p1_s2_1, age > 0)

D_random_8_p1_s2$age <- D_random_8_p1_s2$age -1
D_random_8_p1_s2 <- filter(D_random_8_p1_s2, age > 0)

#Combine init with Dafter
N_random_8_p1_s2 <- rbind(N_random_8_p1_s2_1, D_random_8_p1_s2)

N_random_8_p1_allages <- rbind(N_random_8_p1_s1, N_random_8_p1_s2)

#Sum ages 
N_random_8_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_8_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_8_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_8_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_8_p1_segfin <- N_random_8_p1_mean %>% filter(primary == max(N_random_8_p1_mean$primary))

#sum segments
N_random_8_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_8_p1), 
                               FUN = sum)
###### Final total N #####
N_random_8_p1_Nfin <- N_random_8_p1_Nsum %>% filter(primary == max(N_random_8_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_8_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_8_p1_Nsum), 
                                    FUN = mean)

N_random_8_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_8_p1_CIl)[4] <- 'low.1'

N_random_8_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_8_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_8_p1_Nvtime <- cbind(N_random_8_p1_Nsum_mean,
                           low.1 = N_random_8_p1_CIl$low.1,
                           high.9 = N_random_8_p1_CIh$high.9)

# ggplot(random_8_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p1_s1 <- fread(file_name)
Dcol_random_8_p1_s1 <- data.frame(Dcol_random_8_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p1_s2 <- fread(file_name)
Dcol_random_8_p1_s2 <- data.frame(Dcol_random_8_p1_s2)

Dcol_random_8_p1 <- rbind(Dcol_random_8_p1_s1, Dcol_random_8_p1_s2)

###### D Columbia ######
Dcol_random_8_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_8_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p1_s1 <- fread(file_name)
Dtrav_random_8_p1_s1 <- data.frame(Dtrav_random_8_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p1_s2 <- fread(file_name)
Dtrav_random_8_p1_s2 <- data.frame(Dtrav_random_8_p1_s2)

Dtrav_random_8_p1 <- rbind(Dtrav_random_8_p1_s1, Dtrav_random_8_p1_s2)

###### Distance traveled ######
Dtrav_random_8_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_8_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_8_p1_fin <- N_random_8_p1 %>% filter(primary == max(N_random_8_p1$primary))

Ninvade_random_8_p1 <- N_random_8_p1_fin
colnames(Ninvade_random_8_p1)[7] <- 'invade'
Ninvade_random_8_p1$invade[Ninvade_random_8_p1$invade <= 1000] <- 0
Ninvade_random_8_p1$invade[Ninvade_random_8_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_8_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_8_p1), 
                                FUN = sum)


################################################################################
##### random_8rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p2_s1 <- fread(file_name)
N_random_8_p2_s1 <- data.frame(N_random_8_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p2_s1 <- fread(file_name)
D_random_8_p2_s1 <- data.frame(D_random_8_p2_s1)

#Initial pop: 0
N_random_8_p2_s1_1 <- filter(N_random_8_p2_s1, primary == 1)
N_random_8_p2_s1_1$primary <- 0

#remove age = 0
N_random_8_p2_s1_1$age <- N_random_8_p2_s1_1$age -1
N_random_8_p2_s1_1 <- filter(N_random_8_p2_s1_1, age > 0)

D_random_8_p2_s1$age <- D_random_8_p2_s1$age -1
D_random_8_p2_s1 <- filter(D_random_8_p2_s1, age > 0)

#Combine init with Dafter
N_random_8_p2_s1 <- rbind(N_random_8_p2_s1_1, D_random_8_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p2_s2 <- fread(file_name)
N_random_8_p2_s2 <- data.frame(N_random_8_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p2_s2 <- fread(file_name)
D_random_8_p2_s2 <- data.frame(D_random_8_p2_s2)

#Initial pop: 0
N_random_8_p2_s2_1 <- filter(N_random_8_p2_s2, primary == 1)
N_random_8_p2_s2_1$primary <- 0

#remove age = 0
N_random_8_p2_s2_1$age <- N_random_8_p2_s2_1$age -1
N_random_8_p2_s2_1 <- filter(N_random_8_p2_s2_1, age > 0)

D_random_8_p2_s2$age <- D_random_8_p2_s2$age -1
D_random_8_p2_s2 <- filter(D_random_8_p2_s2, age > 0)

#Combine init with Dafter
N_random_8_p2_s2 <- rbind(N_random_8_p2_s2_1, D_random_8_p2_s2)

N_random_8_p2_allages <- rbind(N_random_8_p2_s1, N_random_8_p2_s2)

#Sum ages 
N_random_8_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_8_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_8_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_8_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_8_p2_segfin <- N_random_8_p2_mean %>% filter(primary == max(N_random_8_p2_mean$primary))

#sum segments
N_random_8_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_8_p2), 
                               FUN = sum)
###### Final total N #####
N_random_8_p2_Nfin <- N_random_8_p2_Nsum %>% filter(primary == max(N_random_8_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_8_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_8_p2_Nsum), 
                                    FUN = mean)

N_random_8_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_8_p2_CIl)[4] <- 'low.1'

N_random_8_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_8_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_8_p2_Nvtime <- cbind(N_random_8_p2_Nsum_mean,
                           low.1 = N_random_8_p2_CIl$low.1,
                           high.9 = N_random_8_p2_CIh$high.9)

# ggplot(random_8_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p2_s1 <- fread(file_name)
Dcol_random_8_p2_s1 <- data.frame(Dcol_random_8_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p2_s2 <- fread(file_name)
Dcol_random_8_p2_s2 <- data.frame(Dcol_random_8_p2_s2)

Dcol_random_8_p2 <- rbind(Dcol_random_8_p2_s1, Dcol_random_8_p2_s2)

###### D Columbia ######
Dcol_random_8_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_8_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p2_s1 <- fread(file_name)
Dtrav_random_8_p2_s1 <- data.frame(Dtrav_random_8_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p2_s2 <- fread(file_name)
Dtrav_random_8_p2_s2 <- data.frame(Dtrav_random_8_p2_s2)

Dtrav_random_8_p2 <- rbind(Dtrav_random_8_p2_s1, Dtrav_random_8_p2_s2)

###### Distance traveled ######
Dtrav_random_8_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_8_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_8_p2_fin <- N_random_8_p2 %>% filter(primary == max(N_random_8_p2$primary))

Ninvade_random_8_p2 <- N_random_8_p2_fin
colnames(Ninvade_random_8_p2)[7] <- 'invade'
Ninvade_random_8_p2$invade[Ninvade_random_8_p2$invade <= 1000] <- 0
Ninvade_random_8_p2$invade[Ninvade_random_8_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_8_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_8_p2), 
                                FUN = sum)

################################################################################
##### random_8rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p3_s1 <- fread(file_name)
N_random_8_p3_s1 <- data.frame(N_random_8_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p3_s1 <- fread(file_name)
D_random_8_p3_s1 <- data.frame(D_random_8_p3_s1)

#Initial pop: 0
N_random_8_p3_s1_1 <- filter(N_random_8_p3_s1, primary == 1)
N_random_8_p3_s1_1$primary <- 0

#remove age = 0
N_random_8_p3_s1_1$age <- N_random_8_p3_s1_1$age -1
N_random_8_p3_s1_1 <- filter(N_random_8_p3_s1_1, age > 0)

D_random_8_p3_s1$age <- D_random_8_p3_s1$age -1
D_random_8_p3_s1 <- filter(D_random_8_p3_s1, age > 0)

#Combine init with Dafter
N_random_8_p3_s1 <- rbind(N_random_8_p3_s1_1, D_random_8_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_8_p3_s2 <- fread(file_name)
N_random_8_p3_s2 <- data.frame(N_random_8_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_8_p3_s2 <- fread(file_name)
D_random_8_p3_s2 <- data.frame(D_random_8_p3_s2)

#Initial pop: 0
N_random_8_p3_s2_1 <- filter(N_random_8_p3_s2, primary == 1)
N_random_8_p3_s2_1$primary <- 0

#remove age = 0
N_random_8_p3_s2_1$age <- N_random_8_p3_s2_1$age -1
N_random_8_p3_s2_1 <- filter(N_random_8_p3_s2_1, age > 0)

D_random_8_p3_s2$age <- D_random_8_p3_s2$age -1
D_random_8_p3_s2 <- filter(D_random_8_p3_s2, age > 0)

#Combine init with Dafter
N_random_8_p3_s2 <- rbind(N_random_8_p3_s2_1, D_random_8_p3_s2)

N_random_8_p3_allages <- rbind(N_random_8_p3_s1, N_random_8_p3_s2)

#Sum ages 
N_random_8_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_8_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_8_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_8_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_8_p3_segfin <- N_random_8_p3_mean %>% filter(primary == max(N_random_8_p3_mean$primary))

#sum segments
N_random_8_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_8_p3), 
                               FUN = sum)
###### Final total N #####
N_random_8_p3_Nfin <- N_random_8_p3_Nsum %>% filter(primary == max(N_random_8_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_8_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_8_p3_Nsum), 
                                    FUN = mean)

N_random_8_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_8_p3_CIl)[4] <- 'low.1'

N_random_8_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_8_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_8_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_8_p3_Nvtime <- cbind(N_random_8_p3_Nsum_mean,
                           low.1 = N_random_8_p3_CIl$low.1,
                           high.9 = N_random_8_p3_CIh$high.9)

# ggplot(random_8_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p3_s1 <- fread(file_name)
Dcol_random_8_p3_s1 <- data.frame(Dcol_random_8_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_8_p3_s2 <- fread(file_name)
Dcol_random_8_p3_s2 <- data.frame(Dcol_random_8_p3_s2)

Dcol_random_8_p3 <- rbind(Dcol_random_8_p3_s1, Dcol_random_8_p3_s2)

###### D Columbia ######
Dcol_random_8_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_8_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p3_s1 <- fread(file_name)
Dtrav_random_8_p3_s1 <- data.frame(Dtrav_random_8_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_8rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_8_p3_s2 <- fread(file_name)
Dtrav_random_8_p3_s2 <- data.frame(Dtrav_random_8_p3_s2)

Dtrav_random_8_p3 <- rbind(Dtrav_random_8_p3_s1, Dtrav_random_8_p3_s2)

###### Distance traveled ######
Dtrav_random_8_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_8_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_8_p3_fin <- N_random_8_p3 %>% filter(primary == max(N_random_8_p3$primary))

Ninvade_random_8_p3 <- N_random_8_p3_fin
colnames(Ninvade_random_8_p3)[7] <- 'invade'
Ninvade_random_8_p3$invade[Ninvade_random_8_p3$invade <= 1000] <- 0
Ninvade_random_8_p3$invade[Ninvade_random_8_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_8_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_8_p3), 
                                FUN = sum)

################################################################################
##### random_8rem_p1 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p1_s1 <- fread(file_name)
N_random_16_p1_s1 <- data.frame(N_random_16_p1_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p1_s1 <- fread(file_name)
D_random_16_p1_s1 <- data.frame(D_random_16_p1_s1)

#Initial pop: 0
N_random_16_p1_s1_1 <- filter(N_random_16_p1_s1, primary == 1)
N_random_16_p1_s1_1$primary <- 0

#remove age = 0
N_random_16_p1_s1_1$age <- N_random_16_p1_s1_1$age -1
N_random_16_p1_s1_1 <- filter(N_random_16_p1_s1_1, age > 0)

D_random_16_p1_s1$age <- D_random_16_p1_s1$age -1
D_random_16_p1_s1 <- filter(D_random_16_p1_s1, age > 0)

#Combine init with Dafter
N_random_16_p1_s1 <- rbind(N_random_16_p1_s1_1, D_random_16_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p1_s2 <- fread(file_name)
N_random_16_p1_s2 <- data.frame(N_random_16_p1_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p1_s2 <- fread(file_name)
D_random_16_p1_s2 <- data.frame(D_random_16_p1_s2)

#Initial pop: 0
N_random_16_p1_s2_1 <- filter(N_random_16_p1_s2, primary == 1)
N_random_16_p1_s2_1$primary <- 0

#remove age = 0
N_random_16_p1_s2_1$age <- N_random_16_p1_s2_1$age -1
N_random_16_p1_s2_1 <- filter(N_random_16_p1_s2_1, age > 0)

D_random_16_p1_s2$age <- D_random_16_p1_s2$age -1
D_random_16_p1_s2 <- filter(D_random_16_p1_s2, age > 0)

#Combine init with Dafter
N_random_16_p1_s2 <- rbind(N_random_16_p1_s2_1, D_random_16_p1_s2)

N_random_16_p1_allages <- rbind(N_random_16_p1_s1, N_random_16_p1_s2)

#Sum ages 
N_random_16_p1 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_16_p1_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_16_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_16_p1), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_16_p1_segfin <- N_random_16_p1_mean %>% filter(primary == max(N_random_16_p1_mean$primary))

#sum segments
N_random_16_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_16_p1), 
                               FUN = sum)
###### Final total N #####
N_random_16_p1_Nfin <- N_random_16_p1_Nsum %>% filter(primary == max(N_random_16_p1_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_16_p1_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_16_p1_Nsum), 
                                    FUN = mean)

N_random_16_p1_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_16_p1_CIl)[4] <- 'low.1'

N_random_16_p1_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p1_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_16_p1_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_16_p1_Nvtime <- cbind(N_random_16_p1_Nsum_mean,
                           low.1 = N_random_16_p1_CIl$low.1,
                           high.9 = N_random_16_p1_CIh$high.9)

# ggplot(random_16_p1_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p1_s1 <- fread(file_name)
Dcol_random_16_p1_s1 <- data.frame(Dcol_random_16_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p1_s2 <- fread(file_name)
Dcol_random_16_p1_s2 <- data.frame(Dcol_random_16_p1_s2)

Dcol_random_16_p1 <- rbind(Dcol_random_16_p1_s1, Dcol_random_16_p1_s2)

###### D Columbia ######
Dcol_random_16_p1_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_16_p1), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p1_s1 <- fread(file_name)
Dtrav_random_16_p1_s1 <- data.frame(Dtrav_random_16_p1_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p1_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p1_s2 <- fread(file_name)
Dtrav_random_16_p1_s2 <- data.frame(Dtrav_random_16_p1_s2)

Dtrav_random_16_p1 <- rbind(Dtrav_random_16_p1_s1, Dtrav_random_16_p1_s2)

###### Distance traveled ######
Dtrav_random_16_p1_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_16_p1), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_16_p1_fin <- N_random_16_p1 %>% filter(primary == max(N_random_16_p1$primary))

Ninvade_random_16_p1 <- N_random_16_p1_fin
colnames(Ninvade_random_16_p1)[7] <- 'invade'
Ninvade_random_16_p1$invade[Ninvade_random_16_p1$invade <= 1000] <- 0
Ninvade_random_16_p1$invade[Ninvade_random_16_p1$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_16_p1 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_16_p1), 
                                FUN = sum)


################################################################################
##### random_16rem_p2 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p2_s1 <- fread(file_name)
N_random_16_p2_s1 <- data.frame(N_random_16_p2_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p2_s1 <- fread(file_name)
D_random_16_p2_s1 <- data.frame(D_random_16_p2_s1)

#Initial pop: 0
N_random_16_p2_s1_1 <- filter(N_random_16_p2_s1, primary == 1)
N_random_16_p2_s1_1$primary <- 0

#remove age = 0
N_random_16_p2_s1_1$age <- N_random_16_p2_s1_1$age -1
N_random_16_p2_s1_1 <- filter(N_random_16_p2_s1_1, age > 0)

D_random_16_p2_s1$age <- D_random_16_p2_s1$age -1
D_random_16_p2_s1 <- filter(D_random_16_p2_s1, age > 0)

#Combine init with Dafter
N_random_16_p2_s1 <- rbind(N_random_16_p2_s1_1, D_random_16_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p2_s2 <- fread(file_name)
N_random_16_p2_s2 <- data.frame(N_random_16_p2_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p2_s2 <- fread(file_name)
D_random_16_p2_s2 <- data.frame(D_random_16_p2_s2)

#Initial pop: 0
N_random_16_p2_s2_1 <- filter(N_random_16_p2_s2, primary == 1)
N_random_16_p2_s2_1$primary <- 0

#remove age = 0
N_random_16_p2_s2_1$age <- N_random_16_p2_s2_1$age -1
N_random_16_p2_s2_1 <- filter(N_random_16_p2_s2_1, age > 0)

D_random_16_p2_s2$age <- D_random_16_p2_s2$age -1
D_random_16_p2_s2 <- filter(D_random_16_p2_s2, age > 0)

#Combine init with Dafter
N_random_16_p2_s2 <- rbind(N_random_16_p2_s2_1, D_random_16_p2_s2)

N_random_16_p2_allages <- rbind(N_random_16_p2_s1, N_random_16_p2_s2)

#Sum ages 
N_random_16_p2 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_16_p2_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_16_p2_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_16_p2), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_16_p2_segfin <- N_random_16_p2_mean %>% filter(primary == max(N_random_16_p2_mean$primary))

#sum segments
N_random_16_p2_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_16_p2), 
                               FUN = sum)
###### Final total N #####
N_random_16_p2_Nfin <- N_random_16_p2_Nsum %>% filter(primary == max(N_random_16_p2_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_16_p2_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_16_p2_Nsum), 
                                    FUN = mean)

N_random_16_p2_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_16_p2_CIl)[4] <- 'low.1'

N_random_16_p2_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p2_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_16_p2_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_16_p2_Nvtime <- cbind(N_random_16_p2_Nsum_mean,
                           low.1 = N_random_16_p2_CIl$low.1,
                           high.9 = N_random_16_p2_CIh$high.9)

# ggplot(random_16_p2_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p2_s1 <- fread(file_name)
Dcol_random_16_p2_s1 <- data.frame(Dcol_random_16_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p2_s2 <- fread(file_name)
Dcol_random_16_p2_s2 <- data.frame(Dcol_random_16_p2_s2)

Dcol_random_16_p2 <- rbind(Dcol_random_16_p2_s1, Dcol_random_16_p2_s2)

###### D Columbia ######
Dcol_random_16_p2_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_16_p2), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p2_s1 <- fread(file_name)
Dtrav_random_16_p2_s1 <- data.frame(Dtrav_random_16_p2_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p2_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p2_s2 <- fread(file_name)
Dtrav_random_16_p2_s2 <- data.frame(Dtrav_random_16_p2_s2)

Dtrav_random_16_p2 <- rbind(Dtrav_random_16_p2_s1, Dtrav_random_16_p2_s2)

###### Distance traveled ######
Dtrav_random_16_p2_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_16_p2), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_16_p2_fin <- N_random_16_p2 %>% filter(primary == max(N_random_16_p2$primary))

Ninvade_random_16_p2 <- N_random_16_p2_fin
colnames(Ninvade_random_16_p2)[7] <- 'invade'
Ninvade_random_16_p2$invade[Ninvade_random_16_p2$invade <= 1000] <- 0
Ninvade_random_16_p2$invade[Ninvade_random_16_p2$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_16_p2 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_16_p2), 
                                FUN = sum)

################################################################################
##### random_16rem_p3 #####
#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s1'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p3_s1 <- fread(file_name)
N_random_16_p3_s1 <- data.frame(N_random_16_p3_s1)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p3_s1 <- fread(file_name)
D_random_16_p3_s1 <- data.frame(D_random_16_p3_s1)

#Initial pop: 0
N_random_16_p3_s1_1 <- filter(N_random_16_p3_s1, primary == 1)
N_random_16_p3_s1_1$primary <- 0

#remove age = 0
N_random_16_p3_s1_1$age <- N_random_16_p3_s1_1$age -1
N_random_16_p3_s1_1 <- filter(N_random_16_p3_s1_1, age > 0)

D_random_16_p3_s1$age <- D_random_16_p3_s1$age -1
D_random_16_p3_s1 <- filter(D_random_16_p3_s1, age > 0)

#Combine init with Dafter
N_random_16_p3_s1 <- rbind(N_random_16_p3_s1_1, D_random_16_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s2'
file_name = paste(path, 'N.csv',sep = '/')
N_random_16_p3_s2 <- fread(file_name)
N_random_16_p3_s2 <- data.frame(N_random_16_p3_s2)

file_name = paste(path, 'D.csv',sep = '/')
D_random_16_p3_s2 <- fread(file_name)
D_random_16_p3_s2 <- data.frame(D_random_16_p3_s2)

#Initial pop: 0
N_random_16_p3_s2_1 <- filter(N_random_16_p3_s2, primary == 1)
N_random_16_p3_s2_1$primary <- 0

#remove age = 0
N_random_16_p3_s2_1$age <- N_random_16_p3_s2_1$age -1
N_random_16_p3_s2_1 <- filter(N_random_16_p3_s2_1, age > 0)

D_random_16_p3_s2$age <- D_random_16_p3_s2$age -1
D_random_16_p3_s2 <- filter(D_random_16_p3_s2, age > 0)

#Combine init with Dafter
N_random_16_p3_s2 <- rbind(N_random_16_p3_s2_1, D_random_16_p3_s2)

N_random_16_p3_allages <- rbind(N_random_16_p3_s1, N_random_16_p3_s2)

#Sum ages 
N_random_16_p3 <- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_random_16_p3_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_random_16_p3_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_random_16_p3), 
                               FUN = mean)

###### Average final N @ sites #####
N_random_16_p3_segfin <- N_random_16_p3_mean %>% filter(primary == max(N_random_16_p3_mean$primary))

#sum segments
N_random_16_p3_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_random_16_p3), 
                               FUN = sum)
###### Final total N #####
N_random_16_p3_Nfin <- N_random_16_p3_Nsum %>% filter(primary == max(N_random_16_p3_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_random_16_p3_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_random_16_p3_Nsum), 
                                    FUN = mean)

N_random_16_p3_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_random_16_p3_CIl)[4] <- 'low.1'

N_random_16_p3_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_random_16_p3_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_random_16_p3_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
random_16_p3_Nvtime <- cbind(N_random_16_p3_Nsum_mean,
                           low.1 = N_random_16_p3_CIl$low.1,
                           high.9 = N_random_16_p3_CIh$high.9)

# ggplot(random_16_p3_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p3_s1 <- fread(file_name)
Dcol_random_16_p3_s1 <- data.frame(Dcol_random_16_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s2'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_random_16_p3_s2 <- fread(file_name)
Dcol_random_16_p3_s2 <- data.frame(Dcol_random_16_p3_s2)

Dcol_random_16_p3 <- rbind(Dcol_random_16_p3_s1, Dcol_random_16_p3_s2)

###### D Columbia ######
Dcol_random_16_p3_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_random_16_p3), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s1'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p3_s1 <- fread(file_name)
Dtrav_random_16_p3_s1 <- data.frame(Dtrav_random_16_p3_s1)

path <- 'D:\\Chapter2\\results\\random\\random_16rem_p3_s2'
file_name = paste(path, 'dist_travel.csv',sep = '/')
Dtrav_random_16_p3_s2 <- fread(file_name)
Dtrav_random_16_p3_s2 <- data.frame(Dtrav_random_16_p3_s2)

Dtrav_random_16_p3 <- rbind(Dtrav_random_16_p3_s1, Dtrav_random_16_p3_s2)

###### Distance traveled ######
Dtrav_random_16_p3_sum <- aggregate(distance ~  param + sim +p + rem , 
                                  data = as.data.frame(Dtrav_random_16_p3), 
                                  FUN = sum)

#-----------------------------------------------------------------------------#
N_random_16_p3_fin <- N_random_16_p3 %>% filter(primary == max(N_random_16_p3$primary))

Ninvade_random_16_p3 <- N_random_16_p3_fin
colnames(Ninvade_random_16_p3)[7] <- 'invade'
Ninvade_random_16_p3$invade[Ninvade_random_16_p3$invade <= 1000] <- 0
Ninvade_random_16_p3$invade[Ninvade_random_16_p3$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_random_16_p3 <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_random_16_p3), 
                                FUN = sum)

###############################################################################
##### Combined data #####
path <- 'D:\\Chapter2\\results\\random'

#-------------------average final N @ sites -----------------------------------#
random_segfin  <- rbind(N_random_4_p1_segfin,N_random_4_p2_segfin,N_random_4_p3_segfin,
                       N_random_8_p1_segfin,N_random_8_p2_segfin,N_random_8_p3_segfin,
                       N_random_16_p1_segfin,N_random_16_p2_segfin,N_random_16_p3_segfin)

random_segfin$location <- 'random'

file_name = paste(path, 'random_Nseg.csv',sep = '/')
fwrite(random_segfin,file_name)

#------------------- final N total -----------------------------------#
random_Nfin <- rbind(N_random_4_p1_Nfin,N_random_4_p2_Nfin,N_random_4_p3_Nfin,
                    N_random_8_p1_Nfin,N_random_8_p2_Nfin,N_random_8_p3_Nfin,
                    N_random_16_p1_Nfin,N_random_16_p2_Nfin,N_random_16_p3_Nfin)

random_Nfin$location <- 'random'
file_name = paste(path, 'random_Ntotal.csv',sep = '/')
fwrite(random_Nfin,file_name)

# ggplot(random_Nfin)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))

#------------------- N vs time -----------------------------------#
random_Nvtime <- rbind(random_4_p1_Nvtime,random_4_p2_Nvtime,random_4_p3_Nvtime,
                      random_8_p1_Nvtime,random_8_p2_Nvtime,random_8_p3_Nvtime,
                      random_16_p1_Nvtime,random_16_p2_Nvtime,random_16_p3_Nvtime)

random_Nvtime$location <- 'random'
file_name = paste(path, 'random_Nvtime.csv',sep = '/')
fwrite(random_Nvtime,file_name)

#------------------- D Columbia  -----------------------------------#
random_Dcol <- rbind(Dcol_random_4_p1_sum,Dcol_random_4_p2_sum,Dcol_random_4_p3_sum,
                    Dcol_random_8_p1_sum,Dcol_random_8_p2_sum,Dcol_random_8_p3_sum,
                    Dcol_random_16_p1_sum,Dcol_random_16_p2_sum,Dcol_random_16_p3_sum)

random_Dcol$location <- 'random'
file_name = paste(path, 'random_Dcol.csv',sep = '/')
fwrite(random_Dcol,file_name)

# ggplot(random_Dcol)+
#   geom_boxplot(aes(x = p, y = count, group = interaction(p,rem), col = rem))
# 

#------------------- distance traveled  -----------------------------------#
random_Dtrav <- rbind(Dtrav_random_4_p1_sum,Dtrav_random_4_p2_sum,Dtrav_random_4_p3_sum,
                     Dtrav_random_8_p1_sum,Dtrav_random_8_p2_sum,Dtrav_random_8_p3_sum,
                     Dtrav_random_16_p1_sum,Dtrav_random_16_p2_sum,Dtrav_random_16_p3_sum)

random_Dtrav$location <- 'random'
file_name = paste(path, 'random_Dtrav.csv',sep = '/')
fwrite(random_Dtrav,file_name)

# ggplot(random_Dtrav)+
#   geom_boxplot(aes(x = p, y = distance, group = interaction(p,rem), col = rem))

#------------------- segs invaded  -----------------------------------#
random_Ninvade <- rbind(Ninvade_random_4_p1,Ninvade_random_4_p2,Ninvade_random_4_p3,
                       Ninvade_random_8_p1,Ninvade_random_8_p2,Ninvade_random_8_p3,
                       Ninvade_random_16_p1,Ninvade_random_16_p2,Ninvade_random_16_p3)

random_Ninvade$location <- 'random'
file_name = paste(path, 'random_Ninvade.csv',sep = '/')
fwrite(random_Ninvade,file_name)

# ggplot(random_Ninvade)+
#   geom_boxplot(aes(x = p, y = invade, group = interaction(p,rem), col = rem))
# 



