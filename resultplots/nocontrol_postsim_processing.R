library(tidyverse)
library(here)
library(plyr)
library(data.table)

#-----------------------------------------------------------------------------#
path <- 'E:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'N.csv',sep = '/')
N_nocontrol<- fread(file_name)
N_nocontrol <- data.frame(N_nocontrol)

file_name = paste(path, 'D.csv',sep = '/')
D_nocontrol<- fread(file_name)
D_nocontrol <- data.frame(D_nocontrol)

#Initial pop: 0
N_nocontrol_1 <- filter(N_nocontrol, primary == 1)
N_nocontrol_1$primary <- 0

#remove age = 0
N_nocontrol_1$age <- N_nocontrol_1$age -1
N_nocontrol_1 <- filter(N_nocontrol_1, age > 0)

D_nocontrol$age <- D_nocontrol$age -1
D_nocontrol <- filter(D_nocontrol, age > 0)

#Combine init with Dafter
N_nocontrol_allages <- rbind(N_nocontrol_1, D_nocontrol)

#Sum ages 
N_nocontrol<- aggregate(count ~ segment + primary + param+ sim +p + rem , 
                          data = as.data.frame(N_nocontrol_allages), 
                          FUN = sum)

#-----------------------------------------------------------------------------#

#average at all segments
N_nocontrol_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_nocontrol), 
                               FUN = mean)

###### Average final N @ sites #####
N_nocontrol_segfin <- N_nocontrol_mean %>% filter(primary == max(N_nocontrol_mean$primary))

#sum segments
N_nocontrol_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_nocontrol), 
                               FUN = sum)
###### Final total N #####
N_nocontrol_Nfin <- N_nocontrol_Nsum %>% filter(primary == max(N_nocontrol_Nsum$primary))

#-----------------------------------------------------------------------------#
#sum segements and average across parameters and simulations
N_nocontrol_Nsum_mean <- aggregate(count ~  primary +p + rem , 
                                    data = as.data.frame(N_nocontrol_Nsum), 
                                    FUN = mean)

N_nocontrol_CIl <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_nocontrol_Nsum), 
                              function(x) quantile(x, probs = 0.1))

colnames(N_nocontrol_CIl)[4] <- 'low.1'

N_nocontrol_CIh <- aggregate(count ~  primary +p + rem , 
                              data = as.data.frame(N_nocontrol_Nsum), 
                              function(x) quantile(x, probs = 0.9))
colnames(N_nocontrol_CIh)[4] <- 'high.9'
#-----------------------------------------------------------------------------#
###### N vs time #####
nocontrol_Nvtime <- cbind(N_nocontrol_Nsum_mean,
                           low.1 = N_nocontrol_CIl$low.1,
                           high.9 = N_nocontrol_CIh$high.9)

# ggplot(nocontrol_Nvtime)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))

file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_nocontrol <- fread(file_name)
Dcol_nocontrol <- data.frame(Dcol_nocontrol)

###### D Columbia ######
Dcol_nocontrol_sum <- aggregate(count ~  sim +p + rem +param, 
                                 data = as.data.frame(Dcol_nocontrol), 
                                 FUN = sum)

#-----------------------------------------------------------------------------#
N_nocontrol_fin <- N_nocontrol %>% filter(primary == max(N_nocontrol$primary))

Ninvade_nocontrol<- N_nocontrol_fin
colnames(Ninvade_nocontrol)[7] <- 'invade'
Ninvade_nocontrol$invade[Ninvade_nocontrol$invade <= 1000] <- 0
Ninvade_nocontrol$invade[Ninvade_nocontrol$invade > 1000] <- 1

###### Segs Invaded ######
Ninvade_nocontrol <- aggregate(invade ~  param + sim +p + rem , 
                                data = as.data.frame(Ninvade_nocontrol), 
                                FUN = sum)

###############################################################################
##### Combined data #####
path <- 'E:\\Chapter2\\results\\nocontrol'

#------------------- final N @ sites -----------------------------------#
N_nocontrol$location <- 'nocontrol'

file_name = paste(path, 'nocontrol_segfin_summary.csv',sep = '/')
fwrite(N_nocontrol,file_name)

#-------------------average final N @ sites -----------------------------------#
N_nocontrol_segfin$location <- 'nocontrol'
file_name = paste(path, 'nocontrol_Nseg.csv',sep = '/')
fwrite(N_nocontrol_segfin,file_name)

#------------------- final N total -----------------------------------#
N_nocontrol_Nfin$location <- 'nocontrol'
file_name = paste(path, 'nocontrol_Ntotal.csv',sep = '/')
fwrite(N_nocontrol_Nfin,file_name)

#------------------- N vs time -----------------------------------#
nocontrol_Nvtime$location <- 'nocontrol'
file_name = paste(path, 'nocontrol_Nvtime.csv',sep = '/')
fwrite(nocontrol_Nvtime,file_name)

#------------------- D Columbia  -----------------------------------#
Dcol_nocontrol_sum$location <- 'nocontrol'
file_name = paste(path, 'nocontrol_Dcol.csv',sep = '/')
fwrite(Dcol_nocontrol_sum,file_name)

#------------------- segs invaded  -----------------------------------#
Ninvade_nocontrol$location <- 'nocontrol'
file_name = paste(path, 'nocontrol_Ninvade.csv',sep = '/')
fwrite(Ninvade_nocontrol,file_name)





