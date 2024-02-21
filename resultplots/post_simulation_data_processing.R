library(tidyverse)
library(here)
library(plyr)
library(data.table)

#### Abund data  ####
##### abund_4rem_p1 #####
###### N ######
path <- 'D:\\Chapter2_results\\results\\abund\\abund_4rem_p1_s1'
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

path <- 'D:\\Chapter2_results\\results\\abund\\abund_4rem_p1_s2'
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

#average at all segments
N_abund_4_p1_mean <- aggregate(count ~ segment + primary +p + rem , 
                               data = as.data.frame(N_abund_4_p1), 
                               FUN = mean)

#sum segments
N_abund_4_p1_Nsum <- aggregate(count ~  primary + param+ sim +p + rem , 
                               data = as.data.frame(N_abund_4_p1), 
                               FUN = sum)

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


N_abund_4_p1_summary <- cbind(N_abund_4_p1_Nsum_mean,
                              low.1 = N_abund_4_p1_CIl$low.1,
                              high.9 = N_abund_4_p1_CIh$high.9)

# ggplot(N_abund_4_p1_summary)+
#   geom_ribbon(aes(x = primary, ymin = low.1, ymax = high.9),fill = 'grey70', alpha = 0.6)+
#   geom_line(aes(x = primary, y = count))


###### D Columbia ######
path <- 'D:\\Chapter2_results\\results\\abund\\abund_4rem_p1_s1'
file_name = paste(path, 'D_columbia.csv',sep = '/')
Dcol_abund_4_p1_s1 <- fread(file_name)
Dcol_abund_4_p1_s1 <- data.frame(N_abund_4_p1_s1)
