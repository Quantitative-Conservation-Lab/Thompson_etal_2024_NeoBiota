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

path <- here::here("results", "Truth_alternatives")

####True data summed across segments :####  
path2 <- "edge_truth"
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

#Combine init with Dafter
Dafter_mean_allseg_edge1 <- rbind(Ninit_allsegssims_edge1, Dafter_allsegssims_edge1)

N_allsims <- Dafter_mean_allseg_edge1
head(N_allsims)

N_allsims <- N_allsims %>% filter(primary <= 84) #7 years of simulation

#sum across ages 

N_allsims_sumage  <- aggregate(count ~ segment + primary + sim + param, 
                           data = as.data.frame(N_allsims), 
                           FUN = sum)

#### sum across segments and ages####
N_sims_parms  <- aggregate(count ~ primary + sim + param, 
                     data = as.data.frame(N_allsims), 
                     FUN = sum)

N_parms_mean  <- aggregate(count ~ primary + param, 
                           data = as.data.frame(N_sims_parms), 
                           FUN = mean)

N_mean  <- aggregate(count ~ primary, 
                           data = as.data.frame(N_parms_mean), 
                           FUN = mean)

ggplot(N_mean) +
  geom_line(mapping = aes(x = primary, y = count))


ggplot(N_parms_mean, aes(x=primary, y=count)) + 
  geom_boxplot(aes(group = primary))

ggplot(N_sims_parms, aes(x=primary, y=count)) + 
  geom_boxplot(aes(group = primary)) 

N.plots <- data.frame(primary = seq(0,84), mean = rep(NA, 85), lower = rep(NA, 85), upper = rep(NA, 85))

for(i in 1:85){
  df <- N_sims_parms %>% filter(primary == i-1)
  
  N.plots$mean[i] <- mean(df$count)
  N.plots$lower[i] <- as.numeric(quantile(df$count, 0.05))
  N.plots$upper[i] <- as.numeric(quantile(df$count, 0.95))
  
}


N.plots$year <- N.plots$primary/12

ggplot(N.plots, aes(x = year)) +
  geom_line(mapping = aes(y = mean), color = "black", lwd = 1)+ 
  ylab("Total Crayfish Abundance") + xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "pink", alpha = .3)+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.title.x = element_text(size = 16),
                     axis.text.x = element_text(size = 14),
                     axis.title.y = element_text(size = 16),
                     axis.line = element_line(colour = "black"))

file_name = paste(path, path2, 'DR_edge_N_plot.csv',sep = '/')
write.csv(N.plots,file_name)

#### Minimize abundance ####
N_simsfinal <- N_sims_parms  %>% filter(primary == max(N_sims_parms$primary))

N_simsfinal

ggplot(N_simsfinal, aes(x=param, y=count)) + 
  geom_boxplot(aes(group = param))


ggplot(N_simsfinal, aes(x=primary, y=count)) + 
  geom_boxplot(aes(group = primary))


summary(N_simsfinal$count)

file_name = paste(path,path2, 'DR_edge_minabund.csv',sep = '/')
write.csv(N_simsfinal,file_name)

#### Minimize spatial extent ####
N_lastsegs1 <- N_allsims  %>% filter(primary == max(N_allsims$primary))
N_lastsegs  <- aggregate(count ~ primary + sim + param + segment, 
                     data = as.data.frame(N_lastsegs1), 
                     FUN = sum) #sum across ages

head(N_lastsegs)

N_space  <- aggregate(count ~ segment, 
                      data = as.data.frame(N_lastsegs), 
                      FUN = mean)


N_space

N_lastsegs$pop <- NA
N_lastsegs$pop <- replace(N_lastsegs$pop, N_lastsegs$count >20000 , 1)
N_lastsegs$pop <- replace(N_lastsegs$pop, N_lastsegs$count <= 20000 , 0)

# N_lastsegs$pop <- replace(N_lastsegs$pop, N_lastsegs$count >0 , 1)
# N_lastsegs$pop <- replace(N_lastsegs$pop, N_lastsegs$count == 0 , 0)

#sum the number of segments where count == 0 for each simulation (max = 35)
N_noedges  <- aggregate(pop ~ sim + param, 
                         data = as.data.frame(N_lastsegs), 
                         FUN = sum) #sum across ages

ggplot(N_noedges, aes(x=param, y=pop)) + 
  geom_boxplot(aes(group = param))

N_noedges$bar <- 1

ggplot(N_noedges, aes(x=bar, y=pop/35)) + 
  geom_boxplot(aes(group = bar))

(summary(N_noedges$pop)/35)


file_name = paste(path,path2, 'DR_edge_minspatial.csv',sep = '/')
write.csv(N_noedges,file_name)

#### Removal data ####
file_name = paste(path, path2,'DR_edge_Y_all.csv',sep = '/')
Y_all1 <- fread(file= file_name)
Y_all <- data.frame(Y_all1)[-1]

Y_all_prim <- aggregate(count ~ segment + primary + sim + param,
                        data = as.data.frame(Y_all), FUN = sum)

Y_all_prim_avg <- aggregate(count ~ segment + primary,
                            data = as.data.frame(Y_all_prim), FUN = mean)

Y_all_prim_avg$year <- rep(1:N.years, each = (I*12))

Y_all_prim_tot <- aggregate(count ~ year + segment,
                            data = as.data.frame(Y_all_prim_avg), FUN = sum)

Y_all_prim_tot %>% filter(year == 2)
