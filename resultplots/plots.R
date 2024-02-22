library(tidyverse)
library(here)
library(plyr)
library(data.table)
library(scales)
library(RColorBrewer) 

path <- 'D:\\Chapter2\\results'

###### Final total N #####
file_name = paste(path, 'all_Ntotal.csv',sep = '/')
all_Ntotal <- fread(file_name)
all_Ntotal <- data.frame(all_Ntotal)

Nocontrol <- all_Ntotal %>% filter(location == 'nocontrol')
Nocontrol1 <- Nocontrol
Nocontrol1$rem <- 1
Nocontrol4 <- Nocontrol
Nocontrol4$rem <- 4
Nocontrol8 <- Nocontrol
Nocontrol8$rem <- 8
Nocontrol16 <- Nocontrol
Nocontrol16$rem <- 16

all_Ntotal <- all_Ntotal %>% filter(rem > 1)
all_Ntotal <- rbind(all_Ntotal, Nocontrol4, Nocontrol8, Nocontrol16)

level_order <- c("nocontrol", "abund", "down", "edge", "grow", "random")

all_Ntotal$rem <- as.factor(all_Ntotal$rem)

new <- c("4 segments", "8 segments",  "16 segments") 
names(new) <-  c("4", "8", "16") 


col <- brewer.pal(5, "Set1") 
colors <- c(col[1], col[2], col[4], col[5])
rem.label <- c("None", "Trap effect", "No trap effect", "Constant 0.5")

ggplot(all_Ntotal)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = count, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Final total crayfish abundance (millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  facet_wrap( ~rem, nrow = 3, labeller = labeller(rem = new))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


###### N v Time #####
file_name = paste(path, 'all_Nvtime.csv',sep = '/')
all_Nvtime <- fread(file_name)
all_Nvtime <- data.frame(all_Nvtime)

ggplot(all_Nvtime)+
 # geom_ribbon(aes(x = primary, ymin = low.1, group = interaction(p,rem, location), ymax = high.9, fill = p), alpha = 0.6)+
  geom_line(aes(x = primary, y = count, group = interaction(p,rem, location), color = p))+
  facet_wrap(~rem+ location)

###### Entered Columbia #####
file_name = paste(path, 'all_Dcol.csv',sep = '/')
all_Dcol <- fread(file_name)
all_Dcol <- data.frame(all_Dcol)

Nocontrol <- all_Dcol %>% filter(location == 'nocontrol')
Nocontrol1 <- Nocontrol
Nocontrol1$rem <- 1
Nocontrol4 <- Nocontrol
Nocontrol4$rem <- 4
Nocontrol8 <- Nocontrol
Nocontrol8$rem <- 8
Nocontrol16 <- Nocontrol
Nocontrol16$rem <- 16

all_Dcol <- all_Dcol %>% filter(rem > 0)
all_Dcol <- rbind(all_Dcol, Nocontrol1, Nocontrol4, Nocontrol8, Nocontrol16)


all_Dcol$rem <- as.factor(all_Dcol$rem)

new <- c("1 segment", "4 segments", "8 segments",  "16 segments") 
names(new) <-  c("1", "4", "8", "16") 

ggplot(all_Dcol)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = count, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Total crayfish in the Columbia River")+
  #scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  facet_wrap( ~rem, nrow = 3, labeller = labeller(rem = new))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

###### Distance Traveled #####
file_name = paste(path, 'all_Dtrav.csv',sep = '/')
all_Dtrav <- fread(file_name)
all_Dtrav <- data.frame(all_Dtrav)

all_Dtrav <- all_Dtrav %>% filter(rem > 1)

all_Dtrav$rem <- as.factor(all_Dtrav$rem)

new <- c("4 segments", "8 segments",  "16 segments") 
names(new) <-  c("4", "8", "16") 

ggplot(all_Dtrav)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = distance, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Total distance traveled")+
  facet_wrap( ~rem, nrow = 3, labeller = labeller(rem = new))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


###### Invaded #####
file_name = paste(path, 'all_Ninvade.csv',sep = '/')
all_Ninvade <- fread(file_name)
all_Ninvade <- data.frame(all_Ninvade)

Nocontrol <- all_Ninvade %>% filter(location == 'nocontrol')
Nocontrol1 <- Nocontrol
Nocontrol1$rem <- 1
Nocontrol4 <- Nocontrol
Nocontrol4$rem <- 4
Nocontrol8 <- Nocontrol
Nocontrol8$rem <- 8
Nocontrol16 <- Nocontrol
Nocontrol16$rem <- 16

all_Ninvade <- all_Ninvade %>% filter(rem > 0)
all_Ninvade <- rbind(all_Ninvade, Nocontrol1, Nocontrol4, Nocontrol8, Nocontrol16)


all_Ninvade$rem <- as.factor(all_Ninvade$rem)

new <- c("1 segment", "4 segments", "8 segments",  "16 segments") 
names(new) <-  c("1", "4", "8", "16") 


ggplot(all_Ninvade)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = invade, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Number of segments invaded")+
  facet_wrap( ~rem, nrow = 3, labeller = labeller(rem = new))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
