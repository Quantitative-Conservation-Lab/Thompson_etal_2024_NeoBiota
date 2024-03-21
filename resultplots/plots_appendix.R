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

all_Ntotal <- all_Ntotal %>% filter(location != 'nocontrol')

all_Ntotal <- rbind(all_Ntotal,Nocontrol1, Nocontrol4, Nocontrol8, Nocontrol16)

level_order <- c("nocontrol", "abund", "down", "edge", "grow", "random")

all_Ntotal$rem <- as.factor(all_Ntotal$rem)

new <- c( "1 segment", "4 segments", "8 segments",  "16 segments") 
names(new) <-  c("1", "4", "8", "16") 


col <- brewer.pal(5, "Set1") 
colors <- c(col[1], col[2], col[4], col[5])
rem.label <- c("None", "Trap effect", "No trap effect", "Constant 0.5")

all_Ntotal <- all_Ntotal %>% filter()

ggplot(all_Ntotal)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = count, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Management Strategy") + ylab("Final total crayfish abundance (millions)")+
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

all_Nvtime$year <- (all_Nvtime$primary/12) + 1
all_Nvtime$location[all_Nvtime$location == 'nocontrol'] <- 'No control'  
all_Nvtime$location[all_Nvtime$location == 'down'] <- 'Downstream'  
all_Nvtime$location[all_Nvtime$location == 'abund'] <- 'Abundance'  
all_Nvtime$location[all_Nvtime$location == 'grow'] <- 'Growth'  
all_Nvtime$location[all_Nvtime$location == 'edge'] <- 'Edge'  
all_Nvtime$location[all_Nvtime$location == 'random'] <- 'Random'  

colnames(all_Nvtime)[3] <- "segments"

all_Nvtime$p <- as.factor(all_Nvtime$p)

rem.labelp <- c("0 = None", "1 = Trap effect", "2 = No trap effect", "3 = Constant 0.5")

all_Nvtime$Strategy <- all_Nvtime$location

ggplot(all_Nvtime)+
 geom_ribbon(aes(x = year, ymin = low.1, group = interaction(p,segments, Strategy), ymax = high.9), fill = 'lightgrey', alpha = 0.6)+
  geom_line(aes(x = year, y = count, group = interaction(p,segments, Strategy), color = p))+
  facet_wrap(~segments+ Strategy + p, labeller = label_both)+
  xlab("Year") + ylab("Final total crayfish abundance (millions)")+
  scale_color_manual(name = "Removal rate (p)", labels = rem.labelp, values = colors)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = c(0, 150000000), labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


colD <- brewer.pal(9, "Set1") 
colorsD <- c(colD[2], colD[3], colD[4], colD[5], "black", colD[9])
labelD <- c("Abundance", "Downstream", "Edge", "Growth", "No control", "Random")


file_name = paste(path, 'all_Nvtime.csv',sep = '/')
all_Nvtime <- fread(file_name)
all_Nvtime <- data.frame(all_Nvtime)

all_Nvtime$year <- (all_Nvtime$primary/12) + 1

all_Nvtime$p[all_Nvtime$p == 0] <- 'None'
all_Nvtime$p[all_Nvtime$p == 1] <- 'Trap effect'
all_Nvtime$p[all_Nvtime$p == 2] <- 'No trap effect'
all_Nvtime$p[all_Nvtime$p == 3] <- 'Constant 0.5'

colnames(all_Nvtime)[3] <- "segments"

ggplot(all_Nvtime)+
  geom_line(aes(x = year, y = count, group = interaction(p,segments, location),color = location), lwd = 0.75)+
  scale_color_manual(name = "Management Strategy", labels = labelD, values = colorsD) +
  facet_wrap(~segments+ p, labeller=label_both)+
  xlab("Year") + ylab("Final total crayfish abundance (millions)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


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
  xlab("Management Strategy") + ylab("Total crayfish in the Columbia River")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
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

new <- c("1 segments", "4 segments", "8 segments",  "16 segments") 
names(new) <-  c("1", "4", "8", "16") 

ggplot(all_Dtrav)+
  geom_boxplot(aes(x = factor(location, level = level_order), y = distance, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Management Strategy") + ylab("Total distance traveled")+
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
  geom_boxplot(aes(x = factor(location, level = level_order), y = invasion, 
                   group = interaction(p,rem, location), col = as.factor(p)))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_color_manual(name = "Removal rate", labels = rem.label, values = colors) +
  xlab("Management Strategy") + ylab("Number of segments invaded")+
  facet_wrap( ~rem, nrow = 3, labeller = labeller(rem = new))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




