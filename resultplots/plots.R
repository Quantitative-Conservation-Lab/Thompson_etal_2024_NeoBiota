library(plyr)
library(tidyverse)
library(here)
library(data.table)
library(scales)
library(RColorBrewer) 
library("cowplot")
library(rPref)
library(ggrepel)
library(cowplot)
library(gridExtra)

path <- 'E:\\Chapter2\\results'

low <- 0.1
high <- 0.9

#### Final total N ####
file_name = paste(path, 'all_Ntotal.csv',sep = '/')
all_Ntotal <- fread(file_name)
all_Ntotal <- data.frame(all_Ntotal)

all_Ntotal$rem <- as.factor(all_Ntotal$rem)

detach(package:plyr)


suppress0 <- all_Ntotal %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress1 <-all_Ntotal %>%
  filter(rem == '1'& p == 1) %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress4 <-all_Ntotal %>%
  filter(rem == '4'& p == 1) %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress8 <-all_Ntotal %>%
  filter(rem == '8'& p == 1) %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress16 <-all_Ntotal %>%
  filter(rem == '16'& p == 1) %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

##### plot #####
nc.val <- all_Ntotal %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ntotal <- all_Ntotal %>% filter(p == 1)

all_Ntotal_rem1 <- all_Ntotal %>% filter(rem == '1')
all_Ntotal_rem1$location[all_Ntotal_rem1$location == 'abund'] <- 'abund1'
all_Ntotal_rem1$location[all_Ntotal_rem1$location == 'down'] <- 'down1'
all_Ntotal_rem1$location[all_Ntotal_rem1$location == 'edge'] <- 'edge1'
all_Ntotal_rem1$location[all_Ntotal_rem1$location == 'grow'] <- 'grow1'
all_Ntotal_rem1$location[all_Ntotal_rem1$location == 'random'] <- 'random1'

all_Ntotal_rem4 <- all_Ntotal %>% filter(rem == '4')
all_Ntotal_rem4$location[all_Ntotal_rem4$location == 'abund'] <- 'abund4'
all_Ntotal_rem4$location[all_Ntotal_rem4$location == 'down'] <- 'down4'
all_Ntotal_rem4$location[all_Ntotal_rem4$location == 'edge'] <- 'edge4'
all_Ntotal_rem4$location[all_Ntotal_rem4$location == 'grow'] <- 'grow4'
all_Ntotal_rem4$location[all_Ntotal_rem4$location == 'random'] <- 'random4'

all_Ntotal_rem8 <- all_Ntotal %>% filter(rem == '8')
all_Ntotal_rem8$location[all_Ntotal_rem8$location == 'abund'] <- 'abund8'
all_Ntotal_rem8$location[all_Ntotal_rem8$location == 'down'] <- 'down8'
all_Ntotal_rem8$location[all_Ntotal_rem8$location == 'edge'] <- 'edge8'
all_Ntotal_rem8$location[all_Ntotal_rem8$location == 'grow'] <- 'grow8'
all_Ntotal_rem8$location[all_Ntotal_rem8$location == 'random'] <- 'random8'

all_Ntotal_rem16 <- all_Ntotal %>% filter(rem == '16')
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'abund'] <- 'abund16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'down'] <- 'down16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'edge'] <- 'edge16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'grow'] <- 'grow16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'random'] <- 'random16'

all_Ntotal <- rbind(all_Ntotal_rem1,all_Ntotal_rem4, all_Ntotal_rem8, all_Ntotal_rem16 )
level_order <- c("abund1", "abund4", "abund8","abund16", 
                 "grow1","grow4", "grow8","grow16", 
                 "edge1", "edge4","edge8","edge16",
                 "down1", "down4", "down8", "down16",
                 "random1", "random4", "random8","random16")
new <- c("1 segment", "4 segments", "8 segments", "16 segments") 
names(new) <-  c("1", "4", "8", "16") 

all_Ntotal$rem2 <- all_Ntotal$rem

###### Plot 1 ####

max.data <- all_Ntotal %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '1') 

s1 <- all_Ntotal %>% 
  filter(rem == '1') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem2, location)))+
  geom_boxplot(
    position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem2, location)), color = "red") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  xlab("") + ylab("Suppression")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 80000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title=element_text(size=14,face="bold"),
       # axis.text.x = element_text(angle = 310, hjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 4 ####
max.data <- all_Ntotal %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '4') 

s4 <- all_Ntotal %>% 
  filter(rem == '4') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem2, location)))+
  geom_boxplot(
    position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem2, location)), color = "red") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  xlab("") + ylab("")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 80000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        #axis.title=element_text(size=14,face="bold"),
        #axis.text.x = element_text(angle = 310, hjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 8 ####

max.data <- all_Ntotal %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '8') 

s8 <- all_Ntotal %>% 
  filter(rem == '8') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem2, location)))+
  geom_boxplot(
    position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem2, location)), color = "red") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
      "abund1" = "",
      "abund4" = "",
      "abund8" = "",
      "abund16" = "",
      "grow1" = "", 
      "grow4" = "", 
      "grow8" = "", 
      "grow16" = "", 
      "edge1" = "",
      "edge4" = "",
      "edge8" = "",
      "edge16" = "",
      "down1" = "",
      "down4" = "",
      "down8" = "",
      "down16" = "",
      "random1" = "",
      "random4" = "",
      "random8" = "",
      "random16" = ""))+
  xlab("") + ylab("")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 80000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
        #axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 310, hjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 16 ####

max.data <- all_Ntotal %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '16') 

s16 <- all_Ntotal %>% 
  filter(rem == '16') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem2, location)))+
  geom_boxplot(
    position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem2, location)), color = "red") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  xlab("") + ylab("")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 80000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 310, hjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


splots <- plot_grid(s1,s4,s8,s16, nrow = 1)

###### Plot different ####

max.data <- all_Ntotal %>%
  group_by(location, rem2) %>%
  filter(count == max(count)) 

sall <- all_Ntotal %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem2, location)))+
  geom_boxplot(
    position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem2, location)), color = "red") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  xlab("") + ylab("Suppression")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 80000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"))+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


#splots <- plot_grid(sall, nrow = 1)

#### Invaded ####
file_name = paste(path, 'all_Ninvade.csv',sep = '/')
all_Ninvade <- fread(file_name)
all_Ninvade <- data.frame(all_Ninvade)

nocontrol <- all_Ninvade %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Ninvade <- rbind(all_Ninvade, nocontrol)

all_Ninvade <- all_Ninvade %>% filter(p == 1)

all_Ninvade$rem <- as.factor(all_Ninvade$rem)

all_Ninvade$count <- all_Ninvade$invasion/ 35

contain0 <- all_Ninvade %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain1 <-all_Ninvade %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain4 <- all_Ninvade %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain8 <- all_Ninvade %>%
  filter(rem == '8') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain16 <-all_Ninvade %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))



##### Plot ####
nc.val <- all_Ninvade %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ninvade <- all_Ninvade %>% filter(p == 1)

all_Ninvade_rem1 <- all_Ninvade %>% filter(rem == '1')
all_Ninvade_rem1$location[all_Ninvade_rem1$location == 'abund'] <- 'abund1'
all_Ninvade_rem1$location[all_Ninvade_rem1$location == 'down'] <- 'down1'
all_Ninvade_rem1$location[all_Ninvade_rem1$location == 'edge'] <- 'edge1'
all_Ninvade_rem1$location[all_Ninvade_rem1$location == 'grow'] <- 'grow1'
all_Ninvade_rem1$location[all_Ninvade_rem1$location == 'random'] <- 'random1'


all_Ninvade_rem4 <- all_Ninvade %>% filter(rem == '4')
all_Ninvade_rem4$location[all_Ninvade_rem4$location == 'abund'] <- 'abund4'
all_Ninvade_rem4$location[all_Ninvade_rem4$location == 'down'] <- 'down4'
all_Ninvade_rem4$location[all_Ninvade_rem4$location == 'edge'] <- 'edge4'
all_Ninvade_rem4$location[all_Ninvade_rem4$location == 'grow'] <- 'grow4'
all_Ninvade_rem4$location[all_Ninvade_rem4$location == 'random'] <- 'random4'

all_Ninvade_rem8 <- all_Ninvade %>% filter(rem == '8')
all_Ninvade_rem8$location[all_Ninvade_rem8$location == 'abund'] <- 'abund8'
all_Ninvade_rem8$location[all_Ninvade_rem8$location == 'down'] <- 'down8'
all_Ninvade_rem8$location[all_Ninvade_rem8$location == 'edge'] <- 'edge8'
all_Ninvade_rem8$location[all_Ninvade_rem8$location == 'grow'] <- 'grow8'
all_Ninvade_rem8$location[all_Ninvade_rem8$location == 'random'] <- 'random8'

all_Ninvade_rem16 <- all_Ninvade %>% filter(rem == '16')
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'abund'] <- 'abund16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'down'] <- 'down16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'edge'] <- 'edge16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'grow'] <- 'grow16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'random'] <- 'random16'

all_Ninvade <- rbind(all_Ninvade_rem1,all_Ninvade_rem4, all_Ninvade_rem8 , all_Ninvade_rem16 )

new <- c("1 segment", "4 segments", "8 segments", "16 segments") 
names(new) <-  c("1", "4", "8", "16") 

all_Ninvade$rem2 <- all_Ninvade$rem

###### Plot 1 ####

max.data <- all_Ninvade %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '1') 

c1 <- all_Ninvade %>% 
  filter(rem == '1') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Containment") +  #ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title=element_text(size=14,face="bold")#,
       # axis.text.x = element_text(angle = 310, hjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 4 ####

max.data <- all_Ninvade %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '4') 

c4 <- all_Ninvade %>% 
  filter(rem == '4') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") +  #ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
      #  axis.title=element_text(size=14,face="bold"),
        #axis.text.x = element_text(angle = 310, hjust = 0)
      )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 8 ####

max.data <- all_Ninvade %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '8') 

c8 <- all_Ninvade %>% 
  filter(rem == '8') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
# scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") +  #ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
       # axis.title=element_text(size=14,face="bold"),
       # axis.text.x = element_text(angle = 310, hjust = 0)
       )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 16 ####

max.data <- all_Ninvade %>%
  group_by(location, rem2) %>%
  filter(count == max(count) & rem == '16') 

c16 <- all_Ninvade %>% 
  filter(rem == '16') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") +  #ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
       # axis.title=element_text(size=14,face="bold"),
        # axis.text.x = element_text(angle = 310, hjust = 0)
       )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


cplots <- plot_grid(c1,c4,c8,c16, nrow = 1)

###### Plot different ####

max.data <- all_Ninvade %>%
  group_by(location, rem2) %>%
  filter(count == max(count)) 

call <- all_Ninvade %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Containment")  +  
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"))+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


#cplots <- plot_grid(call, nrow = 1)

#### Entered Columbia ####
file_name = paste(path, 'all_Dcol.csv',sep = '/')
all_Dcol <- fread(file_name)
all_Dcol <- data.frame(all_Dcol)

nocontrol <- all_Dcol %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Dcol <- rbind(all_Dcol, nocontrol)

all_Dcol <- all_Dcol %>% filter(p == 1)

all_Dcol$rem <- as.factor(all_Dcol$rem)

prevent0 <- all_Dcol %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent1 <-all_Dcol %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent4 <-all_Dcol %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent8 <-all_Dcol %>%
  filter(rem == '8') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent16 <-all_Dcol %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

##### Plot ####
nc.val <- all_Dcol %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Dcol <- all_Dcol %>% filter(p == 1)

all_Dcol_rem1 <- all_Dcol %>% filter(rem == '1')
all_Dcol_rem1$location[all_Dcol_rem1$location == 'abund'] <- 'abund1'
all_Dcol_rem1$location[all_Dcol_rem1$location == 'down'] <- 'down1'
all_Dcol_rem1$location[all_Dcol_rem1$location == 'edge'] <- 'edge1'
all_Dcol_rem1$location[all_Dcol_rem1$location == 'grow'] <- 'grow1'
all_Dcol_rem1$location[all_Dcol_rem1$location == 'random'] <- 'random1'

all_Dcol_rem4 <- all_Dcol %>% filter(rem == '4')
all_Dcol_rem4$location[all_Dcol_rem4$location == 'abund'] <- 'abund4'
all_Dcol_rem4$location[all_Dcol_rem4$location == 'down'] <- 'down4'
all_Dcol_rem4$location[all_Dcol_rem4$location == 'edge'] <- 'edge4'
all_Dcol_rem4$location[all_Dcol_rem4$location == 'grow'] <- 'grow4'
all_Dcol_rem4$location[all_Dcol_rem4$location == 'random'] <- 'random4'

all_Dcol_rem8 <- all_Dcol %>% filter(rem == '8')
all_Dcol_rem8$location[all_Dcol_rem8$location == 'abund'] <- 'abund8'
all_Dcol_rem8$location[all_Dcol_rem8$location == 'down'] <- 'down8'
all_Dcol_rem8$location[all_Dcol_rem8$location == 'edge'] <- 'edge8'
all_Dcol_rem8$location[all_Dcol_rem8$location == 'grow'] <- 'grow8'
all_Dcol_rem8$location[all_Dcol_rem8$location == 'random'] <- 'random8'

all_Dcol_rem16 <- all_Dcol %>% filter(rem == '16')
all_Dcol_rem16$location[all_Dcol_rem16$location == 'abund'] <- 'abund16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'down'] <- 'down16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'edge'] <- 'edge16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'grow'] <- 'grow16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'random'] <- 'random16'

all_Dcol <- rbind(all_Dcol_rem1,all_Dcol_rem4, all_Dcol_rem8, all_Dcol_rem16 )
new <- c("1 segment", "4 segments", "8 segments", "16 segments") 
names(new) <-  c("1", "4", "8", "16") 

all_Dcol$rem2 <- all_Dcol$rem

###### Plot 1 ####

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '1') 

p1 <- all_Dcol %>% 
  filter(rem == '1') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund8" = "Abundance",
    "abund16" = "Abundance",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow8" = "Growth", 
    "grow16" = "Growth", 
    "edge1" = "Edges",
    "edge4" = "Edges",
    "edge8" = "Edges",
    "edge16" = "Edges",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down8" = "Downstream",
    "down16" = "Downstream",
    "random1" = "Random",
    "random4" = "Random",
    "random8" = "Random",
    "random16" = "Random"))+
  #scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Prevention") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 4 ####

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '4') 

p4 <- all_Dcol %>% 
  filter(rem == '4') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund8" = "Abundance",
    "abund16" = "Abundance",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow8" = "Growth", 
    "grow16" = "Growth", 
    "edge1" = "Edges",
    "edge4" = "Edges",
    "edge8" = "Edges",
    "edge16" = "Edges",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down8" = "Downstream",
    "down16" = "Downstream",
    "random1" = "Random",
    "random4" = "Random",
    "random8" = "Random",
    "random16" = "Random"))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
       # axis.title=element_text(size=14,face="bold"),
       axis.text.x = element_text(angle = 90, vjust = 0)
       )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 8 ####

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '8') 

p8 <- all_Dcol %>% 
  filter(rem == '8') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund8" = "Abundance",
    "abund16" = "Abundance",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow8" = "Growth", 
    "grow16" = "Growth", 
    "edge1" = "Edges",
    "edge4" = "Edges",
    "edge8" = "Edges",
    "edge16" = "Edges",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down8" = "Downstream",
    "down16" = "Downstream",
    "random1" = "Random",
    "random4" = "Random",
    "random8" = "Random",
    "random16" = "Random"))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
        #axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0)
        )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

###### Plot 16 ####

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '16') 

p16 <- all_Dcol %>% 
  filter(rem == '16') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0)
        )+
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund8" = "Abundance",
    "abund16" = "Abundance",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow8" = "Growth", 
    "grow16" = "Growth", 
    "edge1" = "Edges",
    "edge4" = "Edges",
    "edge8" = "Edges",
    "edge16" = "Edges",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down8" = "Downstream",
    "down16" = "Downstream",
    "random1" = "Random",
    "random4" = "Random",
    "random8" = "Random",
    "random16" = "Random"))+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

pplots <- plot_grid(p1,p4,p8,p16, nrow = 1)

###### Plot different ####

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count)) 

pall <- all_Dcol %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
 # scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Prevention") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"))+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))

#pplots <- plot_grid(pall, nrow = 1)

#### Combine plots ####
library(gridGraphics)

plot_grid(splots, cplots, pplots, 
          labels = c("A", "B", "C"), nrow = 3)


#### TRADE OFFS ####
suppress0$location <- 'No removal' 
suppress0$type <- 'No removal' 
suppress0$rem <- '1'
suppress0b <- suppress0 
suppress0b$rem <- '4'
suppress0c <- suppress0 
suppress0c$rem <- '8'
suppress0d <- suppress0 
suppress0d$rem <- '16'

suppress1$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress1$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress1$rem <- '1'

suppress4$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress4$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress4$rem <- '4'

suppress8$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress8$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress8$rem <- '8'

suppress16$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress16$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
suppress16$rem <- '16'

suppress <- rbind(suppress0,suppress0b,suppress0c, suppress0c, suppress1, suppress4, 
                  suppress8, suppress16)

suppress$objective <- 'Suppress'
colnames(suppress)[2:5] <- c('Expected_Suppress', 'Max_Suppress', "Low_Suppress", "High_Suppress")

contain0$location <- 'No removal' 
contain0$type <- 'No removal' 
contain0$rem <- '1'
contain0b <- contain0 
contain0b$rem <- '4'
contain0c <- contain0 
contain0c$rem <- '8'
contain0d <- contain0 
contain0d$rem <- '16'

contain1$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain1$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain1$rem <- '1'

contain4$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain4$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain4$rem <- '4'

contain8$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain8$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain8$rem <- '8'

contain16$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain16$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
contain16$rem <- '16'

contain <- rbind(contain0,contain0b,contain0c, contain0c, contain1, contain4, 
                  contain8, contain16)

contain$objective <- 'contain'
colnames(contain)[2:5] <- c('Expected_Contain', 'Max_Contain', "Low_Contain", "High_Contain")

prevent0$location <- 'No removal' 
prevent0$type <- 'No removal' 
prevent0$rem <- '1'
prevent0b <- prevent0 
prevent0b$rem <- '4'
prevent0c <- prevent0 
prevent0c$rem <- '8'
prevent0d <- prevent0 
prevent0d$rem <- '16'

prevent1$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent1$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent1$rem <- '1'

prevent4$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent4$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent4$rem <- '4'

prevent8$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent8$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent8$rem <- '8'

prevent16$location <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent16$type <- c('Abundance', 'Downstream', 'Edges', 'Growth', 'Random')
prevent16$rem <- '16'

prevent <- rbind(prevent0,prevent0b,prevent0c, prevent0c, prevent1, prevent4, 
                 prevent8, prevent16)

prevent$objective <- 'prevent'
colnames(prevent)[2:5] <- c('Expected_Prevent', 'Max_Prevent', "Low_Prevent", "High_Prevent")

#### trade off EV ####
Objectives <- cbind(suppress[,1:7], prevent[,2:5], contain[,2:5])
Objectives_1<- Objectives %>% filter(rem == "1")
Objectives_1 <- psel(Objectives_1, low(Expected_Suppress) * low(Expected_Prevent) * low(Expected_Contain)*
                     low(Max_Prevent) * low(Max_Suppress) * low(Max_Contain))
Objectives_1$rem <- '1'
Objectives_1$location

Objectives_4 <- Objectives %>% filter(rem == "4")
Objectives_4 <- psel(Objectives_4, low(Expected_Suppress) * low(Expected_Prevent) * low(Expected_Contain)*
                       low(Max_Prevent) * low(Max_Suppress) * low(Max_Contain))
Objectives_4$rem <- '4'
Objectives_4$location

Objectives_8 <- Objectives %>% filter(rem == "8")
Objectives_8 <- psel(Objectives_8, low(Expected_Suppress) * low(Expected_Prevent) * low(Expected_Contain)*
                       low(Max_Prevent) * low(Max_Suppress) * low(Max_Contain))
Objectives_8$rem <- '8'
Objectives_8$location

Objectives_16 <- Objectives %>% filter(rem == "16")
Objectives_16 <- psel(Objectives_16, low(Expected_Suppress) * low(Expected_Prevent) * low(Expected_Contain)#*
                        #low(Max_Prevent) * low(Max_Suppress) * low(Max_Contain))
)
Objectives_16$rem <- '16'
Objectives_16$location

Objectives_all <- rbind(Objectives_1, Objectives_4, Objectives_8, Objectives_16)

Objectives_all

#### trade off minimax ####
Objectives <- cbind(suppress[,1:7], prevent[,2:5], contain[,2:5])
Objectives_1<- Objectives %>% filter(rem == "1")
Objectives_1 <- psel(Objectives_1, low(Max_Suppress) * low(Max_Prevent) * low(Max_Contain))
Objectives_1$rem <- '1'
Objectives_1$location


Objectives_4 <- Objectives %>% filter(rem == "4")
Objectives_4 <- psel(Objectives_4, low(Max_Suppress) * low(Max_Prevent)* low(Max_Contain))
Objectives_4$rem <- '4'
Objectives_4$location

Objectives_8 <- Objectives %>% filter(rem == "8")
Objectives_8 <- psel(Objectives_8, low(Max_Suppress) * low(Max_Prevent)* low(Max_Contain))
Objectives_8$rem <- '8'
Objectives_8$location

Objectives_16 <- Objectives %>% filter(rem == "16")
Objectives_16 <- psel(Objectives_16, low(Max_Suppress) * low(Max_Prevent)* low(Max_Contain))
Objectives_16$rem <- '16'
Objectives_16$location

Objectives_all <- rbind(Objectives_1, Objectives_4, Objectives_8, Objectives_16)

Objectives_all


#### Dissertation plot ####
splots_D <- plot_grid(s1,s16, nrow = 1)
splots_D

cplots_D <- plot_grid(c1,c16, nrow = 1)
cplots_D

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '1') 

p1 <- all_Dcol %>% 
  filter(rem == '1') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "", 
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  #scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Prevention") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
      #  strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
        axis.title=element_text(size=14,face="bold"),
       # axis.text.x = "none"
  )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count) & rem == '16') 

p16 <- all_Dcol %>% 
  filter(rem == '16') %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location)), color = 'red') +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(labels=c(
    "abund1" = "",
    "abund4" = "",
    "abund8" = "",
    "abund16" = "",
    "grow1" = "",
    "grow4" = "", 
    "grow8" = "", 
    "grow16" = "", 
    "edge1" = "",
    "edge4" = "",
    "edge8" = "",
    "edge16" = "",
    "down1" = "",
    "down4" = "",
    "down8" = "",
    "down16" = "",
    "random1" = "",
    "random4" = "",
    "random8" = "",
    "random16" = ""))+
  #scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     limits = c(0, 6000000))+
  theme_bw() +   
  theme(panel.border = element_blank(),
        strip.background=element_rect(colour="white",
                                      fill="white"),
        #  strip.text.x = element_text(size = 11),
        axis.line = element_line(colour = "gray20", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
      #  axis.title=element_text(size=14,face="bold"),
        # axis.text.x = "none"
  )+
  
  facet_grid(~rem, scales = "free", 
             labeller = 
               labeller(rem = c(`1` = "1 Segment", 
                                `4` = "4 Segments",
                                `8` = "8 Segments",
                                `16`= "16 Segments"
               )))


p16

pplots_D <- plot_grid(p1,p16, nrow = 1)
pplots_D


plot_grid(splots_D, cplots_D, pplots_D, nrow = 1)


