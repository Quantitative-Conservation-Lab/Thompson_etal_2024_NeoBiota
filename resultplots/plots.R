library(plyr)
library(tidyverse)
library(here)
library(data.table)
library(scales)
library(RColorBrewer) 
library("cowplot")
library(rPref)
library(ggrepel)

path <- 'D:\\Chapter2\\results'

low <- 0.1
high <- 0.9

#### Final total N ####
file_name = paste(path, 'all_Ntotal.csv',sep = '/')
all_Ntotal <- fread(file_name)
all_Ntotal <- data.frame(all_Ntotal)

all_Ntotal$rem <- as.factor(all_Ntotal$rem)


suppress1 <- all_Ntotal %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress2 <-all_Ntotal %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress3 <-all_Ntotal %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

suppress4 <-all_Ntotal %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

##### plot #####
nc.val <- all_Ntotal %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ntotal <- all_Ntotal %>% filter(p == 1 & rem != '8')

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

all_Ntotal_rem16 <- all_Ntotal %>% filter(rem == '16')
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'abund'] <- 'abund16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'down'] <- 'down16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'edge'] <- 'edge16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'grow'] <- 'grow16'
all_Ntotal_rem16$location[all_Ntotal_rem16$location == 'random'] <- 'random16'

all_Ntotal <- rbind(all_Ntotal_rem1,all_Ntotal_rem4, all_Ntotal_rem16 )
level_order <- c("abund1", "abund4","abund16", "down1", "down4","down16", "edge1", "edge4","edge16",
                 "grow1","grow4","grow16", "random1", "random4", "random16")
new <- c("1 segment", "4 segments", "16 segments") 
names(new) <-  c("1", "4", "16") 


col <- brewer.pal(8, "Dark2") 
colors2 <- c(col[2], col[1], col[3])
rem.label <- c("1", "4", "16")


all_Ntotal$rem2 <- all_Ntotal$rem
# all_Ntotal$rem2[all_Ntotal$rem2 == '1'] <- '4'

max.data <- all_Ntotal %>%
  group_by(location, rem) %>%
  filter(count == max(count)) 

p1 <- all_Ntotal %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5, outlier.shape = NA)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location))) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(drop = TRUE) +
  scale_x_discrete(labels=c(
                            "abund1" = "Abundance",
                            "abund4" = "Abundance",
                            "abund16" = "Abundance",
                            "down1" = "Downstream",
                            "down4" = "Downstream",
                            "down16" = "Downstream",
                            "edge1" = "Edge",
                            "edge4" = "Edge",
                            "edge16" = "Edge",
                            "grow1" = "Growth", 
                            "grow4" = "Growth", 
                            "grow16" = "Growth",
                            "random1" = "Random",
                            "random4" = "Random",
                            "random16" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Suppression")+ #Final total crayfish abundance (millions)
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 330, hjust = 0))+
  facet_grid(~rem2, scales = "free")


#### Entered Columbia ####
file_name = paste(path, 'all_Dcol.csv',sep = '/')
all_Dcol <- fread(file_name)
all_Dcol <- data.frame(all_Dcol)

nocontrol <- all_Dcol %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Dcol <- rbind(all_Dcol, nocontrol)

all_Dcol <- all_Dcol %>% filter(p == 1 & rem != 8)

all_Dcol$rem <- as.factor(all_Dcol$rem)

prevent1 <- all_Dcol %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent2 <-all_Dcol %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent3 <-all_Dcol %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

prevent4 <-all_Dcol %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

##### Plot ####
nc.val <- all_Dcol %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Dcol <- all_Dcol %>% filter(p == 1 & rem != '8')

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

all_Dcol_rem16 <- all_Dcol %>% filter(rem == '16')
all_Dcol_rem16$location[all_Dcol_rem16$location == 'abund'] <- 'abund16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'down'] <- 'down16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'edge'] <- 'edge16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'grow'] <- 'grow16'
all_Dcol_rem16$location[all_Dcol_rem16$location == 'random'] <- 'random16'

all_Dcol <- rbind(all_Dcol_rem1,all_Dcol_rem4, all_Dcol_rem16 )
new <- c("1 segment", "4 segments", "16 segments") 
names(new) <-  c("1", "4", "16") 


col <- brewer.pal(8, "Dark2") 
colors2 <- c(col[2], col[1], col[3])
rem.label <- c("1", "4", "16")


all_Dcol$rem2 <- all_Dcol$rem
#all_Dcol$rem2[all_Dcol$rem2 == '1'] <- '4'

max.data <- all_Dcol %>%
  group_by(location, rem) %>%
  filter(count == max(count)) 

p2 <- all_Dcol %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5,outlier.shape = NA)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location))) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(drop = TRUE) +
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund16" = "Abundance",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down16" = "Downstream",
    "edge1" = "Edge",
    "edge4" = "Edge",
    "edge16" = "Edge",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow16" = "Growth",
    "random1" = "Random",
    "random4" = "Random",
    "random16" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Prevention") + #ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 315, hjust = -0.1))+
  facet_grid(~rem2, scales = "free")



#### Invaded ####
file_name = paste(path, 'all_Ninvade.csv',sep = '/')
all_Ninvade <- fread(file_name)
all_Ninvade <- data.frame(all_Ninvade)

nocontrol <- all_Ninvade %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Ninvade <- rbind(all_Ninvade, nocontrol)

all_Ninvade <- all_Ninvade %>% filter(p == 1 & rem != 8)

all_Ninvade$rem <- as.factor(all_Ninvade$rem)

all_Ninvade$count <- all_Ninvade$invasion/ 35

contain1 <- all_Ninvade %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain2 <-all_Ninvade %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))

contain3 <- all_Ninvade %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))
contain4 <-all_Ninvade %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count),
            lower = quantile(count, low),
            upper = quantile(count, high))



##### Plot ####
nc.val <- all_Ninvade %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ninvade <- all_Ninvade %>% filter(p == 1 & rem != '8')

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

all_Ninvade_rem16 <- all_Ninvade %>% filter(rem == '16')
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'abund'] <- 'abund16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'down'] <- 'down16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'edge'] <- 'edge16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'grow'] <- 'grow16'
all_Ninvade_rem16$location[all_Ninvade_rem16$location == 'random'] <- 'random16'

all_Ninvade <- rbind(all_Ninvade_rem1,all_Ninvade_rem4, all_Ninvade_rem16 )

new <- c("1 segment", "4 segments", "16 segments") 
names(new) <-  c("1", "4", "16") 


col <- brewer.pal(8, "Dark2") 
colors2 <- c(col[2], col[1], col[3])
rem.label <- c("1", "4", "16")


all_Ninvade$rem2 <- all_Ninvade$rem
#all_Ninvade$rem2[all_Ninvade$rem2 == '1'] <- '4'

max.data <- all_Ninvade %>%
  group_by(location, rem) %>%
  filter(count == max(count)) 

p3 <- all_Ninvade %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5,outlier.shape = NA)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  geom_point(data = max.data, aes(x = factor(location, level = level_order), y = count, 
                                  group = interaction(rem, location))) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, color = "red", linewidth = 1)+ 
  scale_x_discrete(drop = TRUE) +
  scale_x_discrete(labels=c(
    "abund1" = "Abundance",
    "abund4" = "Abundance",
    "abund16" = "Abundance",
    "down1" = "Downstream",
    "down4" = "Downstream",
    "down16" = "Downstream",
    "edge1" = "Edge",
    "edge4" = "Edge",
    "edge16" = "Edge",
    "grow1" = "Growth", 
    "grow4" = "Growth", 
    "grow16" = "Growth",
    "random1" = "Random",
    "random4" = "Random",
    "random16" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors2) +
  xlab("") + ylab("Containment") +  #ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(angle = 315, hjust = -0.1))+
  facet_grid(~rem2, scales = "free")



#### Combine plots ####
library(gridGraphics)

legend <- get_legend(
  p2 + 
    guides(color = guide_legend(title.position = "right", nrow = 1)) +
    theme(legend.position = "right",
          legend.key.size = unit(0.8, 'cm'))
)


plot_grid(
  plot_grid(p1, NULL, 
            nrow = 1, rel_widths = c(0.35, 0.2), 
            labels = c("A", "")),
  
  plot_grid(p2, legend, 
            nrow = 1, rel_widths = c(0.35, 0.2), 
            labels = c("B", "")),
  
  plot_grid(p3, NULL, 
            nrow = 1, rel_widths = c(0.35, 0.2), 
            labels = c("C", "")), 
  nrow = 3
)


#### TRADE OFFS ####
suppress1$location <- 'No removal' 
suppress1$type <- 'No removal' 
suppress1$rem <- '1'
suppress1b <- suppress1 
suppress1b$rem <- '4'
suppress1c <- suppress1 
suppress1c$rem <- '16'

suppress2$location <- c('Abundance, 1', 'Downstream, 1', 'Edge, 1', 'Growth, 1', 'Random, 1')
suppress2$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
suppress2$rem <- '1'
suppress3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Growth, 4', 'Random, 4')
suppress3$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
suppress3$rem <- '4'
suppress4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Growth, 16', 'Random, 16')
suppress4$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
suppress4$rem <- '16'

suppress <- rbind(suppress1,suppress1b,suppress1b, suppress2, suppress3, suppress4)
suppress$objective <- 'Suppress'
colnames(suppress)[2:5] <- c('Expected_Suppress', 'Max_Suppress', "Low_Suppress", "High_Suppress")

prevent1$location <- 'No removal' 
prevent1$type <- 'No removal' 
prevent1$rem <- '1'
prevent1b <- prevent1 
prevent1b$rem <- '4'
prevent1c <- prevent1 
prevent1c$rem <- '16'

prevent2$location <- c('Abundance, 1', 'Downstream, 1', 'Edge, 1', 'Growth, 1', 'Random, 1')
prevent2$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
prevent2$rem <- '1'
prevent3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Growth, 4', 'Random, 4')
prevent3$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
prevent3$rem <- '4'
prevent4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Growth, 16', 'Random, 16')
prevent4$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
prevent4$rem <- '16'

prevent <- rbind(prevent1,prevent1b, prevent1c, prevent2, prevent3, prevent4)
prevent$objective <- 'Prevent'
colnames(prevent)[2:5] <- c('Expected_Prevent', 'Max_Prevent', "Low_Prevent", "High_Prevent")

contain1$location <- 'No removal' 
contain1$type <- 'No removal' 
contain1$rem <- '1'
contain1b <- contain1
contain1b$rem <- '4'
contain1c <- contain1
contain1c$rem <- '16'

contain2$location <- c('Abundance, 1', 'Downstream, 1', 'Edge, 1', 'Growth, 1', 'Random, 1')
contain2$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
contain2$rem <- '1'
contain3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Growth, 4', 'Random, 4')
contain3$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
contain3$rem <- '4'
contain4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Growth, 16', 'Random, 16')
contain4$type <- c('Abundance', 'Downstream', 'Edge', 'Growth', 'Random')
contain4$rem <- '16'

contain <- rbind(contain1,contain1b,contain1c, contain2, contain3, contain4)
contain$objective <- 'Contain'
colnames(contain)[2:5] <- c('Expected_Contain', 'Max_Contain', "Low_Contain", "High_Contain")

#### trade off 1 ####
Objectives <- cbind(suppress[,1:7], prevent[,2:5], contain[,2:5])
Objectives_svsp1 <- Objectives %>% filter(rem == "1")
Objectives_svsp1 <- psel(Objectives_svsp1, low(Expected_Suppress) * low(Expected_Prevent))
Objectives_svsp1$rem <- '1'

Objectives_svsp2 <- Objectives %>% filter(rem == "4")
Objectives_svsp2 <- psel(Objectives_svsp2, low(Expected_Suppress) * low(Expected_Prevent))
Objectives_svsp2$rem <- '4'

Objectives_svsp3 <- Objectives %>% filter(rem == "16")
Objectives_svsp3 <- psel(Objectives_svsp3, low(Expected_Suppress) * low(Expected_Prevent))
Objectives_svsp3$rem <- '16'

Objectives_svsp <- rbind(Objectives_svsp1, Objectives_svsp2, Objectives_svsp3)

colors_all <-  brewer.pal(12, "Dark2")
colors_all <- c("darkgrey", colors_all[1:4], colors_all[6])

level_order <- c('No removal', "Abundance", "Downstream", "Edge", "Growth", "Random")  
Objectives$type<- factor(Objectives$type, levels = level_order)

t1 <- 
  ggplot(Objectives,aes(x = Expected_Suppress, y = Expected_Prevent))+
  geom_point(size = 2,aes(color = type))+
  geom_errorbarh(aes(xmin = Low_Suppress,xmax = High_Suppress, color = type)) + 
  geom_errorbar(aes(ymin = Low_Prevent,ymax = High_Prevent, color = type))+
  scale_colour_manual(name = "Management Strategy", values = colors_all) +
  geom_point(data = Objectives_svsp, size = 2) +
  geom_text_repel(data=Objectives_svsp, aes(x = Expected_Suppress, y = Expected_Prevent,
                                      label=Objectives_svsp$location), 
            color="black", 
            size=3 , fontface="bold",
            nudge_x = 100000, 
            nudge_y = 100000)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  xlab("Suppression")+ ylab("Prevention")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       legend.position = "none"
        )+
  facet_wrap(~as.numeric(rem))

#### trade off 2 ####
Objectives_svsc1 <- Objectives %>% filter(rem == "1")
Objectives_svsc1 <- psel(Objectives_svsc1, low(Expected_Suppress) * low(Expected_Contain))
Objectives_svsc1$rem <- '1'

Objectives_svsc2 <- Objectives %>% filter(rem == "4")
Objectives_svsc2 <- psel(Objectives_svsc2, low(Expected_Suppress) * low(Expected_Contain))
Objectives_svsc2$rem <- '4'

Objectives_svsc3 <- Objectives %>% filter(rem == "16")
Objectives_svsc3 <- psel(Objectives_svsc3, low(Expected_Suppress) * low(Expected_Contain))
Objectives_svsc3$rem <- '16'

Objectives_svsc <- rbind(Objectives_svsc1, Objectives_svsc2,Objectives_svsc3)

t2 <- 
  ggplot(Objectives,aes(x = Expected_Suppress, y = Expected_Contain))+
  geom_point(size = 2,aes(color = type))+
  geom_errorbarh(aes(xmin = Low_Suppress,xmax = High_Suppress, color = type)) + 
  geom_errorbar(aes(ymin = Low_Contain,ymax = High_Contain, color = type))+
  scale_colour_manual(name = "Alternative", values = colors_all) +
  geom_point(data = Objectives_svsc, size = 2) +
  geom_text_repel(data=Objectives_svsc, aes(x = Expected_Suppress, y = Expected_Contain + 0.01,
                                            label=Objectives_svsc$location), 
                  nudge_y = 0,
                  color="black", 
                  nudge_x = 100000,
                  size=3 , fontface="bold" )+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_y_continuous(labels=scales::percent) +
  xlab("Suppression")+ ylab("Containment")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")+
  facet_wrap(~as.numeric(rem))


t4 <- 
  ggplot(Objectives,aes(x = Expected_Suppress, y = Expected_Contain))+
  geom_point(size = 2,aes(color = type))+
  geom_errorbarh(aes(xmin = Low_Suppress,xmax = High_Suppress, color = type)) + 
  geom_errorbar(aes(ymin = Low_Contain,ymax = High_Contain, color = type))+
  scale_colour_manual(name = "Management Strategy", values = colors_all) +
  geom_point(data = Objectives_svsc, size = 3) +
  geom_text_repel(data=Objectives_svsc, aes(x = Expected_Suppress, y = Expected_Contain + 0.01,
                                            label=Objectives_svsc$location), 
                  nudge_y = -.002,
                  color="black", 
                  nudge_x = 100000,
                  size=3 , fontface="bold" )+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_y_continuous(labels=scales::percent) +
  xlab("Suppression")+ ylab("Containment")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~as.numeric(rem))

#### Trade off 3 ####
Objectives_pvsc1 <- Objectives %>% filter(rem == "1")
Objectives_pvsc1 <- psel(Objectives_pvsc1, low(Expected_Prevent) * low(Expected_Contain))
Objectives_pvsc1$rem <- '1'
  
Objectives_pvsc2 <- Objectives %>% filter(rem == "4")
Objectives_pvsc2 <- psel(Objectives_pvsc2, low(Expected_Prevent) * low(Expected_Contain))
Objectives_pvsc2$rem <- '4'
  
Objectives_pvsc3 <- Objectives %>% filter(rem == "16")
Objectives_pvsc3 <- psel(Objectives_pvsc3, low(Expected_Prevent) * low(Expected_Contain))
Objectives_pvsc3$rem <- '16'

Objectives_pvsc <- rbind(Objectives_pvsc1, Objectives_pvsc2, Objectives_pvsc3)  
  
  
t3 <- ggplot(Objectives,aes(x = Expected_Prevent, y = Expected_Contain))+
  geom_point(size = 2,aes(color = type))+
  geom_errorbarh(aes(xmin = Low_Prevent,xmax = High_Prevent, color = type)) + 
  geom_errorbar(aes(ymin = Low_Contain,ymax = High_Contain, color = type))+
  scale_colour_manual(name = "Alternative", values = colors_all) +
  geom_point(data = Objectives_pvsc, size = 2) +
  geom_text_repel(data=Objectives_pvsc, aes(x = Expected_Prevent, y = Expected_Contain+ 0.01,
                                      label=Objectives_pvsc$location), 
            color="black", 
            size=3 , fontface="bold" )+
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  xlab("Prevention")+ ylab("Containment")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "none")+
  facet_wrap(~as.numeric(rem))



legend2<- get_legend(
  t4 
)

plot_grid(
  plot_grid(t1, NULL,
           nrow = 1, rel_widths = c(0.35, 0.1),
          labels = c("A", "")),#)#,

  plot_grid(t2, legend2,
            nrow = 1, rel_widths = c(0.35, 0.1),
            labels = c("B", "")),#) #,

  plot_grid(t3, NULL,
            nrow = 1, rel_widths = c(0.35, 0.1),
            labels = c("C", "")),
  nrow = 3
)

ggsave2(
  filename = 'paretoplot.png',
  plot = ggplot2::last_plot(),
  scale = 1,
  width = 7,
  height = 10,
  units = "in",
  dpi = 800
)
