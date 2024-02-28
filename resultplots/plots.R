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

nocontrol <- all_Ntotal %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Ntotal <- rbind(all_Ntotal, nocontrol)

all_Ntotal <- all_Ntotal %>% filter(p == 1 & rem != 8)

level_order <- c("nocontrol", "abund", "down", "edge", "grow", "random")

all_Ntotal$rem <- as.factor(all_Ntotal$rem)

new <- c( "None", "1 segment", "4 segments", "16 segments") 
names(new) <-  c("0", "1", "4", "16") 


col <- brewer.pal(8, "Dark2") 
colors <- c(col[8], col[2], col[1], col[3])
rem.label <- c("None", "1", "4", "16")

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

###### Plot ######
exp.val <- all_Ntotal %>% filter(location == 'abund' & rem == 16)
exp.val <- mean(exp.val$count)

minimax.val <- all_Ntotal %>% filter(location == 'grow' & rem == 16)
minimax.val <- max(minimax.val$count)

nc.val <- all_Ntotal %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ntotal %>% 
ggplot(aes(x = factor(location, level = level_order), y = count, 
           group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Final total crayfish abundance (millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p1 <- all_Ntotal %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
 # annotate("text", x = "nocontrol", y = 85000000, label = "A)", size = 6)+
  xlab("Removal location") + ylab("Final total crayfish abundance (millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none")

##### plot 2 #####
all_Ntotal2 <- all_Ntotal %>% filter(rem != '0' & rem != '1')

level_order2 <- c("abund", "down", "edge", "grow", "random")

new2 <- c( "4 segments", "16 segments") 
names(new2) <-  c("4", "16") 


col2 <- brewer.pal(8, "Dark2") 
colors2 <- c(col2[1], col2[3])
rem.label2 <- c("4", "16")

p1 <- all_Ntotal2 %>% 
  ggplot(aes(x = factor(location, level = level_order2), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="red",
               shape = 18, size = 3,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label2, values = colors2) +
  xlab("") + ylab("Final total crayfish abundance (millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~rem)


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
exp.val <- all_Dcol %>% filter(location == 'down' & rem == 16)
exp.val <- mean(exp.val$count)

minimax.val <- all_Dcol %>% filter(location == 'down' & rem == 16)
minimax.val <- max(minimax.val$count)

nc.val <- all_Dcol %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Dcol %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Total crayfish in the Columbia River")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p2 <- all_Dcol %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Total crayfish in the Columbia River (millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none")

##### plot 2 #####
all_Dcol2 <- all_Dcol %>% filter(rem != '0')

level_order2 <- c("abund", "down", "edge", "grow", "random")

new2 <- c("1 segment", "4 segments", "16 segments") 
names(new2) <-  c("1", "4", "16") 


col2 <- brewer.pal(8, "Dark2") 
colors2 <- c(col2[2], col2[1], col2[3])
rem.label2 <- c("1", "4", "16")


all_Dcol2$rem2 <- all_Dcol2$rem
all_Dcol2$rem2[all_Dcol2$rem2 == '1'] <- '4'

p2 <- all_Dcol2 %>% 
  ggplot(aes(x = factor(location, level = level_order2), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="red",
               shape = 18, size = 3,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label2, values = colors2) +
  xlab("") + ylab("Total crayfish in the Columbia River (Millions)")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~rem2)



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
exp.val <- all_Ninvade %>% filter(location == 'edge' & rem == 16)
exp.val <- mean(exp.val$count)

minimax.val <- all_Ninvade %>% filter(location == 'grow' & rem == 16)
minimax.val <- max(minimax.val$count)

nc.val <- all_Ninvade %>% filter(location == 'nocontrol')
nc.val <- mean(nc.val$count)

all_Ninvade %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p3 <- all_Ninvade %>% 
  ggplot(aes(x = factor(location, level = level_order), y = count, 
             group = interaction(rem, location)))+
  geom_violin(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = exp.val, linetype = 2, color = 'chartreuse3', linewidth = 1) + 
  geom_hline(yintercept = minimax.val, linetype = 2, color = 'blue3', linewidth = 1) + 
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="black",
               shape = 19, size = 2,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label, values = colors) +
  xlab("Removal location") + ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"))+
  theme(panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right")

##### plot 2 #####
all_Ninvade2 <- all_Ninvade %>% filter(rem != '0' & rem != '1')

level_order2 <- c("abund", "down", "edge", "grow", "random")

new2 <- c( "4 segments", "16 segments") 
names(new2) <-  c("4", "16") 


col2 <- brewer.pal(8, "Dark2") 
colors2 <- c(col2[1], col2[3])
rem.label2 <- c("4", "16")


p3 <- all_Ninvade2 %>% 
  ggplot(aes(x = factor(location, level = level_order2), y = count, 
             group = interaction(rem, location)))+
  geom_boxplot(aes( fill = as.factor(rem)), position="dodge",alpha = 0.5)+
  geom_hline(yintercept = nc.val, linetype = 2) + 
  stat_summary(fun.y=mean,
               geom="point", color="red",
               shape = 18, size = 3,
               position = position_dodge(width = 0.9))+
  scale_x_discrete(labels=c("nocontrol" = "No removal", "abund" = "Abundance",
                            "down" = "Downstream", "edge" = "Edge",
                            "grow" = "Growth", "random" = "Random"))+
  scale_fill_manual(name = "Segments removed", labels = rem.label2, values = colors2) +
  xlab("") + ylab("Percent invaded")+
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~rem)


#### Combine plots ####
library(gridGraphics)

legend <- get_legend(
  p2 + 
    guides(color = guide_legend(title.position = "right", nrow = 1)) +
    theme(legend.position = "right",
          legend.key.size = unit(1.5, 'cm'))
)

plot_grid(
  plot_grid(p1, p2, nrow = 1, ncol = 2,
            labels = c("A", "B")),
  plot_grid(NULL, p3, legend, nrow = 1, rel_widths = c(0.25, 0.5, 0.25), labels = c("", "C", "")),
  nrow = 2
)


# plot_grid(
#   plot_grid(p1, p2, nrow = 1, ncol = 2,
#             labels = c("A", "B")),
#   plot_grid(p3, legend, nrow = 1, rel_widths = c(0.25, 0.25), labels = c("C", "")),
#   nrow = 2
# )




#### TRADE OFFS ####
suppress1$location <- 'No removal' 
suppress2$location <- 'Downstream, 1'
suppress3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Grow, 4', 'Random, 4')
suppress4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Grow, 16', 'Random, 16')

suppress <- rbind(suppress1, suppress2, suppress3, suppress4)
suppress$objective <- 'Suppress'
colnames(suppress)[2:5] <- c('Expected_Suppress', 'Max_Suppress', "Low_Suppress", "High_Suppress")

prevent1$location <- 'No removal' 
prevent2$location <- 'Downstream, 1'
prevent3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Grow, 4', 'Random, 4')
prevent4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Grow, 16', 'Random, 16')

prevent <- rbind(prevent1, prevent2, prevent3, prevent4)
prevent$objective <- 'Prevent'
colnames(prevent)[2:5] <- c('Expected_Prevent', 'Max_Prevent', "Low_Prevent", "High_Prevent")

contain1$location <- 'No removal' 
contain2$location <- 'Downstream, 1'
contain3$location <- c('Abundance, 4', 'Downstream, 4', 'Edge, 4', 'Grow, 4', 'Random, 4')
contain4$location <- c('Abundance, 16', 'Downstream, 16', 'Edge, 16', 'Grow, 16', 'Random, 16')

contain <- rbind(contain1, contain2, contain3, contain4)
contain$objective <- 'Contain'
colnames(contain)[2:5] <- c('Expected_Contain', 'Max_Contain', "Low_Contain", "High_Contain")

#### trade off 1 ####
Objectives <- cbind(suppress[,1:5], prevent[,2:5], contain[,2:5])
Objectives_svsp <- psel(Objectives, low(Expected_Suppress) * low(Expected_Prevent))

level_order <- c("No removal", "Abundance, 4","Abundance, 16", "Downstream, 1","Downstream, 4","Downstream, 16",
                 "Edge, 4", "Edge, 16", "Grow, 4", "Grow, 16","Random, 4", "Random, 16")

colors_all <-  brewer.pal(12, "Paired")
colors_all <- c("darkgrey", colors_all[1:4], "darkgreen", colors_all[5:10])
  
Objectives$location <- factor(Objectives$location, levels = level_order)

t1 <- ggplot(Objectives,aes(x = Expected_Suppress, y = Expected_Prevent))+
  geom_point(size = 2,aes(color = location))+
  geom_errorbarh(aes(xmin = Low_Suppress,xmax = High_Suppress, color = location)) + 
  geom_errorbar(aes(ymin = Low_Prevent,ymax = High_Prevent, color = location))+
  scale_colour_manual(name = "Alternative", values = colors_all) +
  geom_point(data = Objectives_svsp, size = 3) +
  geom_text(data=Objectives_svsp, aes(x = Expected_Suppress- 8000000, y = Expected_Prevent+ 50000,
                                      label=Objectives_svsp$location), 
            color="black", 
            size=3 , fontface="bold" )+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  xlab("Suppress")+ ylab("Prevent")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")


#### trade off 2 ####
Objectives_svsc <- psel(Objectives, low(Expected_Suppress) * low(Expected_Contain))

Objectives_svsc$Expected_Suppress2 <- Objectives_svsc$Expected_Suppress
Objectives_svsc$Expected_Suppress2[2] <- Objectives_svsc$Expected_Suppress2[2]+2000000
Objectives_svsc$Expected_Contain2 <- Objectives_svsc$Expected_Contain
Objectives_svsc$Expected_Contain2[2] <- Objectives_svsc$Expected_Contain2[2] - 0.01

t2 <- ggplot(Objectives,aes(x = Expected_Suppress, y = Expected_Contain))+
  geom_point(size = 2,aes(color = location))+
  geom_errorbarh(aes(xmin = Low_Suppress,xmax = High_Suppress, color = location)) + 
  geom_errorbar(aes(ymin = Low_Contain,ymax = High_Contain, color = location))+
  scale_colour_manual(name = "Alternative", values = colors_all) +
  geom_point(data = Objectives_svsc, size = 3) +
  geom_text_repel(data=Objectives_svsc, aes(x = Expected_Suppress2+ 1000000, y = Expected_Contain2+ .005,
                                      label=Objectives_svsc$location), 
            color="black", 
            size=3 , fontface="bold" )+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_y_continuous(labels=scales::percent) +
  xlab("Suppress")+ ylab("Contain")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")




#### Trade off 3 ####
Objectives_pvsc <- psel(Objectives, low(Expected_Prevent) * low(Expected_Contain))

t3 <- ggplot(Objectives,aes(x = Expected_Prevent, y = Expected_Contain))+
  geom_point(size = 2,aes(color = location))+
  geom_errorbarh(aes(xmin = Low_Prevent,xmax = High_Prevent, color = location)) + 
  geom_errorbar(aes(ymin = Low_Contain,ymax = High_Contain, color = location))+
  scale_colour_manual(name = "Alternative", values = colors_all) +
  geom_point(data = Objectives_pvsc, size = 3) +
  geom_text_repel(data=Objectives_pvsc, aes(x = Expected_Prevent+ 100000, y = Expected_Contain+ 0.01,
                                      label=Objectives_pvsc$location), 
            color="black", 
            size=3 , fontface="bold" )+
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  xlab("Prevent")+ ylab("Contain")+
  theme_bw() +   
  theme(strip.background=element_rect(colour="white",
                                      fill="white"),
        strip.text.x = element_blank(),
        panel.border = element_rect(colour = "gray", size = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) #,legend.position = "none")


legend2 <- get_legend(
  t2 + 
    guides(color = guide_legend(title.position = "right", nrow = 1)) +
    theme(
          legend.key.size = unit(1.5, 'cm'))
)


plot_grid(
  plot_grid(t1, t2, nrow = 1, ncol = 2,
            labels = c("A", "B")),
  plot_grid(NULL, t3, legend2, nrow = 1, rel_widths = c(0.25, 0.5, 0.25), labels = c("", "C", "")),
  nrow = 2
)


plot_grid(t1, t2,t3, nrow = 2, ncol = 2, labels = c("A", "B", "C"))

plot_grid(
  plot_grid(t1, t2, nrow = 1, ncol = 2,
            labels = c("A", "B")),
  plot_grid(NULL, t3, nrow = 1, rel_widths = c(0.15, 0.35), labels = c("", "C")),
  nrow = 2
)
