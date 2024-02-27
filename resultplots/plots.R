library(plyr)
library(tidyverse)
library(here)
library(data.table)
library(scales)
library(RColorBrewer) 
library("cowplot")

path <- 'D:\\Chapter2\\results'

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

all_Ntotal %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ntotal %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ntotal %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ntotal %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

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


#### Entered Columbia ####
file_name = paste(path, 'all_Dcol.csv',sep = '/')
all_Dcol <- fread(file_name)
all_Dcol <- data.frame(all_Dcol)

nocontrol <- all_Dcol %>% filter(location == 'nocontrol')
nocontrol$p <- 1
all_Dcol <- rbind(all_Dcol, nocontrol)

all_Dcol <- all_Dcol %>% filter(p == 1 & rem != 8)

all_Dcol$rem <- as.factor(all_Dcol$rem)

all_Dcol %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Dcol %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Dcol %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Dcol %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

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

all_Ninvade %>%
  filter(rem == '0') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ninvade %>%
  filter(rem == '1') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ninvade %>%
  filter(rem == '4') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

all_Ninvade %>%
  filter(rem == '16') %>% 
  group_by(location) %>%
  summarise(mean_c = mean(count),
            max_c = max(count))

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

#### Combine plots ####
library(gridGraphics)

legend <- get_legend(
  p1 + 
    guides(color = guide_legend(title.position = "right", nrow = 1)) +
    theme(legend.position = "right",
          legend.key.size = unit(1, 'cm'))
)

plot_grid(
  plot_grid(p1, p2, nrow = 1, ncol = 2,
            labels = c("A", "B")),
  plot_grid(NULL, p3, NULL, nrow = 1, rel_widths = c(0.25, 1, 0.25), labels = c("", "C", "")),
  nrow = 2
)

# plot_grid(
#   p1,p2,p3, legend,
#   labels = c("A", "B", "C", " "),
#   ncol = 3,
#   rel_widths = c(1, 1, 1,1)
# )
