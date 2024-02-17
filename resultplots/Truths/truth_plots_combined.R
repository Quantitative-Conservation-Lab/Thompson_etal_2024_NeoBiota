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

#### Color themes ####
truth_colors <- RColorBrewer::brewer.pal(11, "Paired")[c(1,3,9,12)]
est_colors <- RColorBrewer::brewer.pal(11, "Paired")[c(2,4,10,12)]

all_colors <- RColorBrewer::brewer.pal(11, "Paired")[c(1,2,3,4,9,10,12)]

labels_truth <- c("Abundant (truth)", "Edge (truth)", "Growth (truth)", "Random")

labels_all <- c("Abundant (truth)", "Abundant", 
                "Edge (truth)", "Edge", 
                "Growth (truth)", "Growth",
                "Random")


###
path <- here::here("results", "Truth_alternatives")

#### True abund data: ####  
path2 <- "abund_truth"
file_name = paste(path, path2, 'DR_abund_N_plot.csv',sep = '/')
N.plots.abunds <- fread(file_name)
N.plots.abunds <- N.plots.abunds[-1]
N.plots.abunds$DR <- "Abundant"

file_name = paste(path, path2, 'DR_abund_minabund.csv',sep = '/')
abunds.minabund <- fread(file_name)
abunds.minabund <- abunds.minabund[-1]
abunds.minabund$DR <- "Abundant"

file_name = paste(path, path2, 'DR_abund_minspatial.csv',sep = '/')
abunds.minspace <- fread(file_name)
abunds.minspace <- abunds.minspace[-1]
abunds.minspace$DR <- "Abundant"

#### True grow data: ####  
path2 <- "grow_truth"
file_name = paste(path, path2, 'DR_grow_N_plot.csv',sep = '/')
N.plots.grows <- fread(file_name)
N.plots.grows <- N.plots.grows[-1]
N.plots.grows$DR <- "Growth"

file_name = paste(path, path2, 'DR_grow_minabund.csv',sep = '/')
grows.minabund <- fread(file_name)
grows.minabund <- grows.minabund[-1]
grows.minabund$DR <- "Growth"

file_name = paste(path, path2, 'DR_grow_minspatial.csv',sep = '/')
grows.minspace <- fread(file_name)
grows.minspace <- grows.minspace[-1]
grows.minspace$DR <- "Growth"

#### True edge data: ####  
path2 <- "edge_truth"
file_name = paste(path, path2, 'DR_edge_N_plot.csv',sep = '/')
N.plots.edges <- fread(file_name)
N.plots.edges <- N.plots.edges[-1]
N.plots.edges$DR <- "Edge"

file_name = paste(path, path2, 'DR_edge_minabund.csv',sep = '/')
edges.minabund <- fread(file_name)
edges.minabund <- edges.minabund[-1]
edges.minabund$DR <- "Edge"

file_name = paste(path, path2, 'DR_edge_minspatial.csv',sep = '/')
edges.minspace <- fread(file_name)
edges.minspace <- edges.minspace[-1]
edges.minspace$DR <- "Edge"

#### True rand data: ####  
path <- here::here("results")
path2 <- "trulyrandom"
file_name = paste(path, path2, 'DR_rand_N_plot.csv',sep = '/')
N.plots.rands <- fread(file_name)
N.plots.rands <- N.plots.rands[-1]
N.plots.rands$DR <- "Random"

file_name = paste(path, path2, 'DR_rand_minabund.csv',sep = '/')
rands.minabund <- fread(file_name)
rands.minabund <- rands.minabund[-1]
rands.minabund$DR <- "Random"

file_name = paste(path, path2, 'DR_rand_minspatial.csv',sep = '/')
rands.minspace <- fread(file_name)
rands.minspace <- rands.minspace[-1]
rands.minspace$DR <- "Random"

#### No Control data: ####  
path2 <- "nocontrol"
file_name = paste(path, path2, 'DR_nocontrol_N_plot.csv',sep = '/')
N.plots.nocontrols<- fread(file_name)
N.plots.nocontrols <- N.plots.nocontrols[-1]
N.plots.nocontrols$DR <- "No Control"

file_name = paste(path, path2, 'DR_nocontrol_minabund.csv',sep = '/')
nocontrols.minabund <- fread(file_name)
nocontrols.minabund <- nocontrols.minabund[-1]
nocontrols.minabund$DR <- "No Control"

file_name = paste(path, path2, 'DR_nocontrol_minspatial.csv',sep = '/')
nocontrols.minspace <- fread(file_name)
nocontrols.minspace <- nocontrols.minspace[-1]
nocontrols.minspace$DR <- "No Control"

#### Plots through time ####
N.plots <- rbind(N.plots.abunds, N.plots.edges, N.plots.grows, N.plots.rands)


labels <- c("Abundant" = "Abundant (truth)", 
            "Edge" = "Edge (truth)", 
            "Growth" = "Growth (truth)",
            "Random" = "Random")

ggplot(N.plots, aes(x = year)) +
  geom_line(mapping = aes(y = mean), color = "black", lwd = 1)+ 
  ylab("Abundance (Millions of Crayfish)") + xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = DR), alpha = .5)+
  scale_fill_manual(values = truth_colors, labels = labels_truth)+
  scale_y_continuous(limits = c(0,220000000),labels = scales::label_number_si())+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))+
  guides(fill=guide_legend(title="Alternative"))+
  facet_wrap(~DR, labeller = labeller(DR = labels))


##### No control plot time #####
ggplot(N.plots.nocontrols, aes(x = year)) +
  geom_line(mapping = aes(y = mean), color = "black", lwd = 1)+ 
  ylab("Abundance (Millions of Crayfish)") + xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),fill = "goldenrod", alpha = .5)+
  scale_y_continuous(limits = c(0,220000000),labels = scales::label_number_si())+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))+
  facet_wrap(~DR)



#### Minimize abundance ####
Min.abund <- rbind(abunds.minabund, edges.minabund, grows.minabund, rands.minabund, nocontrols.minabund)

#ROUNDED TO NEAREST HUNDRED THOUSDAND 
round_to <- function(x, to = 10) round(x/to)*to

mean_txt <- function(x){
  return(data.frame(y=mean(x),
                    label = format(round_to(mean(x,na.rm=T), 100000),big.mark=",",scientific=FALSE)
                    
  ))}

max_txt <- function(x){
  return(data.frame(y=mean(x),
                    label = format(round_to(max(x,na.rm=T), 100000),big.mark=",",scientific=FALSE)
                    
  ))}

median_txt <- function(x){
  return(data.frame(y=median(x),
                    label = format(round_to(median(x,na.rm=T), 100000),big.mark=",",scientific=FALSE)
                    
  ))}

min_txt <- function(x){
  return(data.frame(y=median(x),
                    label = format(round_to(min(x,na.rm=T), 1),big.mark=",",scientific=FALSE)
                    
  ))}

ggplot(Min.abund, aes(x=DR, y=count)) + 
  geom_boxplot(aes(group = DR))+
  stat_summary(fun="mean",color="red", shape=16)+
  stat_summary(fun.data = mean_txt, geom="text", color="red",vjust=1.3)+
  stat_summary(fun="max",color="blue", shape=8)+
  stat_summary(fun.data = median_txt, geom="text", color="black",vjust=-.3)+
  stat_summary(fun.data = max_txt, geom="text", color="blue",vjust=-33)+
  ylab("Final Crayfish Abundance (Millions)")+ xlab("Alternative")+
  scale_y_continuous(labels = scales::label_number_si())


nocontrols <- rep(nocontrols.minabund$count, 4 )

Min.abund.comparison <- filter(Min.abund, DR != "No Control")
Min.abund.comparisonA <- Min.abund.comparison 
Min.abund.comparisonA$count <-  nocontrols- Min.abund.comparisonA$count  

# ggplot(Min.abund.comparison, aes(x=DR, y=count)) + 
#   geom_boxplot(aes(group = DR))+
#   stat_summary(fun="mean",color="red", shape=16)+
#   stat_summary(fun.data = mean_txt, geom="text", color="red",vjust=1.2)+
#   stat_summary(fun="max",color="blue", shape=8)+
#   stat_summary(fun.data = median_txt, geom="text", color="black",vjust=1.7)+
#   stat_summary(fun.data = max_txt, geom="text", color="blue",vjust=-30)+
#   ylab("Difference in Final Crayfish Abundance (Millions)")+ xlab("Alternative")+
#   scale_y_continuous(labels = scales::label_number_si())


mins <- Min.abund.comparisonA  %>%
  group_by(DR) %>%
  slice(which.min(count)) 

mins$count <- round_to(mins$count, 100)
mins <- data.frame(mins)

maxs <- Min.abund.comparisonA  %>%
  group_by(DR) %>%
  slice(which.max(count)) 

maxs$count <- round_to(maxs$count, 100000)
maxs <- data.frame(maxs)



ggplot(Min.abund.comparisonA, aes(x=DR, y=count)) + 
  geom_violin(aes(group = DR, fill = DR))+
  scale_fill_manual(values = truth_colors, labels = labels_truth)+
  geom_boxplot(width=.05)+
  stat_summary(fun="mean",color="blue", shape=17)+
  stat_summary(fun.data = mean_txt, geom="text", color="blue",hjust=1.6)+
  annotate("text", x=mins[1,6], y=-500000, label= min_txt(mins[1,5])[2], color = "black")+
  annotate("text", x=mins[2,6], y=-500000, label= min_txt(mins[2,5])[2], color = "black")+
  annotate("text", x=mins[3,6], y=-500000, label= min_txt(mins[3,5])[2], color = "black")+
  annotate("text", x=mins[4,6], y=-500000, label= min_txt(mins[4,5])[2], color = "black")+
  annotate("text", x=maxs[1,6], y=14000000, label= min_txt(maxs[1,5])[2], color = "black")+
  annotate("text", x=maxs[2,6], y=14000000, label= min_txt(maxs[2,5])[2], color = "black")+
  annotate("text", x=maxs[3,6], y=14000000, label= min_txt(maxs[3,5])[2], color = "black")+
  annotate("text", x=maxs[4,6], y=14000000, label= min_txt(maxs[4,5])[2], color = "black")+
  ylab("No Control - Alternative Final Abundance (Millions of Crayfish)")+ xlab("Alternative")+
  scale_y_continuous(limits = c(-500000,15000000), labels = scales::label_number_si())+
  scale_x_discrete(labels = labels_truth)+
  guides(fill=guide_legend(title="Alternative"))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))

#####Test 2 #####
Min.abund.comparisonB <- Min.abund.comparison 
Min.abund.comparisonB$count <-  Min.abund.comparisonB$count  - nocontrols


mins <- Min.abund.comparisonB  %>%
  group_by(DR) %>%
  slice(which.min(count)) 

mins$count <- round_to(mins$count, 100000)
mins <- data.frame(mins)
mins

maxs <- Min.abund.comparisonB  %>%
  group_by(DR) %>%
  slice(which.max(count)) 

maxs

maxs$count <- round_to(maxs$count,100)
maxs$count <- -maxs$count
maxs <- data.frame(maxs)
maxs 

means <- Min.abund.comparisonB  %>%
  filter(DR == 'Random') %>%

mean(means$count)

labels_truth <- c("Abundant (truth)", "Edge (truth)", "Growth (truth)", "Random")

ggplot(Min.abund.comparisonB, aes(x=DR, y=count)) + 
  geom_violin(aes(group = DR, fill = DR))+
  scale_fill_manual(values = truth_colors, labels = labels_truth)+
  geom_boxplot(width=.05)+
  stat_summary(fun="mean",color="blue", shape=17)+
  stat_summary(fun.data = mean_txt, geom="text", color="blue",hjust=1.4)+
  annotate("text", x=maxs[1,6], y=500000, label= min_txt(maxs[1,5])[2], color = "black")+
  annotate("text", x=maxs[2,6], y=500000, label= min_txt(maxs[2,5])[2], color = "black")+
  annotate("text", x=maxs[3,6], y=500000, label= min_txt(maxs[3,5])[2], color = "black")+
  annotate("text", x=maxs[4,6], y=500000, label= min_txt(maxs[4,5])[2], color = "black")+
  annotate("text", x=mins[1,6], y=-14000000, label= min_txt(mins[1,5])[2], color = "black")+
  annotate("text", x=mins[2,6], y=-14000000, label= min_txt(mins[2,5])[2], color = "black")+
  annotate("text", x=mins[3,6], y=-14000000, label= min_txt(mins[3,5])[2], color = "black")+
  annotate("text", x=mins[4,6], y=-14000000, label= min_txt(mins[4,5])[2], color = "black")+
  ylab("Alternative - No Control Final Abundance (Millions of Crayfish)")+ xlab("Alternative")+
  scale_y_continuous(limits = c(-15000000,500000), labels = scales::label_number_si())+
  scale_x_discrete(labels = labels_truth)+
  guides(fill=guide_legend(title="Alternative"))+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))


#### Test 3 - pretend we have all data ####
Min.abund.comparisonC <- Min.abund.comparisonB

for(i in 1:length(Min.abund.comparisonC$DR)){
  if(Min.abund.comparisonC$DR[i] == "Abundant"){
    Min.abund.comparisonC$DR[i] <- "Abundant (truth)"
  }
  if(Min.abund.comparisonC$DR[i] == "Growth"){
    Min.abund.comparisonC$DR[i] <- "Growth (truth)"
  }
  if(Min.abund.comparisonC$DR[i] == "Edge"){
    Min.abund.comparisonC$DR[i] <- "Edge (truth)"
  }
}

Min.abund.comparisonC <- Min.abund.comparisonC %>% filter(DR != "Random")

Min.abund.comparisonD <- rbind(Min.abund.comparisonB, Min.abund.comparisonC)


mins <- Min.abund.comparisonD %>%
  group_by(DR) %>%
  slice(which.min(count)) 

mins$count <- round_to(mins$count, 100000)
mins <- data.frame(mins)
mins

maxs <- Min.abund.comparisonD  %>%
  group_by(DR) %>%
  slice(which.max(count))

maxs$count <- round_to(maxs$count,100)
maxs$count <- -maxs$count
maxs <- data.frame(maxs)
maxs 

ggplot(Min.abund.comparisonD, aes(x=DR, y=count)) + 
  geom_violin(aes(group = DR, fill = DR))+
  scale_fill_manual(values = all_colors, labels = labels_all)+
  geom_boxplot(width=.05)+
  stat_summary(fun="mean",color="blue", shape=17)+
  stat_summary(fun.data = mean_txt, geom="text", color="blue",hjust=1.4)+
  annotate("text", x=maxs[1,6], y=500000, label= min_txt(maxs[1,5])[2], color = "black")+
  annotate("text", x=maxs[2,6], y=500000, label= min_txt(maxs[2,5])[2], color = "black")+
  annotate("text", x=maxs[3,6], y=500000, label= min_txt(maxs[3,5])[2], color = "black")+
  annotate("text", x=maxs[4,6], y=500000, label= min_txt(maxs[4,5])[2], color = "black")+
  annotate("text", x=maxs[5,6], y=500000, label= min_txt(maxs[5,5])[2], color = "black")+
  annotate("text", x=maxs[6,6], y=500000, label= min_txt(maxs[6,5])[2], color = "black")+
  annotate("text", x=maxs[7,6], y=500000, label= min_txt(maxs[7,5])[2], color = "black")+
  annotate("text", x=mins[1,6], y=-14000000, label= min_txt(mins[1,5])[2], color = "black")+
  annotate("text", x=mins[2,6], y=-14000000, label= min_txt(mins[2,5])[2], color = "black")+
  annotate("text", x=mins[3,6], y=-14000000, label= min_txt(mins[3,5])[2], color = "black")+
  annotate("text", x=mins[4,6], y=-14000000, label= min_txt(mins[4,5])[2], color = "black")+
  annotate("text", x=mins[5,6], y=-14000000, label= min_txt(mins[5,5])[2], color = "black")+
  annotate("text", x=mins[6,6], y=-14000000, label= min_txt(mins[6,5])[2], color = "black")+
  annotate("text", x=mins[7,6], y=-14000000, label= min_txt(mins[7,5])[2], color = "black")+
  ylab("Alternative - No Control Final Abundance (Millions of Crayfish)")+ xlab("Alternative")+
  scale_y_continuous(limits = c(-15000000,500000), labels = scales::label_number_si())+
  guides(fill=guide_legend(title="Alternative"))+
  scale_x_discrete(labels = labels_all)+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))



#### Minimize spatial extent ####
Min.space <- rbind(abunds.minspace, edges.minspace, grows.minspace, rands.minspace, nocontrols.minspace)

Min.space$pop <- Min.space$pop/35

mean_txt <- function(x){
  return(data.frame(y=mean(x),
                    label = format(round_to(mean(x,na.rm=T), .001),big.mark=",",scientific=FALSE)
                    
  ))}

median_txt <- function(x){
  return(data.frame(y=median(x),
                    label = format(round_to(median(x,na.rm=T), .001),big.mark=",",scientific=FALSE)
                    
  ))}

min_txt <- function(x){
  return(data.frame(y=min(x),
                    label = format(round_to(min(x,na.rm=T), .001),big.mark=",",scientific=FALSE)
                    
  ))}



# ggplot(Min.space, aes(x=DR, y=1-pop)) + 
#   geom_boxplot(aes(group = DR))+
#   stat_summary(fun="mean",color="red", shape=16)+
#   stat_summary(fun.data = mean_txt, geom="text", color="red",vjust=2)+
#   stat_summary(fun.data = median_txt, geom="text", color="black",vjust=-.3)+
#   ylab("Fractions of Segments Invaded")+ xlab("Alternative")+
#   scale_y_continuous(labels = scales::label_number_si())

Min.space.withoutnocontrol <- Min.space %>% filter(DR != "No Control")

ggplot(Min.space.withoutnocontrol, aes(x=DR, y=pop)) +
  geom_violin(aes(group = DR, fill = DR))+
  scale_fill_manual(values = truth_colors,labels = labels_truth)+
  geom_boxplot(width=.1)+
  stat_summary(fun="mean",color="blue", shape=17)+
  stat_summary(fun.data = mean_txt, geom="text", color="blue",hjust=1.2)+
  ylab("Fraction of Segments Invaded")+ xlab("Alternative")+
  scale_y_continuous(labels = scales::label_number_si())+
  guides(fill=guide_legend(title="Alternative"))+scale_x_discrete(labels = labels_truth)+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))



##### Test - pretend we have data ####

Min.space <- Min.space.withoutnocontrol

for(i in 1:length(Min.space $DR)){
  if(Min.space$DR[i] == "Abundant"){
    Min.space$DR[i] <- "Abundant (truth)"
  }
  if(Min.space$DR[i] == "Growth"){
    Min.space$DR[i] <- "Growth (truth)"
  }
  if(Min.space$DR[i] == "Edge"){
    Min.space$DR[i] <- "Edge (truth)"
  }
}

Min.space <- Min.space %>% filter(DR != "Random")

Min.space2 <- rbind(Min.space.withoutnocontrol, Min.space)


ggplot(Min.space2, aes(x=DR, y=pop)) +
  geom_violin(aes(group = DR, fill = DR))+
  scale_fill_manual(values = all_colors, labels = labels_all)+
  geom_boxplot(width=.1)+
  stat_summary(fun="mean",color="blue", shape=17)+
  stat_summary(fun.data = mean_txt, geom="text", color="blue",hjust=1.2)+
  ylab("Fraction of Segments Invaded")+ xlab("Alternative")+
  scale_y_continuous(labels = scales::label_number_si())+
  guides(fill=guide_legend(title="Alternative"))+
  scale_x_discrete(labels = labels_all)+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.title.x = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black"))


