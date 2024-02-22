library(tidyverse)
library(here)
library(plyr)
library(data.table)

##### average final N @ sites #####
path <- 'D:\\Chapter2\\results\\abund'
file_name = paste(path, 'abund_Nseg.csv',sep = '/')
abund_segfin <- fread(file_name)
abund_segfin <- data.frame(abund_segfin)

path <- 'D:\\Chapter2\\results\\down'
file_name = paste(path, 'down_Nseg.csv',sep = '/')
down_segfin <- fread(file_name)
down_segfin <- data.frame(down_segfin)

path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_Nseg.csv',sep = '/')
edge_segfin <- fread(file_name)
edge_segfin <- data.frame(edge_segfin)

path <- 'D:\\Chapter2\\results\\grow'
file_name = paste(path, 'grow_Nseg.csv',sep = '/')
grow_segfin <- fread(file_name)
grow_segfin <- data.frame(grow_segfin)

path <- 'D:\\Chapter2\\results\\random'
file_name = paste(path, 'random_Nseg.csv',sep = '/')
random_segfin <- fread(file_name)
random_segfin <- data.frame(random_segfin)

path <- 'D:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'nocontrol_Nseg.csv',sep = '/')
nocontrol_segfin <- fread(file_name)
nocontrol_segfin <- data.frame(nocontrol_segfin)

all_segfin <- rbind(abund_segfin,down_segfin,edge_segfin,
                    grow_segfin,random_segfin,nocontrol_segfin)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_segfin.csv',sep = '/')
fwrite(all_segfin,file_name)

##### final total N #####
path <- 'D:\\Chapter2\\results\\abund'
file_name = paste(path, 'abund_Ntotal.csv',sep = '/')
abund_Ntotal <- fread(file_name)
abund_Ntotal <- data.frame(abund_Ntotal)

path <- 'D:\\Chapter2\\results\\down'
file_name = paste(path, 'down_Ntotal.csv',sep = '/')
down_Ntotal <- fread(file_name)
down_Ntotal <- data.frame(down_Ntotal)

path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_Ntotal.csv',sep = '/')
edge_Ntotal <- fread(file_name)
edge_Ntotal <- data.frame(edge_Ntotal)

path <- 'D:\\Chapter2\\results\\grow'
file_name = paste(path, 'grow_Ntotal.csv',sep = '/')
grow_Ntotal <- fread(file_name)
grow_Ntotal <- data.frame(grow_Ntotal)

path <- 'D:\\Chapter2\\results\\random'
file_name = paste(path, 'random_Ntotal.csv',sep = '/')
random_Ntotal <- fread(file_name)
random_Ntotal <- data.frame(random_Ntotal)

path <- 'D:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'nocontrol_Ntotal.csv',sep = '/')
nocontrol_Ntotal <- fread(file_name)
nocontrol_Ntotal <- data.frame(nocontrol_Ntotal)

all_Ntotal <- rbind(abund_Ntotal,down_Ntotal,edge_Ntotal,
                  grow_Ntotal,random_Ntotal,nocontrol_Ntotal)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_Ntotal.csv',sep = '/')
fwrite(all_Ntotal,file_name)

##### N v Time #####
path <- 'D:\\Chapter2\\results\\abund'
file_name = paste(path, 'abund_Nvtime.csv',sep = '/')
abund_Nvtime <- fread(file_name)
abund_Nvtime <- data.frame(abund_Nvtime)

path <- 'D:\\Chapter2\\results\\down'
file_name = paste(path, 'down_Nvtime.csv',sep = '/')
down_Nvtime <- fread(file_name)
down_Nvtime <- data.frame(down_Nvtime)

path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_Nvtime.csv',sep = '/')
edge_Nvtime <- fread(file_name)
edge_Nvtime <- data.frame(edge_Nvtime)

path <- 'D:\\Chapter2\\results\\grow'
file_name = paste(path, 'grow_Nvtime.csv',sep = '/')
grow_Nvtime <- fread(file_name)
grow_Nvtime <- data.frame(grow_Nvtime)

path <- 'D:\\Chapter2\\results\\random'
file_name = paste(path, 'random_Nvtime.csv',sep = '/')
random_Nvtime <- fread(file_name)
random_Nvtime <- data.frame(random_Nvtime)

path <- 'D:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'nocontrol_Nvtime.csv',sep = '/')
nocontrol_Nvtime <- fread(file_name)
nocontrol_Nvtime <- data.frame(nocontrol_Nvtime)

all_Nvtime <- rbind(abund_Nvtime,down_Nvtime,edge_Nvtime,
                  grow_Nvtime,random_Nvtime,nocontrol_Nvtime)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_Nvtime.csv',sep = '/')
fwrite(all_Nvtime,file_name)

##### Dcol #####
path <- 'D:\\Chapter2\\results\\abund'
file_name = paste(path, 'abund_Dcol.csv',sep = '/')
abund_Dcol <- fread(file_name)
abund_Dcol <- data.frame(abund_Dcol)

path <- 'D:\\Chapter2\\results\\down'
file_name = paste(path, 'down_Dcol.csv',sep = '/')
down_Dcol <- fread(file_name)
down_Dcol <- data.frame(down_Dcol)

path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_Dcol.csv',sep = '/')
edge_Dcol <- fread(file_name)
edge_Dcol <- data.frame(edge_Dcol)

path <- 'D:\\Chapter2\\results\\grow'
file_name = paste(path, 'grow_Dcol.csv',sep = '/')
grow_Dcol <- fread(file_name)
grow_Dcol <- data.frame(grow_Dcol)

path <- 'D:\\Chapter2\\results\\random'
file_name = paste(path, 'random_Dcol.csv',sep = '/')
random_Dcol <- fread(file_name)
random_Dcol <- data.frame(random_Dcol)

path <- 'D:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'nocontrol_Dcol.csv',sep = '/')
nocontrol_Dcol <- fread(file_name)
nocontrol_Dcol <- data.frame(nocontrol_Dcol)

all_Dcol <- rbind(abund_Dcol,down_Dcol,edge_Dcol,
                    grow_Dcol,random_Dcol,nocontrol_Dcol)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_Dcol.csv',sep = '/')
fwrite(all_Dcol,file_name)

##### Distance Traveled #####
path <- 'D:\\Chapter2\\results\\abund'
file_name = paste(path, 'abund_Dtrav.csv',sep = '/')
abund_Dtrav <- fread(file_name)
abund_Dtrav <- data.frame(abund_Dtrav)

path <- 'D:\\Chapter2\\results\\down'
file_name = paste(path, 'down_Dtrav.csv',sep = '/')
down_Dtrav <- fread(file_name)
down_Dtrav <- data.frame(down_Dtrav)

path <- 'D:\\Chapter2\\results\\edge'
file_name = paste(path, 'edge_Dtrav.csv',sep = '/')
edge_Dtrav <- fread(file_name)
edge_Dtrav <- data.frame(edge_Dtrav)

path <- 'D:\\Chapter2\\results\\grow'
file_name = paste(path, 'grow_Dtrav.csv',sep = '/')
grow_Dtrav <- fread(file_name)
grow_Dtrav <- data.frame(grow_Dtrav)

path <- 'D:\\Chapter2\\results\\random'
file_name = paste(path, 'random_Dtrav.csv',sep = '/')
random_Dtrav <- fread(file_name)
random_Dtrav <- data.frame(random_Dtrav)

grow_Dtrav$location <- 'grow'
file_name = paste(path, 'grow_Dtrav.csv',sep = '/')
fwrite(grow_Dtrav,file_name)

all_Dtrav <- rbind(abund_Dtrav,down_Dtrav,edge_Dtrav,
                  grow_Dtrav,random_Dtrav)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_Dtrav.csv',sep = '/')
fwrite(all_Dtrav,file_name)

##### Segs Invaded #####
path <- 'E:\\Chapter2\\results\\nocontrol'
file_name = paste(path, 'nocontrol_Ntotal.csv',sep = '/')
nocontrol_Ntotal <- fread(file_name)
nocontrol_Ntotal <- data.frame(nocontrol_Ntotal)

#average final total population 
nocontrol_Ntotal_avgp <- aggregate(count ~ param +p + rem , 
                                   data = as.data.frame(nocontrol_Ntotal), 
                                   FUN = mean)

#find invasion cut off for each parameter set
nocontrol_Ntotal_avgp$invasioncut <- (nocontrol_Ntotal_avgp$count*0.1)/ 35


#then for each alternative under each parameter set and simulation...
#a segment is invaded if it is > the invasion cut off value
file_name = paste(path, 'abund_segfin_summary.csv',sep = '/')
file_name = paste(path, 'down_segfin_summary.csv',sep = '/')
file_name = paste(path, 'edge_segfin_summary.csv',sep = '/')
file_name = paste(path, 'grow_segfin_summary.csv',sep = '/')
file_name = paste(path, 'random_segfin_summary.csv',sep = '/')
file_name = paste(path, 'nocontrol_segfin_summary.csv',sep = '/')








all_Ninvade <- rbind(abund_Ninvade,down_Ninvade,edge_Ninvade,
                     grow_Ninvade,random_Ninvade,nocontrol_Ninvade)

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_Ninvade.csv',sep = '/')
fwrite(all_Ninvade,file_name)





