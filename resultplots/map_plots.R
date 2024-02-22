library(tidyverse)
library(here)
library(plyr)
library(data.table)
library(scales)
library(RColorBrewer) 

path <- 'D:\\Chapter2\\results'
file_name = paste(path, 'all_segfin.csv',sep = '/')
all_segfin <- fread(file_name)
all_segfin <- data.frame(all_segfin)

