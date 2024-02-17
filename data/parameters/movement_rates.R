#libraries
library(ggplot2)
library(tidyr)
library(here)

#### Dispersal Kernel ####
#From Messager & Olden
# kernel <- c(0.30 + 0.09 + 0.08 + 0.06 + 0.06 + 0.06 + 0.05 + 0.05 + 0.05 + 0.04,
#               0.04 + 0.03 +  0.03 + 0.02 + 0.02 + 0.01 + 0.01)

kernel <- c(0.85, 0.15)
kernel1 <- kernel
#scaling factor:
#to incorporate uncertainty, we will modify the stationary (movement) probability
#we will change the probability of staying (first probability in the kernel vector)
#for a scaling rate r we will increase the stationary probability by r%, r/2% 
#and decrease by r%, r/2%. 
r <- 0.05

kernel2 <- c(kernel1[1] + r, 1-(kernel1[1] + r))
kernel3 <- c(kernel1[1] + 2*r, 1-(kernel1[1] + 2*r))
kernel4 <- c(kernel1[1] - r, 1-(kernel1[1] -r))
kernel5 <- c(kernel1[1] - 2*r, 1-(kernel1[1] -2*r))

dist <- c(0,1)
#these are all the 5 kernels!
kernels <- cbind(dist,kernel1, kernel2, kernel3, kernel4, kernel5)

kernel_data <- as.data.frame(kernels)
names(kernel_data)[1] <- "Distance"
names(kernel_data)[2] <- "Kernel1"
names(kernel_data)[3] <- "Kernel2"
names(kernel_data)[4] <- "Kernel3"
names(kernel_data)[5] <- "Kernel4"
names(kernel_data)[6] <- "Kernel5"

data_long <- gather(kernel_data,
                    kernel, probability,
                    Kernel1:Kernel5, factor_key=TRUE)

#Plot kernel
ggplot(data_long, aes(Distance, probability, fill = kernel)) + 
  geom_bar(position='dodge', stat = "identity") +
  ylab("Movement probability") +
  labs(title = "Kernels") +
  facet_wrap(~kernel)

data_long <- gather(kernel_data,
                    kernel, probability,
                    Kernel1:Kernel5, factor_key=TRUE)


ggplot(data_long, aes(Distance, probability, fill = kernel)) + 
  geom_bar(position='dodge', stat = "identity") +
  ylab("Movement probability") +
  xlab("Segments moved") +
  scale_x_continuous(breaks = c(0,1)) +
  facet_wrap(~kernel, 
             labeller = labeller(kernel = c("Kernel1" = "Original",
                                            "Kernel2" = "Kernel 2", 
                                            "Kernel3" = "Kernel 3",
                                            "Kernel4"= "Kernel 4",
                                            "Kernel5"="Kernel 5"), 
                                          facet_category = label_wrap_gen(width = 50)))+
  theme(legend.position = "none")


################################################################################
# #### Bifurcation ####
# #CSV of movement probabilities under original kernel
move_prob_kernel1 <- read.csv(here::here("data", "movement", "JDR_5km_movementprobs.csv"))
move_prob_kernel1 <- move_prob_kernel1[,-1] #remove weird first column
move_prob_kernel1 <- as.matrix(move_prob_kernel1)
colnames(move_prob_kernel1) <- seq(1,nrow(move_prob_kernel1)) #114
rownames(move_prob_kernel1) <- seq(1,nrow(move_prob_kernel1)) #114 total

#sum(is.na(move_prob_kernel1) == TRUE) #double check that there is no missing data

move_prob_kernel2 <- move_prob_kernel1
for(i in 1:nrow(move_prob_kernel2)){
  for(h in 1:ncol(move_prob_kernel2)){
    if(move_prob_kernel2[i,h] == kernel[1]){
      move_prob_kernel2[i,h] <- kernel2[1]
    }
    if(move_prob_kernel2[i,h] == kernel[2]){
      move_prob_kernel2[i,h] <- kernel2[2]
    }
  }
}

move_prob_kernel3 <- move_prob_kernel1
for(i in 1:nrow(move_prob_kernel3)){
  for(h in 1:ncol(move_prob_kernel3)){
    if(move_prob_kernel3[i,h] == kernel[1]){
      move_prob_kernel3[i,h] <- kernel3[1]
    }
    if(move_prob_kernel3[i,h] == kernel[2]){
      move_prob_kernel3[i,h] <- kernel3[2]
    }
  }
}


move_prob_kernel4 <- move_prob_kernel1
for(i in 1:nrow(move_prob_kernel4)){
  for(h in 1:ncol(move_prob_kernel4)){
    if(move_prob_kernel4[i,h] == kernel[1]){
      move_prob_kernel4[i,h] <- kernel4[1]
    }
    if(move_prob_kernel4[i,h] == kernel[2]){
      move_prob_kernel4[i,h] <- kernel4[2]
    }
  }
}

move_prob_kernel5 <- move_prob_kernel1
for(i in 1:nrow(move_prob_kernel5)){
  for(h in 1:ncol(move_prob_kernel5)){
    if(move_prob_kernel5[i,h] == kernel[1]){
      move_prob_kernel5[i,h] <- kernel5[1]
    }
    if(move_prob_kernel5[i,h] == kernel[2]){
      move_prob_kernel5[i,h] <- kernel5[2]
    }
  }
}

write.table(move_prob_kernel1,
            file=here::here("data", "movement", "movementprobs_k1.csv"),
            sep = ',', col.names = T, row.names = T)

write.table(move_prob_kernel2,
            file=here::here("data", "movement", "movementprobs_k2.csv"),
            sep = ',', col.names = T, row.names = T)

write.table(move_prob_kernel3,
            file=here::here("data", "movement", "movementprobs_k3.csv"),
            sep = ',', col.names = T, row.names = T)

write.table(move_prob_kernel4,
            file=here::here("data", "movement", "movementprobs_k4.csv"),
            sep = ',', col.names = T, row.names = T)

write.table(move_prob_kernel5,
            file=here::here("data", "movement", "movementprobs_k5.csv"),
            sep = ',', col.names = T, row.names = T)

#CSV of movement multipliers
move_mults <- read.csv(here::here("data", "movement", "JDR_5km_movementmults.csv"))
move_mults <- move_mults[,-1] #remove weird first column
dim(move_mults) #114 x 114
move_mults <- as.matrix(move_mults)
colnames(move_mults) <- seq(1,ncol(move_mults))
rownames(move_mults) <- seq(1,nrow(move_mults))

sum(is.na(move_mults) == TRUE) #double check that there is no missing data
