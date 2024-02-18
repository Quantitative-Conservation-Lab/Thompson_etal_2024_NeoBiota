I <- 35

d.matrix <- matrix(NA, nrow = I, ncol = I)

diag(d.matrix) <- 0

for(i in 1:22){
  d.matrix[i,i:22] <- d.matrix[i:22,i] <- seq(0,(22-i))
  
}

for(i in 1:22){
  d.matrix[i,23] <- d.matrix[23,i] <- d.matrix[i,6]+4
  d.matrix[i,24] <- d.matrix[24,i] <- d.matrix[i,6]+3
  d.matrix[i,25] <- d.matrix[25,i] <- d.matrix[i,6]+2
  d.matrix[i,26] <- d.matrix[26,i] <- d.matrix[i,6]+1

  d.matrix[i,27] <- d.matrix[27,i] <- d.matrix[i,6]+4
  d.matrix[i,28] <- d.matrix[28,i] <- d.matrix[i,6]+3
  
  d.matrix[i,29] <- d.matrix[29,i] <- d.matrix[i,8] + 5
  d.matrix[i,30] <- d.matrix[30,i] <- d.matrix[i,8] + 4
  d.matrix[i,31] <- d.matrix[31,i] <- d.matrix[i,8] + 3
  d.matrix[i,32] <- d.matrix[32,i] <- d.matrix[i,8] + 2
  d.matrix[i,33] <- d.matrix[33,i] <- d.matrix[i,8] + 1
  
  d.matrix[i,34] <- d.matrix[34,i] <- d.matrix[i,8] + 5
  d.matrix[i,35] <- d.matrix[35,i] <- d.matrix[i,8] + 4
  
}

for(i in 23:26){
  d.matrix[i,i:26] <- d.matrix[i:26,i] <- seq(0,(26-i))
}

d.matrix[28,27] <- d.matrix[27,28] <- 1

for(i in 23:26){
  d.matrix[i,27] <- d.matrix[27,i] <- d.matrix[i,25]+2
  d.matrix[i,28] <- d.matrix[28,i] <- d.matrix[i,25]+1
}

for(i in 29:33){
  d.matrix[i,i:33] <- d.matrix[i:33,i] <- seq(0,(33-i))
}

d.matrix[35,34] <- d.matrix[34,35] <- 1

for(i in 29:33){
  d.matrix[i,34] <- d.matrix[34,i] <- d.matrix[i,31]+2
  d.matrix[i,35] <- d.matrix[35,i] <- d.matrix[i,31]+1
}

for(i in 29:35){
  for(h in 23:28){
    d.matrix[i,h] <- d.matrix[h,i] <- d.matrix[i,6] + d.matrix[6,h]
  }
}

d.matrix

save(d.matrix, file = "data/parameters/d.matrix.RData")

# load("data/parameters/d.matrix.RData")
