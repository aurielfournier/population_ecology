#####################################################
#lambda varies over time
#####################################################

low <- as.data.frame(matrix(ncol=100, nrow=100))

high <- as.data.frame(matrix(ncol=100, nrow=100))

t <- seq(2,101, by=1)
N0 <- 50
high[1,] <- N0
low[1,] <- N0

for(i in 1:100){
  for(j in 1:100){
	lambda <- rnorm(n=100, mean=1.05,sd=.3)
	high[t[i],j] <- high[(t[i]-1),j]*lambda[t[i]]
}
}

for(i in 1:100){
  for(j in 1:100){
    lambda <- rnorm(n=100, mean=1.05,sd=.1)
    low[t[i],j] <- low[(t[i]-1),j]*lambda[t[i]]
  }
}



low$mean <- rowMeans(low)
high$mean <- rowMeans(high)

#generate the arith mean
lambda <- rep(1.05,100)
t <- seq(1,100,by=1)
arith <- N0*(lambda^t)

par(mfcol=c(3,1))

#plot low 
plot(low[,1] ,  type="l")
for(i in 2:10){
  lines(low[,i], type="l")
}

#plot high 
plot(high[,1],type="l")
for(i in 2:10){
  lines(high[,i], type="l")
}

# plot means
plot(arith, col="red", type="l", lwd=3)
lines(high$mean,  col="orange",lwd=3)
lines(low$mean,  col="green", lwd=3)
