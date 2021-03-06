# population ecology
# Chapter 2 simulation

#from page 56-57

###########################################################################
##                Stochastic Population Model                            ##
###########################################################################
##                       Variables                                       ##
###########################################################################
pop<-1000      ### populations
iter<-100      ### number of time steps
N0<-100         ### initial population size
lmean<-1.1    ### mean of lamda
lowsd<-.01      ### SD of lamda low
highsd <- .03   ### SD of lambda high
K <- 1000      ### Carrying Capacity
###########################################################################
high <- data.frame(matrix(ncol=pop, nrow=iter))  
high[1,] <- N0 
#lamda<-matrix(data=rnorm(n=pop*iter, mean=lmean,sd=highsd),nrow=iter,ncol=pop)
for(i in 1:(iter-1)){
  high[(i+1),] <- high[i,]*exp(log(rnorm(n=pop*iter, mean=lmean,sd=highsd))*((K-N0)/K))
  
  ###################################
  ##            clock              ##
  ###################################
  total <- (iter-1)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  Sys.sleep(0.001)
  setTxtProgressBar(pb, i)
  ###################################
}

low <- data.frame(matrix(ncol=pop, nrow=iter))  
low[1,] <- N0 
for(i in 1:(iter-1)){
  low[(i+1),]<-low[i,]*exp(log(rnorm(n=pop*iter, mean=lmean,sd=lowsd))*((K-N0)/K))
  
  ###################################
  ##            clock              ##
  ###################################
  total <- (iter-1)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  Sys.sleep(0.001)
  setTxtProgressBar(pb, i)
  ###################################
}


low$mean <- rowMeans(low)
high$mean <- rowMeans(high)

#generate the arith mean
lambda <- 1.1
time <- seq(1,100,by=1)
arith <- N0*exp(log(lambda)*((K-N0)/K))

low[i,]*exp(log(rnorm(n=pop*iter, mean=lmean,sd=lowsd))*((K-N0)/K))

png("3_plot_lambda.png")
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
lines(low$mean,  col="blue", lwd=3)
title(main="Red = Arithmetic, Orange = High, Blue = Low")
dev.off()

print(paste0(sum(apply(as.data.frame(high<1),2,sum)>0),' high SD populations have crashed'))
print(paste0(sum(apply(as.data.frame(low <1),2,sum)>0),' low SD populations have crashed'))
