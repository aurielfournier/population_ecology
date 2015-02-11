### lab 4, week 5, population ecology
options(scipen=999)

years <- 10
r <- 1.5
N0 <- 2
K <- 100
Nhat <- matrix(ncol=2, nrow=years)
)
t <- seq(1,years,by=1)
Nhat[1,] <- N0

for(i in 2:years){
Nhat[i,] <- K/(1+((K-Nhat[i-1,])/Nhat[i-1,])*exp(-r*i))
}
Nhat[,2] <- seq(1,10,by=1
plot(Nhat)


#####################


r = 1.5
K=100
tmax=10
dt=0.01
N <- matrix(ncol=2,nrow=10)
N[1]=2

for(i in 2:tmax){
N[i,] <- N[i-1,] + dt * r*N[i-1,]*((K-N[i-1,])/K)
}

N[,2] <- seq(1,10,by=1)
plot(Nhat[,2],Nhat[,1])
lines(N[,2],N[,1])

#################################

r = 5
K=1000
N <- 900

dndt <- r * N *(N-K)*((K-N)/K)

#############
# Matrix Part
##################

dat <- matrix(rnorm(mean=1, sd=2,n=35),5,7)

###############

a <- cbind(1:3,4:6,7:9)
b <- rbind(1:3,4:6)



###############

dat <- matrix(runif(min=0,max=1,n=25),5,5)

dat[2,2]

dat[2:4,2:4]

dat[1,] <- c(2,5,8,11,14)

a <- matrix(c(1,4,9,16),2,2)

#######################

years <- 100
r <- matrix(runif(min=.5, max=1.5, n=500*10),ncol=10, nrow=years)
N0 <- 50
K <- 1000
Nhat <- matrix(ncol=10, nrow=years)

Nhat[1,] <- N0

for(i in 2:years){
Nhat[i,] <- Nhat[i-1,]*exp(r[1,]*(1-(Nhat[i-1,]/K)))
}

end <- rowMeans(Nhat)