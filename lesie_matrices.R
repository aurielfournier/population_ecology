m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t

####
#project into future
####

for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
#####
#total population by column
#####

total <- colSums(les)

######
#take the log
######

log <- log(les)

########
#lambda
########
lambda <- matrix(ncol=11, nrow=1)
lambda[1] <- 0
for(i in 2:11){
lambda[,i] <-  total[i]/total[i-1]
}

########
# age distribution
########
agedist <- matrix(ncol=11,nrow=4)
for(i in 1:11){
agedist[,i] <- les[,i]/total[i]
}

##########part II#######

eigens=eigen(matrix)

lambda=eigens$values[1] #stable lambda
c=eigens$vectors[,1]/sum(eigens$vectors[,1]) # stable age distribution


#########part III#######


#### varying m1
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t

sens <- matrix(ncol=100,nrow=4)
unim1 <- runif(min=0, max=4,n=100)

for(j in 1:100){
  matrix[1,1]<-unim1[j]*.5
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
}

csm1 <- colSums(sens)

plot(uni,csm1)

##### varying p0
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t

sens <- matrix(ncol=100,nrow=4)
unip0 <- runif(min=0.1, max=.9,n=100)

for(j in 1:100){
  matrix[1,1]<-unip0[j]*2
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
}

csp0 <- colSums(sens)

plot(uni,csp0)



##### varying p1
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t

sens <- matrix(ncol=100,nrow=4)
unip1 <- runif(min=0.1, max=.9,n=100)

for(j in 1:100){
  matrix[2,2]<-unip1[j]*2
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
}

csp1 <- colSums(sens)

plot(unim1,csm1)
points(unip0,csp0, add=T, col="red")
points(unip1,csp1, add=T, col="blue")


########### Challenge ################


fa <- 10
sy1 <- .35
sy2 <- .53
sy3 <- .72
sa <- .9

m <- c(0,sy2,0,0,0,sy3,fa*sy1,0,sa)

matrix <- matrix(m, ncol=3, nrow=3)

les <- matrix(ncol=100, nrow=3)