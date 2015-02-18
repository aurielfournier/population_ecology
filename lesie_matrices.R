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

###########################
#########part III#######
###########################

#### varying m1
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t
lambdam1 <- rep(0,31)
sens <- matrix(ncol=31,nrow=4)
unim1 <- seq(1, 4,by=.1)

for(j in 1:31){
  matrix[1,1]<-unim1[j]*.5
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
tot <- colSums(les)
lambdam1[j] <- tot[11]/tot[10]
}



##### varying p0
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t
lambdap0 <- rep(0,21)
sens <- matrix(ncol=21,nrow=4)
unip0 <- seq(0, 1,by=.05)

for(j in 1:21){
  matrix[1,1]<-unip0[j]*2
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
tot <- colSums(les)
lambdap0[j] <- tot[11]/tot[10]

}







##### varying p1
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)

t <- c(200,0,0,0)

les <- matrix(ncol=11, nrow=4)
les[,1] <- t
lambdap1 <- rep(0,21)
sens <- matrix(ncol=21,nrow=4)
unip1 <- seq(0, 1,by=.05)

for(j in 1:21){
  matrix[2,2]<-unip1[j]*2
for(i in 2:11){
les[,i] <- matrix %*% les[,(i-1)]
}
sens[,j] <- les[,11]
tot <- colSums(les)
lambdap1[j] <- tot[11]/tot[10]
}

##

###############
# with eigen lambda
###############

m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)
unim1 <- seq(1, 4,by=.1)
for(i in 1:31){
 matrix[1,1]<-unim1[i]*.5
eigenm1 <- eigen(matrix)
lambdam1[i] <- eigenm1$values[1]
}

##
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)
unip0 <- seq(0, 1,by=.05)
for(i in 1:21){
 matrix[1,1]<-unip0[i]*.5
eigenp0 <- eigen(matrix)
lambdap0[i] <- eigenp0$values[1]
}


##
m <- c(1,.5,0,0,.4,0,.4,0,.5,0,0,.5,0,0,0,0)
matrix <- matrix(m, nrow=4)
unip1 <- seq(0, 1,by=.05)
for(i in 1:21){
 matrix[2,2]<-unip0[i]*.5
eigenp1 <- eigen(matrix)
lambdap1[i] <- eigenp1$values[1]
}


par(mfrow=c(1,2))
plot(unip1,lambdap1, xlim=c(0,1),ylim=c(.5,1.5))
points(unip0,lambdap0,  col="blue")

plot(unim1, lambdam1)


#######################################
########### Challenge ################
###################################

fa <- 10
sy1 <- .20
sy2 <- .53
sy3 <- .72
sa <- .9

m <- c(0,sy2,0,0,0,sy3,fa*sy1,0,sa)

matrix <- matrix(m, ncol=3, nrow=3)

t <- c(100,100,100)
les <- matrix(ncol=20, nrow=3)
les[,1] <- t


for(i in 2:20){
les[,i] <- matrix %*% les[,(i-1)]
}

total <- colSums(les)
agedist <- matrix(ncol=20,nrow=3)
for(i in 1:20){
agedist[,i] <- les[,i]/total[i]
}


### changing first year survival

dat <- matrix(ncol=11,nrow=3)
fys <- seq(.1,.35,by=.025)
for(j in 1:11){
m <- c(0,sy2,0,0,0,sy3,fa*fys[j],0,sa)

matrix <- matrix(m, ncol=3, nrow=3)

t <- c(100,100,100)
les <- matrix(ncol=20, nrow=3)
les[,1] <- t
for(i in 2:20){
les[,i] <- matrix %*% les[,(i-1)]
}
dat[,j] <- les[,20]
}

total <- colSums(dat)

plot(fys,total)



### changing adult survival

dat <- matrix(ncol=17,nrow=3)
as <- seq(.1,.9,by=.05)
for(j in 1:17){
m <- c(0,sy2,0,0,0,sy3,fa*as[j],0,sa)

matrix <- matrix(m, ncol=3, nrow=3)

t <- c(100,100,100)
les <- matrix(ncol=20, nrow=3)
les[,1] <- t
for(i in 2:20){
les[,i] <- matrix %*% les[,(i-1)]
}
dat[,j] <- les[,20]
}

total <- colSums(dat)

plot(as,total)