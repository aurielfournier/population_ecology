## Field Experiment Modeling Day
## April 15 2015
## Population Ecology Spring 2015
## Auriel Fournier

## now we are going to use the 'given results' values (since the exp isn't done yet, sad day)

## already summarized from excel
sumdat <- read.csv("surv_summary.csv")

## raw stuff
rawdat <- read.csv("raw_fake_data.csv")

## we need to use our fake data to find d and gamma

## Survival of tadpoles St = Stmax /(1-d*Tadpoles(t))^gamma
		## gamma is what tells you if it's over or under compensating 

T <- seq(1:1000)
stmax = 1
d = .004
gamma = 1

mat <- matrix(nrow=1000,ncol=2)
mat[,1] <- T

for(i in 2:max(T)){
mat[i,2] <- T[i] * ((stmax)/(1+d*(T[i]^gamma)))
}

plot(mat)

######################
## plotting the 'real' data
######################

par(mfrow=c(2,1))
plot(sumdat[sumdat$veg=="prairie",]$density, sumdat[sumdat$veg=="prairie",]$density_m_sqr)

plot(sumdat[sumdat$veg=="farm",]$density, sumdat[sumdat$veg=="farm",]$density_m_sqr)
par(mfrow=c(2,1))
plot(sumdat[sumdat$veg=="prairie",]$density, sumdat[sumdat$veg=="prairie",]$surv_m_sqr)

plot(sumdat[sumdat$veg=="farm",]$density, sumdat[sumdat$veg=="farm",]$surv_m_sqr)


###########################
## copied from JD's script
###########################

densitytrt <- grass[,2]
nsteps = 100
sexpected = rep(0,length(densitytrt))
resid=rep(0,length(densitytrt))
sumresid = matrix(0,nsteps,nsteps)
d=seq(0,0.1,length=nsteps)
gamma=seq(0,3,length=nsteps)

for(j in 1:nsteps){
for(k in 1:nsteps){
	for(i in 1:length(densitytrt)){
		sexpected[i] <- densitytrt[i]*stmax/(1+d[j]*densitytrt[i])^gamma[k]
		resid[i] <- abs(sexpected[i]-densitytrt[i])
	sumresid[j,k]=sum(resid)
}
}}

minloc <- which(sumresid==min(sumresid),arr.ind=TRUE)
minloc






