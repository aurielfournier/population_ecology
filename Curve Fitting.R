
nsteps=1000

Stmax=1
d=seq(0,0.1,length=nsteps)
gamma=seq(0,3,length=nsteps)

DensityTRT=c(14,42,84,168,336,672)
SObsPrairie=c(14.00560224,42.01680672,63.02521008,80.53221289,98.03921569,129.5518207)
SObsPond=c(14.00560224,35.0140056,56.02240896,73.52941176,87.53501401,98.03921569)

SEst=rep(0,length(DensityTRT))
Resid=rep(0,length(DensityTRT))
sumResid=matrix(0,nsteps,nsteps)

for (k in 1:nsteps){
for (j in 1:nsteps){

	for (i in 1:length(DensityTRT)){
		SEst[i]=DensityTRT[i]*Stmax/((1+d[k]*DensityTRT[i])^gamma[j])
		Resid[i]=abs(SObsPrairie[i]-SEst[i])
	}

sumResid[k,j]=sum(Resid)

}
}

minloc=which(sumResid==min(sumResid), arr.ind=TRUE)		#finds the matrix address of the minumum value of sumResid
min(sumResid)								#prints the minimum sumResid value
minloc									#prints matrix address of the minimum sumResid value
d[minloc[1]]								#prints the best fit d value (associated with the first value in minloc)
gamma[minloc[2]]								#prints the best fit gamma value (associated with the second value in minloc)

plot(DensityTRT,SObsPrairie,xlab="DensityTRT (per m2)", ylab="Number Surviving (per m2)", col="green",lwd=3)
points(DensityTRT,SObsPond,xlab="DensityTRT (per m2)", ylab="Number Surviving (per m2)", col="red",lwd=3)
legend("bottomright",c("Prairie","Farm Pond", "Best Fit"),col=c("green","red","black"),lwd=c(2))

nsteps=100
Meta=rep(0,nsteps)
T=seq(1,1000,length=nsteps)

for (i in 1:nsteps){
	Meta[i]=T[i]*Stmax/((1+d[minloc[1]]*T[i])^gamma[minloc[2]])
}

lines(T,Meta,lwd=3)
