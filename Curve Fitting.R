## Prairie

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

Prairieminloc=which(sumResid==min(sumResid), arr.ind=TRUE)		#finds the matrix address of the minumum value of sumResid
min(sumResid)								#prints the minimum sumResid value
Prairieminloc									#prints matrix address of the minimum sumResid value
d[Prairieminloc[1]]								#prints the best fit d value (associated with the first value in minloc)
gamma[Prairieminloc[2]]								#prints the best fit gamma value (associated with the second value in minloc)



nsteps=1000
PrairieMeta=rep(0,nsteps)
T=seq(1,1000,length=nsteps)

for (i in 1:nsteps){
	PrairieMeta[i]=T[i]*Stmax/((1+d[Prairieminloc[1]]*T[i])^gamma[Prairieminloc[2]])
}

### Pond

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
      Resid[i]=abs(SObsPond[i]-SEst[i])
    }
    
    sumResid[k,j]=sum(Resid)
    
  }
}

Pondminloc=which(sumResid==min(sumResid), arr.ind=TRUE)		#finds the matrix address of the minumum value of sumResid
min(sumResid)								#prints the minimum sumResid value
Pondminloc									#prints matrix address of the minimum sumResid value
d[Pondminloc[1]]								#prints the best fit d value (associated with the first value in minloc)
gamma[Pondminloc[2]]								#prints the best fit gamma value (associated with the second value in minloc)


PondMeta=rep(0,nsteps)
T=seq(1,1000,length=nsteps)

for (i in 1:nsteps){
  PondMeta[i]=T[i]*Stmax/((1+d[Pondminloc[1]]*T[i])^gamma[Pondminloc[2]])
}

plot(DensityTRT,SObsPrairie,xlab="DensityTRT (per m2)", ylab="Number Surviving (per m2)", col="orange",lwd=3)
legend("bottomright",c("Prairie","Farm Pond", "Best Fit Prairie", "Best Fit Pond"),col=c("orange","blue","black","gray"),lwd=c(2))
lines(T,PrairieMeta,lwd=3, col="black")
lines(T,PondMeta,lwd=3, col="gray")
points(DensityTRT,SObsPond,xlab="DensityTRT (per m2)", ylab="Number Surviving (per m2)", col="blue",lwd=3)
points(DensityTRT,SObsPrairie,xlab="DensityTRT (per m2)", ylab="Number Surviving (per m2)", col="orange",lwd=3)
