#### model parameters
male.max.age = 36
luredist = 20 #the maximum distance in metres from which a lure is able to attract a fly. 
mmdd = 100  ## mean male daily distance

#################  surveillance design... set this to NULL for no surveillance
design=read.csv('design_grid.csv')  #surveillance trap design file 
#design=NULL

load('ntrees')
tinds = which(ntrees>0,arr.ind=TRUE)
tsize = ntrees[tinds]
load('alltrees')
head(alltrees)  
##loads data files at the start, including the tree locations, the tree distances, the fruit fly population estimates, and also the introduction risk map.

load('allcloseones')

load('allpleave')  #allpleave

load('introrisk')

##########################################################################
### Run these lines to get a plot of the female dispersal kernel. 
distwtfunc = function(dist){     ##the dispersal kernel 
	2^(-dist/50)                 ## A dispersal kernel is a probability density function showing how the likelihood of a fly spreading a certain distance declines as the distance increases.
}
plot(1:1000,distwtfunc(1:1000),col='red',t='l',lwd=2,xlab="Distance (m)",ylab="Weighted probability")
##########################################################################



# 1. Preparing the Risk Map
introrisk2=round(introrisk,5)          #This is a risk matrix (rounded values of introrisk) that represents the infection risk across the area.
unvals=sort(unique(as.vector(introrisk2)),decreasing=TRUE) #A sorted list in decreasing order from risk high to low.
unvals
forplotrisk = introrisk2
forplotrisk[introrisk2==unvals[1]]=1              #A modified version of introrisk2 where the highest risk areas are set to 1, and lower risks are given values like 0.75, 0.5, 0.4, and 0
forplotrisk[introrisk2==unvals[2]]=0.75
forplotrisk[introrisk2==unvals[3]]=0.5
forplotrisk[introrisk2==unvals[4]]=0.4
forplotrisk[introrisk2==unvals[5]]=0
image(x=1:1000,y=1:1000,forplotrisk)
write.csv(forplotrisk,"forplotrisk.csv")

#2. Plotting Tree Locations
alph = 0.2*tsize
alph[alph>1]=1                                   #Transparency factor based on tree size (tsize), capped at 1.
points(tinds,cex=0.1,col=rgb(0,0,0,alph),pch=16)

#3. Trap locations
points(design$x,design$y,col='purple',pch=3)
ntraps = dim(design)[1]

#4. Initial Infected Point
#image(x=1:1000,y=1:1000,introrisk)
#initincurpt = sample(length(introrisk),1000,prob=introrisk)
#introrisk[initincurpt]=2
#for (i in 1:100){
initincurpt = sample(length(introrisk),1,prob=introrisk)   #Selects an initial point for infection based on the probability distribution
initincurpt
rr = initincurpt%%nrow(introrisk)                          #Converts the linear index of initincurpt to row (rr) and column (cc) coordinates in the introrisk matrix.
rr[rr==0]=nrow(introrisk)
cc = (initincurpt-1+nrow(introrisk))%/%nrow(introrisk)
#rr=300
#cc=300
#introrisk[cbind(rr,cc)]=10

#5. Distance Calculation
dists = with(alltrees,sqrt( (x-rr)^2 + (y-cc)^2 )*10)
wts = sapply(dists,distwtfunc )                         #weighted distance for female dispersal kenel
firsttree = alltrees[sample(length(wts),1,prob=wts),]
points(firsttree$x,firsttree$y,col='blue',pch=16,cex=0.5)
#}

# 6. Infection Simulation (Over Time)
malelist = data.frame()                                 #malelist: A list to keep track of male insects that spread the infection.
infectedlist = data.frame(id=firsttree$id,day=1)        #infectedlist: A list of infected trees, starting with the firsttree and its infection day.
for (t in 2:400){                                    #time 400days
	print(t)
  #6.1. Female Spread of Infection
	szs=alltrees$size[infectedlist$id]
	days=t-infectedlist$day
	nleaversp = unlist(sapply(1:length(szs) , function(ii) allpleave[[szs[ii]]][days[ii]]))  #nleaversp: Number of female insects leaving from each infected tree, based on tree size and the number of days since infection
	nleaversp[is.na(nleaversp)]=0
	#6.2. Male Spread of Infection
	nleaversm = rpois(length(nleaversp), nleaversp)  #nleaversm: Number of male insects leaving from each infected tree, generated using a Poisson distribution.
	prodids = infectedlist$id[nleaversm>0]         #prodids: Trees that are producing male insects.
	nleaversm = nleaversm[nleaversm>0]
	ntreesprod = length(prodids)
	if (ntreesprod>0){
		newmales = data.frame(x=rep(alltrees$x[prodids],nleaversm) , y=rep(alltrees$y[prodids],nleaversm), 
			age=0) 
		malelist = rbind(malelist , newmales )
	}
	#6.3. Male Insect Movement
	malelist$age=malelist$age+1
	malelist=subset(malelist,age<=male.max.age)
	nmales=dim(malelist)[1]
	mdist = rnorm(nmales)*mmdd/0.7977/10
	a = runif(nmales)*pi*2
	malelist$x=malelist$x+mdist*cos(a)
	malelist$y=malelist$y+mdist*sin(a)
	#6.4. Female Insect Spread
	nleavers = rpois(length(nleaversp), nleaversp)
	prodids = infectedlist$id[nleavers>0]
	#producers = alltrees[,prodids]
	nleavers = nleavers[nleavers>0]
	ntreesprod = length(prodids)
	if (ntreesprod>0) for (ii in 1:ntreesprod ){
		wts=sapply( allcloseones[[prodids[ii]]]$dist , distwtfunc )
		nexttreesids = allcloseones[[prodids[ii]]]$id[sample(length(wts),nleavers[ii],replace=TRUE,prob=wts)]
		#points(nexttrees$x,nexttrees$y,col='green',pch=16,cex=0.9)
		nexttreesids = setdiff(nexttreesids,infectedlist$id )
		if (length(nexttreesids)>0) infectedlist = rbind( infectedlist , data.frame(id=nexttreesids ,day=t))
	}
	infectedtrees = alltrees[infectedlist$id,]
	#7. plot Visualization
	#plot(malelist$x,malelist$y,col='darkgreen',pch=16,cex=0.05,xlim=c(0,1000),ylim=c(0,1000))
	points(malelist$x,malelist$y,col='darkgreen',pch=16,cex=0.05)
	points(infectedtrees$x,infectedtrees$y,col='green',pch=16,cex=0.3)
	### plot to file
	if (TRUE & t%%10==0){
		png(paste('outfig',t+1000,'.png'))
		image(x=1:1000,y=1:1000,forplotrisk)
		alph = 0.2*tsize
		alph[alph>1]=1
		points(tinds,cex=0.1,col=rgb(0,0,0,alph),pch=16)
		points(design$x,design$y,col='purple',pch=3)                            ## the purple crosses are traps 
		points(malelist$x,malelist$y,col='darkgreen',pch=16,cex=0.05)
		points(infectedtrees$x,infectedtrees$y,col='green',pch=16,cex=0.3)
		dev.off()
	}
	###### 8. surveillance with Traps
	if (!is.null(design) & nmales>0){
		dzs=sapply(1:ntraps, function(i) (design$x[i]-malelist$x)^2 + (design$y[i]-malelist$y)^2 )         #dzs: Computes the distance between each trap and every male insect.
		if (min(sapply(1:ntraps, function(i) (design$x[i]-malelist$x)^2 + (design$y[i]-malelist$y)^2 )) < (luredist/10)^2) {
			dzs=matrix(dzs,ncol=ntraps)
			trapi=which(dzs==min(dzs),arr.ind=TRUE)[2]
			print(paste("DETECTED at day:",t))
			points(design$x[trapi],design$y[trapi],col='cyan',pch=16)
			break
	}}

}
































