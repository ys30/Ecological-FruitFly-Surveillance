nruns=100

#### model parameters
male.max.age = 36
luredist = 20		# metres
mmdd = 300  ## mean male daily distance

### notes on distance units... 
## the units on the map are 'pixels', which are 10m by 10m
## units in alltrees and the design are also in pixels, so multiply by 10m to get metres
## simulation is run on pixels, so units in metres are divided by 10 in the code

#################  surveillance design... 
designname = 'design_grid'
design=read.csv(paste(designname,'.csv',sep=''))


load('ntrees')
tinds = which(ntrees>0,arr.ind=TRUE)
tsize = ntrees[tinds]
load('alltrees')
head(alltrees)

load('allcloseones')

load('allpleave')  #allpleave

load('introrisk')

##########################################################################
### Run these lines to get a plot of the female dispersal kernel. 
distwtfunc = function(dist){
	2^(-dist/50)
}
plot(1:1000,distwtfunc(1:1000),col='red',t='l',lwd=2,xlab="Distance (m)",ylab="Weighted probability")
##########################################################################




introrisk2=round(introrisk,5)
unvals=sort(unique(as.vector(introrisk2)),decreasing=TRUE)
forplotrisk = introrisk2
forplotrisk[introrisk2==unvals[1]]=1
forplotrisk[introrisk2==unvals[2]]=0.75
forplotrisk[introrisk2==unvals[3]]=0.5
forplotrisk[introrisk2==unvals[4]]=0.4
forplotrisk[introrisk2==unvals[5]]=0
image(x=1:1000,y=1:1000,forplotrisk)
alph = 0.2*tsize
alph[alph>1]=1
points(tinds,cex=0.1,col=rgb(0,0,0,alph),pch=16)

points(design$x,design$y,col='purple',pch=3)
ntraps = dim(design)[1]

allres=NULL
for (run in 1:nruns){

#image(x=1:1000,y=1:1000,introrisk)
#initincurpt = sample(length(introrisk),1000,prob=introrisk)
#introrisk[initincurpt]=2
#for (i in 1:100){
initincurpt = sample(length(introrisk),1,prob=introrisk)
rr = initincurpt%%nrow(introrisk) 
rr[rr==0]=nrow(introrisk)
cc = (initincurpt-1+nrow(introrisk))%/%nrow(introrisk)
#introrisk[cbind(rr,cc)]=10
dists = with(alltrees,sqrt( (x-rr)^2 + (y-cc)^2 )*10)	##multiply by 10 to covert to metres
wts = sapply(dists,distwtfunc )
firsttree = alltrees[sample(length(wts),1,prob=wts),]
points(firsttree$x,firsttree$y,col='blue',pch=16,cex=0.5)
#}


malelist = data.frame()
infectedlist = data.frame(id=firsttree$id,day=1)
for (t in 2:400){
	print(t)
	szs=alltrees$size[infectedlist$id]
	days=t-infectedlist$day
	nleaversp = unlist(sapply(1:length(szs) , function(ii) allpleave[[szs[ii]]][days[ii]]))
	nleaversp[is.na(nleaversp)]=0
	### males
	nleaversm = rpois(length(nleaversp), nleaversp)
	prodids = infectedlist$id[nleaversm>0]
	nleaversm = nleaversm[nleaversm>0]
	ntreesprod = length(prodids)
	if (ntreesprod>0){
		newmales = data.frame(x=rep(alltrees$x[prodids],nleaversm) , y=rep(alltrees$y[prodids],nleaversm), 
			age=0) 
		malelist = rbind(malelist , newmales )
		}
	malelist$age=malelist$age+1
	malelist=subset(malelist,age<=male.max.age)
	nmales=dim(malelist)[1]
	mdist = rnorm(nmales)*mmdd/0.7977/10
	a = runif(nmales)*pi*2
	malelist$x=malelist$x+mdist*cos(a)
	malelist$y=malelist$y+mdist*sin(a)
	### females
	nleavers = rpois(length(nleaversp), nleaversp)
	prodids = infectedlist$id[nleavers>0]
	#producers = alltrees[,prodids]
	nleavers = nleavers[nleavers>0]
	ntreesprod = length(prodids)
	if (ntreesprod>0) for (ii in 1:ntreesprod ){
		wts=sapply( allcloseones[[prodids[ii]]]$dist , distwtfunc )	## this distance already in metres
		nexttreesids = allcloseones[[prodids[ii]]]$id[sample(length(wts),nleavers[ii],replace=TRUE,prob=wts)]
		#points(nexttrees$x,nexttrees$y,col='green',pch=16,cex=0.9)
		nexttreesids = setdiff(nexttreesids,infectedlist$id )
		if (length(nexttreesids)>0) infectedlist = rbind( infectedlist , data.frame(id=nexttreesids ,day=t))
	}
	infectedtrees = alltrees[infectedlist$id,]
	###### surveillance
	if (!is.null(design) & nmales>0){
		dzs=sapply(1:ntraps, function(i) (design$x[i]-malelist$x)^2 + (design$y[i]-malelist$y)^2 )
		if (min(dzs) < (luredist/10)^2) {		## dividing by 10 converts to pixels
			dzs=matrix(dzs,ncol=ntraps)
			trapi=which(dzs==min(dzs),arr.ind=TRUE)[2]
			print(paste("run",run,"DETECTED at time:",t))
			points(design$x[trapi],design$y[trapi],col='cyan',pch=16)
			allres=rbind(allres,c(t,trapi,nmales,dim(infectedlist)[1]))
			break
	}}

}}

allres=data.frame(allres)
names(allres)=c('time','trap.id','nmales','ntrees')

allres
write.csv(allres, paste('results ', designname,'.csv',sep=''))



























