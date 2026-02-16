design=read.csv('design_grid.csv')

load('ntrees')
tinds = which(ntrees>0,arr.ind=TRUE)
tsize = ntrees[tinds]
load('alltrees')
load('introrisk')

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

points(856,650)



