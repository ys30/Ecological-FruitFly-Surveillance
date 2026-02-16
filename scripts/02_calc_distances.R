## calculating the distances between all the trees in the town

load('ntrees')
tinds = which(ntrees>0,arr.ind=TRUE) #Finds the indices of trees in the dataset (i.e., non-zero cells in ntrees).
tsize = ntrees[tinds] #Extracts the tree sizes for those indices.
tsize
table(tsize)   # Creates a frequency table of tree sizes.
plot(tinds,cex=0.1,col=rgb(0,0,0,alpha=0.1*tsize),pch=16)  #Plots the tree positions (tinds) with a size and transparency proportional to their size (tsize).

ntreecells = dim(tinds)[1]  #Calculates the total number of trees by getting the number of rows (tree positions)
tinds2 = floor(tinds/50)  #Divides the tree coordinates by 50 and floors the result
plot(tinds2)               #Plots the scaled or binned tree coordinates

alltrees = data.frame(x=tinds[,1],y=tinds[,2],x2=tinds2[,1],y2=tinds2[,2],id=1:ntreecells,size=tsize)
head(alltrees)                              #Creating a DataFrame for All Trees
save(alltrees,file='alltrees')

#### record the 400 closest for each tree
allcloseones=list()
for (i in 1:ntreecells){
	print(i)
	dists = with(alltrees,sqrt( (x-x[i])^2 + (y-y[i])^2 )*10) #Computes the Euclidean distance between the ith tree and all other trees.
	dists[i] = 9999999999
	closeones = data.frame(id=order(dists)[1:400],dist=dists[order(dists)][1:400])  #Finds the 400 trees with the smallest distances and stores their IDs and distances
	allcloseones[[i]] = closeones 
}
save(allcloseones,file='allcloseones')
load('allcloseones')
##check
str(allcloseones)
i=49000
i=900
aco=allcloseones[[i]]
aco
plot(alltrees[aco$id,]$x, alltrees[aco$id,]$y,cex=0.01)
text(alltrees[aco$id,]$x, alltrees[aco$id,]$y,1:400,cex=0.6)
text(alltrees[aco$id,]$x[1:200], alltrees[aco$id,]$y[1:200],1:200,cex=0.6)
points(alltrees[i,]$x, alltrees[i,]$y,col='red')
points(alltrees[aco$id,]$x[1:3], alltrees[aco$id,]$y[1:3],col='green')

































