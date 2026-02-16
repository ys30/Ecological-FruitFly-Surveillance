setwd("C://Users//tangl//Desktop//20240919//1//1//model w males 2020")

##  estimate how many female flies are likely to be leaving an infested fruit tree 
  ## on any given day after the first female fly arrives at the tree.

allpleave=list()
for (ntrees in c(1:10)){        #ntrees: Number of trees being considered. The loop runs for different numbers of trees, from 1 to 10.

pop=data.frame(ls=4,ttg=5)
# 1 egg, 2 grub, 3 pupa, 4 fly   #ls: Life stage of the fly. 1 = egg, 2 = grub, 3 = pupa, 4 = fly.
                                 #ttg: Time to grow, the number of days remaining in the current life stage.
meandaysinls = c(4,15,13,20)     #meandaysinls: A vector containing the average number of days spent in each life stage.
femeggsperday = 2                #femeggsperday: Number of eggs laid by each female fly per day.

arec=NULL                        #arec: Tracks the number of flies that remain in the population and are in the adult stage (life stage 4) each day.
leaverrec=NULL                   #leaverrec: Keeps track of the number of female flies that leave each day.
resources = 250*ntrees           #resources: Total resources available, fruits maybe. The more trees there are, the more resources are available
for (t in 1:120){                #For each tree count (ntrees), the simulation runs for 120 days (t in 1:120)
	pop$ttg=pop$ttg-1
	pop$ls[pop$ttg<1]=pop$ls[pop$ttg<1]+1
	pop$ttg[pop$ttg<1]=meandaysinls[pop$ls[pop$ttg<1]]
	nnew = sum(pop$ls==4)*femeggsperday     #New eggs are laid by flies in life stage 4 (adults).
	if (nnew>resources){
		nnew=resources
	}
	resources = resources - nnew  #resources that remains not colonized by the flies
	newpop = data.frame(ls=rep(1,nnew), ttg = rep(meandaysinls[1],nnew) )  #new egg start in 1 egg status
	pop=rbind(pop,newpop)
	pop=subset(pop,ls<5)   #ls 1-4
	nleavers = 0
	if (resources==0) {   #If resources run out (i.e., resources == 0), flies in life stage 4 are forced to leave (nleavers)
		nleavers = length(pop$ls[pop$ls==4])
		pop=subset(pop,ls<4)
		}
	leaverrec=c(leaverrec,nleavers)
	arec=c(arec,dim(subset(pop,ls==4))[1])
}

plot(leaverrec,ylab='N female flies leaving',xlab='days',type='b')
points(arec,col='red')

pleave = leaverrec+arec*0.05   #adjust for leave numbers to pleave 
pleave[1:10]=0                 #The first 10 values of pleave are set to 0, implying that no flies leave in the initial days.
#pleave[pleave==0]=0.001

allpleave[[ntrees]]=pleave     #Stores the pleave vector for each value of ntrees
}
save(allpleave,file='allpleave')



##########################################
## plot spread predictions
## ps=dcauchy(0:1000,0,100)
## plot(ps,t='l',xlab='distance (m)',ylab='probability',col='red',lwd=2)

###### plot population predictions
ntrees=4
plot(allpleave[[ntrees]]+0.0001,log='y',ylab='Expected Number',xlab='days',type='b',ylim=c(0.1,500))
##+0.0001 to avoid warning















