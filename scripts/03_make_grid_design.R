##makes a few simple surveillance trapping designs

df=data.frame(expand.grid(x=seq(50,1000,by=100),y=seq(50,1000,by=100)))
df
write.csv(df,'design1.csv')

df=data.frame(x=runif(100)*100+500,y=runif(100)*100+500)
df
write.csv(df,'designr1.csv')

df=data.frame(x=runif(100)*1000,y=runif(100)*1000)
df
write.csv(df,'designr.csv')
