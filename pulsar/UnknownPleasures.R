#Recreate Unknown Pleasures (Joy Division) by Peter Saville as a 3D plot
#Author: Christopher Steven Marcum
# <cmarcum@uci.edu> @csmarcum on twitter and @cmarcum on github
#Date: 1 August 2019

#Notes:
# PSR B1919+21 is a radio pulsar first discovered by 
# Inspired by the annivserary of the Unknown Pleasures album cover of Joy Division
# https://phys.org/news/2019-07-joy-division-years-unknown-pleasures.html
# And adapted from work previously done by:
# datawookie at https://www.r-bloggers.com/recreating-unknown-pleasures-graphic/
# Data sourced from Borgar at https://github.com/borgar

#Acquire data
pulsar<-read.csv("https://gist.githubusercontent.com/borgar/31c1e476b8e92a11d7e9/raw/0fae97dab6830ecee185a63c1cee0008f6778ff6/pulsar.csv",head=FALSE,colClasses="numeric")
pulsar<-as.matrix(pulsar)

#Use rgl and some pretty colors
require(rgl)
nbcol<-prod(dim(pulsar))
rcol<-rev(rainbow(nbcol,start=0,end=1))
zcol<-cut(pulsar,nbcol)
zcol2<-cut(pulsar,nbcol)
persp3d(x=1:nrow(pulsar),y=1:ncol(pulsar),z=pulsar,col=rcol[zcol2],aspect=c(1,1,1),smooth=FALSE,las=1,xlab="",ylab="",zlab="",axes=FALSE)

#I did not retain my original par3d() settings so just play around with interactive
# plot to obtain the perspective you want.

rgl.snapshot("pulsar.png")
