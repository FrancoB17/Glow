#flow direction preprocessor




#################################### Flow direction DDM30################################
# With this script I prepare the VIC DDM30 flow direction file received from Michelle van Vliet for further use
# I first add rows and columns to top and bottom to generate a file with compatible extent to my own files
# then I change the flow direction specifications to the specification I use
# the changed file is then used to perform the flow accumulation function in Arcgis

library("sp")
library("raster")
library("maptools")

FlowDirectionDDM30 <- (readAsciiGrid("D:/LuciePhD/Data/VIC/Michelle/ddm30_flowdir_cru_nevaVIC_ARCGIS.asc"))
FlowDirectionMatrix <- as.matrix(FlowDirectionDDM30)
FDMT <-t(FlowDirectionMatrix)

#this matrix has an incorrect number of rows, 280 instead of 360
# we will therefore add rows on top and below to generate a compatible file
m1<-matrix(NA, nrow=12,ncol=720)
m2<-matrix(NA, nrow=68,ncol=720)


#test order of rbind function
m3<-matrix(3,nrow=2,ncol=2)
m4<-matrix(5,nrow=2,ncol=2)
m5<-rbind(m3,m4)

#do actual rbind
tmp1<-rbind(m1,FDMT)
tmp2<-rbind(tmp1,m2)

#now I change the flow direction of the VIC DDM30 file to the format that Arcgis uses
# this I need to subsequently apply the function Flow Accumulation in Arcgis
# with flow accumulation I can create the flow accumulation grid that I need for my routing model

#Flow direction specification (as needed by Arcgis)
#+---+----+-----+
#|32 | 64 | 128 |
#+---+----+-----+
#|16 |    |  1  |
#+---+----+-----+
#|8  | 4  |  2  |
#+---+----+-----+

#Flow direction specification (as in VIC DDM30 file)
#+---+----+-----+
#| 8 |  1 |  2  |
#+---+----+-----+
#| 7 |    |  3  |
#+---+----+-----+
#| 6 | 5  |  4  |
#+---+----+-----+

#watch the order in which you change this...
h<-which(tmp2==8)
tmp2[h]<-32

a<-which(tmp2==1)
tmp2[a]<-64

b<-which(tmp2==2)
tmp2[b]<-128

c<-which(tmp2==3)
tmp2[c]<-1

d<-which(tmp2==4)
tmp2[d]<-2

e<-which(tmp2==5)
tmp2[e]<-4

f<-which(tmp2==6)
tmp2[f]<-8

g<-which(tmp2==7)
tmp2[g]<-16


tmp3<-raster(tmp2, xmn=-180,xmx=180,ymn=-90,ymx=90)

writeRaster(tmp3,"D:/LuciePhD/Data/VIC/Michelle/FlowDirectionDDM30preprocessed.asc",format="ascii", overwrite=TRUE)
