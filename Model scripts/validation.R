# This script is used for validation of the crypto concentrations model
# developed by Lucie May 2017
library(sp)
library(raster)
library(NADA)
library(fitdistrplus)
library(ggplot2)
library(survival)
library(plyr)
library(hydroGOF)

tables.path<-"D:/LuciePhD/Data/Microbial data/PROCESSED/Global_crypto_validation4/"
setwd(tables.path)

data<-read.csv("GloWPa_validation_final_lakesremoved.csv")

#create TRUE FALSE vector for censored data
data$censored<-ifelse(is.na(data$OOCYSTS_L),TRUE,FALSE)

#create a dummy TRUE FALSE vector for the modelled data
#this one is FALSE everywhere, since there is a value everywhere
data$censored_modelled<-FALSE


# where OOCYSTS_CO is NA it should contain the value of the detection limit 
# in order to be compatible with later analysis
#first check how many NAs there are
countNA <- length(which(is.na(data$OOCYSTS_CO)))
countNA

for(i in 1:length(data$OOCYSTS_CO)){
  if(is.na(data$OOCYSTS_CO[i])){
    data$OOCYSTS_CO[i]<-data$DETECT_LIM[i]
  } else {
    data$OOCYSTS_CO[i]<-data$OOCYSTS_CO[i]
  }

}

#check again how many NAs
countNA <- length(which(is.na(data$OOCYSTS_CO)))
countNA



#fix -9999 that erroneously appear in model output column (ARcGIS code for NA)
data$RASTERVALU[data$RASTERVALU==-9.999]<-NA #50 values
#convert to log scale for plotting
data$predictedlog<-log10(data$RASTERVALU)
data$observedlog<-log10(data$OOCYSTS_CO)
data$month_string<-NA
for(c in 1:12){
  mo<-which(data$MONTH==c)
  if(c<10){
  data$month_string[mo]<-paste0("M0",c)
  } else{
    data$month_string[mo]<-paste0("M",c)
  }
}


# Dennis Helsel says:
# You cannot compare pairwise
# You can fit a distribution to both, with mle for example and compare characteristics of the distribution
# Or you can do a regression, with cenreg for example
# With the latter your predicted should be X, as X cannot have censored values

### trying out cenreg, this works! 
# but there is a problem where both a value and a detection limit are missing...
#hmm doesn't work now... why?
b<-with(data, cenreg(Cen(OOCYSTS_CO, censored)~RASTERVALU))
b

#observed op y as plot
with(data, cenxyplot(log10(RASTERVALU), censored_modelled, log10(OOCYSTS_CO),censored,col="red")) # x vs. y
abline(0,1)

cencolors<-c("darkgrey","blue","dodgerblue", "red4", "green3", "gold", "darkorange","red1", "purple","pink","black")
data$colors<-NA
data$colors<-cencolors[data$COUNTRY]

#trying to group colors
with(data, cenxyplot(log10(RASTERVALU), censored_modelled, log10(OOCYSTS_CO),censored,col=cencolors,pch=16)) # x vs. y
abline(0,1)

#trying to group colors, not censored
with(data, plot(log10(RASTERVALU), log10(OOCYSTS_CO),col=colors,pch=16)) # x vs. y
abline(0,1)


#observed op x as plot
with(data, cenxyplot(log10(OOCYSTS_CO),censored,log10(RASTERVALU), censored_modelled)) 
abline(0,1)

# er is een functie cenboxplot, compare the following
boxplot(data$observedlog~data$COUNTRY,col=cencolors)
cenboxplot(obs=data$OOCYSTS_CO,cen=data$censored,group=data$COUNTRY,col=cencolors)
# er is een functie cenboxplot, compare the following
boxplot(data$predictedlog~data$COUNTRY,col=cencolors)
cenboxplot(obs=data$RASTERVALU,cen=data$censored_modelled,group=data$COUNTRY,col=cencolors)


################### making country data #########################
countries<-unique(data$COUNTRY)
countries<-as.vector(countries)
months<-c(1:12)
#create vectors of the indices of the different countries and months
for(c in 1:length(countries)){
  
  assign(countries[c],which(data$COUNTRY==countries[c]))
  
  for(m in 1:12){ 
    assign(paste0(countries[c],"_",months[m]),which(data$COUNTRY==countries[c] & data$MONTH==months[m]))
    
  }
}

assign("data_belgium",data[Belgium,])
assign("data_brazil",data[Brazil,])
assign("data_canada",data[Canada,])
assign("data_france",data[France,])
assign("data_germany",data[Germany,])
assign("data_japan",data[Japan,])
assign("data_luxembourg",data[Luxembourg,])
assign("data_netherlands",data[Netherlands,])
assign("data_newzealand",data[`New Zealand`,])
assign("data_thailand",data[Thailand,])
assign("data_us",data[`United States`,])

countries2<-c("belgium", "brazil", "canada", "france", "germany", "japan", "luxembourg", "netherlands", "newzealand", "thailand", "us")

write.csv(data_belgium,file="data_belgium.csv")
write.csv(data_brazil,file="data_brazil.csv")
write.csv(data_canada,file="data_canada.csv")
write.csv(data_france,file="data_france.csv")
write.csv(data_germany,file="data_germany.csv")
write.csv(data_japan,file="data_japan.csv")
write.csv(data_luxembourg,file="data_luxembourg.csv")
write.csv(data_netherlands,file="data_netherlands.csv")
write.csv(data_newzealand,file="data_newzealand.csv")
write.csv(data_thailand,file="data_thailand.csv")
write.csv(data_us,file="data_us.csv")


######################## country data descriptive stats and performance stats ########################################

countrydata<-data.frame(countries)
countrydata$N<-NA
countrydata$N_locs1<-NA
countrydata$N_locs2<-NA
countrydata$N_locs3<-NA
countrydata$detectpercent<-NA
countrydata$detectmin<-NA
countrydata$detectmean<-NA
countrydata$detectmax<-NA
countrydata$recoverypercent<-NA
countrydata$recoverymin<-NA
countrydata$recoverymean<-NA
countrydata$recoverymax<-NA

countrydata$pearson<-NA
countrydata$spearman<-NA
countrydata$kendall<-NA
countrydata$index_agreement<-NA
countrydata$RMSE<-NA
countrydata$NSE<-NA

countrydata$median<-NA
countrydata$mean<-NA
countrydata$percentile5<-NA
countrydata$percentile25<-NA
countrydata$percentile75<-NA
countrydata$percentile95<-NA
countrydata$median_pred<-NA
countrydata$mean_pred<-NA
countrydata$percentile5_pred<-NA
countrydata$percentile25_pred<-NA
countrydata$percentile75_pred<-NA
countrydata$percentile95_pred<-NA

datalist<-list(data_belgium,data_brazil,data_canada,data_france,data_germany,data_japan,data_luxembourg,data_netherlands,data_newzealand,data_thailand,data_us)

par(mfrow=c(4,3),mar=c(3, 1, 1, 1),mgp=c(3,1,0))
for(c in 1:11){
  datatest<-datalist[c]
  countrydata$N[c]<-with(datatest[[1]],length(COUNTRY))
  countrydata$N_locs1[c]<-with(datatest[[1]],length(unique(NAME)))
  countrydata$N_locs2[c]<-with(datatest[[1]],length(unique(LATITUDE)))
  countrydata$N_locs3[c]<-with(datatest[[1]],length(unique(LONGITUDE)))
  countrydata$detectpercent[c]<-with(datatest[[1]],1-(sum(censored==TRUE)/length(censored)))
  countrydata$detectmin[c]<-with(datatest[[1]],min(DETECT_LIM))
  countrydata$detectmean[c]<-with(datatest[[1]],mean(DETECT_LIM))
  countrydata$detectmax[c]<-with(datatest[[1]],max(DETECT_LIM))
  countrydata$recoverypercent[c]<-with(datatest[[1]],sum(!is.na(RECOVERY))/length(RECOVERY))
  countrydata$recoverymin[c]<-with(datatest[[1]],min(RECOVERY,na.rm=TRUE))
  countrydata$recoverymean[c]<-with(datatest[[1]],mean(RECOVERY,na.rm=TRUE))
  countrydata$recoverymax[c]<-with(datatest[[1]],max(RECOVERY,na.rm=TRUE))
  
  countrydata$pearson[c]<-with(datatest[[1]],cor(predictedlog,observedlog, use="complete.obs",method="pearson"))
  countrydata$spearman[c]<-with(datatest[[1]],cor(predictedlog,observedlog, use="complete.obs",method="spearman"))
  countrydata$kendall[c]<-with(datatest[[1]],cor(predictedlog,observedlog, use="complete.obs",method="kendall"))
  countrydata$index_agreement[c]<-with(datatest[[1]],d(predictedlog,observedlog, na.rm=TRUE))
  countrydata$RMSE[c]<-with(datatest[[1]],rmse(predictedlog,observedlog, na.rm=TRUE))
  countrydata$NSE[c]<-with(datatest[[1]],NSE(predictedlog,observedlog, na.rm=TRUE))  
  

  mle<-with(datatest[[1]],cenmle(OOCYSTS_CO,censored))
  countrydata$median[c]<-median(mle)
  #this mean function is really weird for NZ, don't trust outcomes
  countrydata$mean[c]<-mean(mle)
  countrydata$percentile5[c]<-quantile(mle,probs=0.05)
  countrydata$percentile25[c]<-quantile(mle,probs=0.25)  
  countrydata$percentile75[c]<-quantile(mle,probs=0.75)
  countrydata$percentile95[c]<-quantile(mle,probs=0.95)
  
  countrydata$median_pred[c]<-with(datatest[[1]],median(RASTERVALU,na.rm=TRUE))
  countrydata$mean_pred[c]<-with(datatest[[1]],mean(RASTERVALU,na.rm=TRUE))
  countrydata$percentile5_pred[c]<-with(datatest[[1]],quantile(RASTERVALU,probs=0.05,na.rm=TRUE))
  countrydata$percentile25_pred[c]<-with(datatest[[1]],quantile(RASTERVALU,probs=0.25,na.rm=TRUE))
  countrydata$percentile75_pred[c]<-with(datatest[[1]],quantile(RASTERVALU,probs=0.75,na.rm=TRUE))
  countrydata$percentile95_pred[c]<-with(datatest[[1]],quantile(RASTERVALU,probs=0.95,na.rm=TRUE))

  #trying to group colors
  with(datatest[[1]], cenxyplot(log10(RASTERVALU), censored_modelled, log10(OOCYSTS_CO),censored,col=colors,pch=16)) # x vs. y
  abline(0,1)
  #you can ignore the warnings that the mean function produces
}
par(mgp=c(3,-1,1),mfrow=c(1,1))
cenboxplot(obs=data$OOCYSTS_CO,cen=data$censored,group=data$COUNTRY,col=cencolors,cex.lab=0.3,las=3)


plot(log10(countrydata$median),log10(countrydata$median_pred))
abline(0,1)

plot(log10(countrydata$mean),log10(countrydata$mean_pred))
abline(0,1)

write.csv(countrydata,file="countrydata.csv")

### calculating performance statistics for all data together ####
# this is all on the logtransformed data where the detection limit is put in place of a missing value
# check https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf
# calculating these per location gives really bad results --> much worse than stats on all data together
# indicating that the variability is high
# but locations are distinguishable from one another


# correlation coefficient
pearson<-cor(data$predictedlog, data$observedlog, use="complete.obs",method="pearson")
spearman<-cor(data$predictedlog, data$observedlog, use="complete.obs",method="spearman")
kendall<-cor(data$predictedlog, data$observedlog, use="complete.obs",method="kendall")
#pearson2<-cor(data$RASTERVALU, data$OOCYSTS_CO, use="complete.obs",method="pearson")
#spearman2<-cor(data$RASTERVALU, data$OOCYSTS_CO, use="complete.obs",method="spearman")
#kendall2<-cor(data$RASTERVALU, data$OOCYSTS_CO, use="complete.obs",method="kendall")

#index of agreement (between 0 and 1, 1 is best agreement)
index_agreement<-d(data$predictedlog, data$observedlog, na.rm=TRUE)

# Root mean squared error
RMSE<-rmse(data$predictedlog, data$observedlog, na.rm=TRUE)

#nash suthcliffe efficiency, between -Inf and 1
#1 is perfect agreement, 0 is model is equally good predictor as mean
# <0 is mean is better predictor than model
NSE<-NSE(data$predictedlog, data$observedlog, na.rm=TRUE)

# same calculations, but
# exclude japan row 1954-2509
data_nojapan1<-data[1:1953,]
data_nojapan2<-data[2510:4338,]
data_nojapan<-rbind(data_nojapan1,data_nojapan2)

pearson_nojapan<-cor(data_nojapan$predictedlog, data_nojapan$observedlog, use="complete.obs",method="pearson")
spearman_nojapan<-cor(data_nojapan$predictedlog, data_nojapan$observedlog, use="complete.obs",method="spearman")
kendall_nojapan<-cor(data_nojapan$predictedlog, data_nojapan$observedlog, use="complete.obs",method="kendall")
index_agreement_nojapan<-d(data_nojapan$predictedlog, data_nojapan$observedlog, na.rm=TRUE)
RMSE_nojapan<-rmse(data_nojapan$predictedlog, data_nojapan$observedlog, na.rm=TRUE)

##### nieuw data frame voor boxplot geordend #####

data_boxplot<-data.frame(data$COUNTRY,data$OOCYSTS_CO,data$observedlog,data$censored,data$RASTERVALU,data$predictedlog,data$censored_modelled)
#data_boxplot2<-data.frame(data$COUNTRY,data$RASTERVALU,data$predictedlog,data$censored_modelled)
data_b<-rbind(data_boxplot,data_boxplot)
data_b$data.RASTERVALU[1:4338]<-NA
data_b$data.predictedlog[1:4338]<-NA
data_b$data.censored_modelled[1:4338]<-NA
data_b$data.OOCYSTS_CO[4339:8676]<-NA
data_b$data.observedlog[4339:8676]<-NA
data_b$data.censored[4339:8676]<-NA
data_b$COUNTRY_2<-NA
data_b$COUNTRY_2[1:4338]<-paste0(data_b$data.COUNTRY[1:4338],"_obs")
data_b$COUNTRY_2[4339:8676]<-paste0(data_b$data.COUNTRY[4339:8676],"_prd")
data_b$value<-NA
data_b$cens<-NA
data_b$value[1:4338]<-data_b$data.OOCYSTS_CO[1:4338]
data_b$value[4339:8676]<-data_b$data.RASTERVALU[4339:8676]
data_b$cens[1:4338]<-data_b$data.censored[1:4338]
data_b$cens[4339:8676]<-data_b$data.censored_modelled[4339:8676]

cencolors2<-c("grey25","grey25","blue","blue","dodgerblue", "dodgerblue","red4", "red4", "green3","green3", "gold","gold", "darkorange","darkorange","red1","red1", "purple","purple","pink","pink","grey75","grey75")
cenboxplot(obs=data_b$value,cen=data_b$cens,group=data_b$COUNTRY_2,col=cencolors2)

#par(mfrow=c(1,2))
#boxplot(data_b$data.observedlog~data_b$data.COUNTRY,col=cencolors)
#cenboxplot(obs=data_b$data.OOCYSTS_CO,cen=data_b$data.censored,group=data$COUNTRY,col=cencolors)
#default c(bottom, left, top, right) par(mar= c(5, 4, 4, 2) + 0.1)

tiff("boxplot.tiff",pointsize=10,width=20, height=15, units = "cm", res=200)
par(mar= c(10, 4, 4, 2) + 0.1)
cenboxplot(obs=data_b$value,cen=data_b$cens,group=data_b$COUNTRY_2,
           col=cencolors2,
           ylab="Oocysts/L",
           yaxt="n",las=3)
axis(2, at=c(1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-8,3,by=1))
dev.off()

tiff("boxplot_lowres.tiff",pointsize=10,width=20, height=15, units = "cm", res=100)
par(mar= c(10, 4, 4, 2) + 0.1)
cenboxplot(obs=data_b$value,cen=data_b$cens,group=data_b$COUNTRY_2,
           col=cencolors2,
           ylab="Oocysts/L",
           yaxt="n",las=3)
axis(2, at=c(1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-8,3,by=1))
dev.off()

tiff("boxplot_notcensored.tiff",pointsize=10,width=20, height=15, units = "cm", res=200)
par(mar= c(10, 4, 4, 2) + 0.1)
boxplot(log10(data_b$value)~data_b$COUNTRY_2,
           col=cencolors2,
           ylab="Oocysts/L",
           yaxt="n",las=3)
axis(2, at=seq(-8,3,by=1),labels=seq(-8,3,by=1))
dev.off()

#Same, but with for Japan only the positives included
#select the nondetects in the japanese data, and remove from the dataframe
jpos<-which(data_b$data.censored==TRUE & data_b$data.COUNTRY=="Japan")
jpos2<-jpos+4338
data_b_jpos<-data_b[-jpos2,]
data_b_jpos<-data_b_jpos[-jpos,]

tiff("boxplot_jpos.tiff",pointsize=10,width=20, height=15, units = "cm", res=200)
par(mar= c(10, 4, 4, 2) + 0.1)
cenboxplot(obs=data_b_jpos$value,cen=data_b_jpos$cens,group=data_b_jpos$COUNTRY_2,
           col=cencolors2,
           ylab="Oocysts/L",
           yaxt="n",las=3)
axis(2, at=c(1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-8,3,by=1))
dev.off()



###### proberen: categories plot ###########

data$who_predicted<-NA
data$who_observed<-NA

for(who in 1:length(data$who_predicted)){
  #observed
  if(data$OOCYSTS_CO[who]<=0.005){
    data$who_observed[who]<-1
  }
  if(data$OOCYSTS_CO[who]>0.005 && data$OOCYSTS_CO[who]<=0.05){
    data$who_observed[who]<-2
  }
  if(data$OOCYSTS_CO[who]>0.05 && data$OOCYSTS_CO[who]<=0.5){
    data$who_observed[who]<-3
  }
  if(data$OOCYSTS_CO[who]>0.5 && data$OOCYSTS_CO[who]<=5){
    data$who_observed[who]<-4
  }   
  if(data$OOCYSTS_CO[who]>5 && data$OOCYSTS_CO[who]<=50){
    data$who_observed[who]<-5
  }   
  if(data$OOCYSTS_CO[who]>50){
    data$who_observed[who]<-6
  }   
 #predicted
  if(is.na(data$RASTERVALU[who])){
    data$who_predicted[who]<-NA
  } else {
  if(data$RASTERVALU[who]<=0.005){
    data$who_predicted[who]<-1
  }
  if(data$RASTERVALU[who]>0.005 && data$RASTERVALU[who]<=0.05){
    data$who_predicted[who]<-2
  }
  if(data$RASTERVALU[who]>0.05 && data$RASTERVALU[who]<=0.5){
    data$who_predicted[who]<-3
  }
  if(data$RASTERVALU[who]>0.5 && data$RASTERVALU[who]<=5){
    data$who_predicted[who]<-4
  }   
  if(data$RASTERVALU[who]>5 && data$RASTERVALU[who]<=50){
    data$who_predicted[who]<-5
  }   
  if(data$RASTERVALU[who]>50){
    data$who_predicted[who]<-6
  }   
  }
}

data$who_difference<-data$who_predicted-data$who_observed

plot(data$who_observed,data$who_predicted)
hist(data$who_difference)

write.csv(data,file="GloWPa_validation_Lucie_processed.csv")


# plotting while splitting for months works only for some of these countries
# it works for: Thailand, Japan,France,Canada,Brazil
# not entirely sure what the error gives in the others, maybe too little data, or NA values where
# there should actually be a detection limit put in place


##Brazil
 temp<-read.csv(paste0("data_",countries2[2],".csv"))
 tiff("Brazil.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[2],group=(temp$month_string),yaxt="n",
           ylim=c(1e-5,1e3),xlab="Observed")
    axis(2, at=c(1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-5,3,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-5,3),xlab="Predicted")
 axis(2, at=seq(-5,3,by=1),labels=seq(-5,3,by=1))
 dev.off()

##Canada
 temp<-read.csv(paste0("data_",countries2[3],".csv"))
 tiff("Canada.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[3],group=(temp$month_string),yaxt="n",
            ylim=c(1e-5,1e2),xlab="Observed")
 axis(2, at=c(1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-5,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-5,2),xlab="Predicted")
 axis(2, at=seq(-5,2,by=1),labels=seq(-5,2,by=1))
 dev.off()
 
 ##France
 temp<-read.csv(paste0("data_",countries2[4],".csv"))
 tiff("France.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[4],group=(temp$month_string),yaxt="n",
            ylim=c(1e-7,1e2),xlab="Observed")
 axis(2, at=c(1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-7,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-7,2),xlab="Predicted")
 axis(2, at=seq(-7,2,by=1),labels=seq(-7,2,by=1))
 dev.off()
 
 ##Japan
 temp<-read.csv(paste0("data_",countries2[6],".csv"))
 tiff("Japan.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[6],group=(temp$month_string),yaxt="n",
            ylim=c(1e-4,1e3),xlab="Observed")
 axis(2, at=c(1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-4,3,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-4,3))
 axis(2, at=seq(-4,3,by=1),labels=seq(-4,3,by=1),xlab="Predicted")
 dev.off()
 
 ##Thailand
 temp<-read.csv(paste0("data_",countries2[10],".csv"))
 tiff("Thailand.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[10],group=(temp$month_string),yaxt="n",
            ylim=c(1e-5,1e1),xlab="Observed")
 axis(2, at=c(1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1),labels=seq(-5,1,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-5,1))
 axis(2, at=seq(-5,1,by=1),labels=seq(-5,1,by=1), xlab="Predicted")
 dev.off()
 
## Belgium 
 tiff("Belgium.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[1],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[1],group=(temp$month_string),yaxt="n",
            ylim=c(1e-3,1e3),xlab="Observed")
 axis(2, at=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-3,3,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-3,3), xlab="Predicted")
 axis(2, at=seq(-3,3,by=1),labels=seq(-3,3,by=1))
 dev.off()
 
 
 ## Germany
 tiff("Germany.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[5],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[5],group=(temp$month_string),yaxt="n",
            ylim=c(1e-4,1e3),xlab="Observed")
 axis(2, at=c(1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-4,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-4,2), xlab="Predicted")
 axis(2, at=seq(-4,2,by=1),labels=seq(-4,2,by=1))
 dev.off() 
 
 ## Netherlands
 tiff("Netherlands.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[8],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[8],group=(temp$month_string),yaxt="n",
            ylim=c(1e-5,1e2),xlab="Observed")
 axis(2, at=c(1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-5,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-5,2), xlab="Predicted")
 axis(2, at=seq(-5,2,by=1),labels=seq(-5,2,by=1))
 dev.off()
 
 ## New Zealand
 tiff("Newzealand.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[9],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[9],group=(temp$month_string),yaxt="n",
            ylim=c(1e-10,1e2),xlab="Observed")
 axis(2, at=c(1e-10,1e-9,1e-8,1e-7,1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-10,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-10,2), xlab="Predicted")
 axis(2, at=seq(-10,2,by=1),labels=seq(-10,2,by=1))
 dev.off()
 
 
 ## United States
 tiff("Unitedstates.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[11],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[11],group=(temp$month_string),yaxt="n",
            ylim=c(1e-4,1e2),xlab="Observed")
 axis(2, at=c(1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2),labels=seq(-4,2,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-4,2), xlab="Predicted")
 axis(2, at=seq(-4,2,by=1),labels=seq(-4,2,by=1))
 dev.off()
 
 
 
 
 
 
 
 
 ##Does not work...
 
 ## Luxembourg
 tiff("Luxembourg.tiff",pointsize=10,width=15, height=15, units = "cm", res=200)
 temp<-read.csv(paste0("data_",countries2[7],".csv"))
 par(mfrow=c(2,1),mar=c(4, 2, 2, 2) + 0.1,oma=4,2,4,2)
 cenboxplot(obs=temp$OOCYSTS_CO,cen=temp$censored,
            main=countries[7],group=(temp$month_string),yaxt="n",
            ylim=c(1e-3,1e3),xlab="Observed")
 axis(2, at=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3),labels=seq(-3,3,by=1))
 boxplot(temp$predictedlog~temp$month_string,yaxt="n",ylim=c(-3,3), xlab="Predicted")
 axis(2, at=seq(-3,3,by=1),labels=seq(-3,3,by=1))
 dev.off()
 
 
 





 
########################## archive ####################################

  

 
 #normal linear model is wrong, because this does not take into account censored data
a<-with(data,lm(observedlog~predictedlog))
a

#example
# (examples in Chap 12 of the NADA book)
data(TCEReg)

# Using the formula interface
with(TCEReg, cenreg(Cen(TCEConc, TCECen)~PopDensity))

####### boxplots with ggplot

# create the frame - ONE VARIABLE ####
g <- ggplot(data = data, aes(x = log10(OOCYSTS_CO)))
g
# now, add layers, just to check it out
g + geom_density()
g + geom_histogram()

# more variables
# but this does not take into account the censored data, so is actually meaningless
g <- ggplot(data = data, aes(y = log10(OOCYSTS_CO), x = COUNTRY)) #z= for color
g + geom_boxplot()



#bij de gemodelleerde waardes heb je dat probleem niet
h <- ggplot(data = data, aes(y = log10(total_oocy), x = COUNTRY)) #z= for color
h + geom_boxplot()

# to be adjusted
g + geom_boxplot(aes(color=Age,fill=Age),alpha=0.5) +
  ylab("Concentration (10log oocysts/gram)") +
  xlab("") +
  scale_fill_manual(values=caPalette) +
  scale_colour_manual(values=caPalette) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=25,color="black"),
        axis.text.y = element_text(size=25,color="black"),
        axis.title.y = element_text(size=25,hjust=1,vjust=1),
        legend.position="none")
ggsave("concentrationggplot.png", units="cm", width=18, height=20, dpi=300)




######## try out with regression for censored data
a<-cenken(y=data$OOCYSTS_CO,ycen=data$censored,x=data$total_oocy)
b<-cenken(y=data$observedlog,ycen=data$censored,x=data$predictedlog) #doesnt work
plot(data$OOCYSTS_CO,data$total_oocy)
plot(data$observedlog,data$predictedlog)

#werkt ook niet
mletest = with(data, cenmle(OOCYSTS_CO,censored=censored,total_oocy))
mletest = with(data, cenmle(observedlog,censored=censored,predictedlog))

# Create a MLE regression object
data(TCEReg)
tcemle = with(TCEReg, cenmle(TCEConc, TCECen))
summary(tcemle)
median(tcemle)
mean(tcemle)
sd(tcemle)
quantile(tcemle)
# This time specifiy a different confidence interval
tcemle = with(TCEReg, cenmle(TCEConc, TCECen, conf.int=0.80))
# Use the model's confidence interval with the quantile function
quantile(tcemle, conf.int=TRUE)
# With groupings
with(TCEReg, cenmle(TCEConc, TCECen, PopDensity))

######### proberen: bagplot
library(aplpack)

with(data, bagplot(log10(RASTERVALU), log10(OOCYSTS_CO)))#,factor=3,col=colors,pch=16)) # x vs. y
abline(0,1)

########### nu doen: mooi uitziend plotje, met ggplot?

library(ggplot2)

#color palette   - nog aanpassen
caPalette <- c("#E69F00", "#56B4E9", "#009E73")

# nog aanpassen, kijken of dit met bounds kan bijvoorbeeld
# evt n achter country namen zetten in dataframe al?
# namen in dataframe van kolommen aanpassen

g <- ggplot(data = countrydata, aes(y = log10(mean), x = log10(mean_pred),z=countries))
g + geom_point()
g + geom_point(aes(color=countries,fill=countries),alpha=0.5) #+
#  ylab("Observed") +
#  xlab("Predicted") )#+
#  scale_fill_manual(values=caPalette) +
# scale_colour_manual(values=caPalette) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=25,color="black"),
#       axis.text.y = element_text(size=25,color="black"),
#      axis.title.y = element_text(size=25,hjust=1,vjust=1),
#     legend.position="none")

g <- ggplot(data = countrydata, aes(y = log10(median), x = log10(median_pred),z=countries))
g + geom_point()
g + geom_point(aes(color=countries,fill=countries),alpha=0.5)

#dit nog aanpassen
ggsave("scatterplot_means.png", units="cm", width=18, height=20, dpi=300)



#change some erroneous numbers from Arcgis to NA (ArcGIS NA value is -9999, 
# but they appear here divided by 1000 for the conversion to /L)
data$total_oocy[data$total_oocy==-9.999]<-NA

longitude <-data$Longitude[1:403] 
latitude <- data$Latitude[1:403]
lonlat <- cbind(longitude, latitude)
crdref <- CRS('+proj=longlat +datum=WGS84')
pts <- SpatialPoints(lonlat, proj4string=crdref)

brazil<-SpatialPointsDataFrame(pts)

#change ArcGIS indication for not available (-9999) to NA
data$RASTERVALU<-ifelse(data$RASTERVALU==-9999,NA,data$RASTERVALU)

#change to same unit as observations (oocysts/L instead of m3)
data$total_oocy<-data$RASTERVALU/1000 

#if detection limit says 0, change to NA
data$DETECT_LIM<-ifelse(data$DETECT_LIM==0,NA,data$DETECT_LIM)

data$RECOVERY<-ifelse(data$RECOVERY==0,NA,data$RECOVERY)
data$OOCYSTS_L<-ifelse(data$OOCYSTS_L==0,NA,data$OOCYSTS_L)

#where recovery was zero, things go wrong

data$OOCYSTS_CO<-data$OOCYSTS_L/data$RECOVERY
data$OOCYSTS_CO<-ifelse(is.na(data$OOCYSTS_CO),data$OOCYSTS_L,data$OOCYSTS_CO)
data$OOCYSTS_CO<-ifelse(is.na(data$OOCYSTS_CO),data$DETECT_LIM,data$OOCYSTS_CO)

data$observedlog[data$observedlog==-Inf]<-NA

