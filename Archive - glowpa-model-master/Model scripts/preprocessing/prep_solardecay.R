# this script was made to find the sunlight survival function parameters
# based on the King et al. survival data 
# with Wageningen R Github help 


########## FOR LOGTRANSFORMED SURVIVAL WITH INSOLATION DATA ######
#I used optim (stats package) which is nice for general purpose optimization.


#// this returns predicted Fs, basically the model you want to optimise
predict_Fs_LN <- function(kl, kd, I, CDOC, Z) {
  -((kl*I/(kd*CDOC*Z))*(1-exp(-kd*CDOC*Z)))
}


#// function that calculates error (in this case RMSE) for given set of parameters (kl,kd)
#// par = c(kl,kd) -> optim requires all parameters in one vector
error_function <- function(par, I, CDOC, Z) {
  #  // predict with function
  pred <- predict_Fs_LN(par[1], par[2], I, CDOC, Z)
  #  // calc RMSE
  sqrt(mean((Fs_LN - pred) ^ 2))
}

data<-read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/UV/KingData_NAremoved.csv")

I<-data$I
CDOC<-data$CDOC
Z<-data$Z
Fs_LN<-data$LNFs


library(rgl)
plot3d(CDOC,I,Fs_LN,col="red", size=10)


#// par gives initial estimates for the parameters , 
#here you should have an idea in which region the parameters are
#the default method is minimization following Nelder and Mead 1965
opt <- optim(par = c(0.0005,10),
             fn = error_function,
             I = I, CDOC = CDOC, Z = Z)
opt

#optim returns a list that contains the results. opt$par is the best estimate
#I ended up with kl=0.0004798414 and kd= 9.8312160355

# the value of the minimized error is 1.251218 (opt$value) --> still quite large

kl<-0.0004798414
kd<-9.8312160355

data$prediction<-NA
data$prediction<-predict_Fs_LN(kl, kd, I, CDOC, Z)

plot(data$prediction,data$LNFs,ylim=c(-8,0),xlim=c(-8,0))
abline(0,1,lty=2)
abline(-0.6646941,0.8445185) #the obtained coefficients

#  Linear Regression
fit <- lm(data$LNFs ~ data$prediction)
summary(fit) # show results
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

data$differencesquared<-(data$prediction-data$LNFs)^2
differencesum<-sum(data$differencesquared)
RMSD<-1/(87-1)*differencesum #root mean squared deviation
#this is apparantly better than RMSE in this case
#RMSD is in the same unit as the data, so ln Fs
# see Piñeiro et al. 2008


# load library ####
library(ggplot2)

data$observed<-data$LNFs
data$predicted<-data$prediction
# use ggplot2 package to plot by water type
g <- ggplot(data = data, aes(y = observed, x=predicted)) +
  geom_point(aes(color = Watertype), alpha = 0.5, cex = 3) +
  geom_abline(mapping = NULL, data = NULL, slope=1, intercept=0,linetype="dashed") +
  geom_abline(mapping = NULL, data = NULL, slope=0.8445185, intercept=-0.6646941) +
  xlim(-8,0) +
  ylim(-8,0) +
  ylab("Observed survival ln(Fs)") +
  xlab("Predicted survival ln(Fs)")
g

setwd("D:/LuciePhD/Papers/Paper 4/Figures/Kingetalplot")
ggsave("watertype_observedpredicted.tiff", width=7, height=4, dpi=300)



bp + theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Weight (Kg)")                       # Set y-axis label

# Also possible to set the axis label with the scale
# Note that vertical space is still reserved for x's label
bp + scale_x_discrete(name="") +
  scale_y_continuous(name="Weight (Kg)")




# and also try by UV index
g <- ggplot(data = data, aes(y = LNFs, x=prediction)) +
  geom_point(aes(color = UVindex), alpha = 0.5, cex = 3) +
  geom_abline(mapping = NULL, data = NULL, slope=1, intercept=0) +
  xlim(-8,0) +
  ylim(-8,0)
g

# and also try by DOC concentration
g <- ggplot(data = data, aes(y = LNFs, x=prediction)) +
  geom_point(aes(color = CDOC), alpha = 0.5, cex = 3) +
  geom_abline(mapping = NULL, data = NULL, slope=1, intercept=0) +
  xlim(-8,0) +
  ylim(-8,0)
g
