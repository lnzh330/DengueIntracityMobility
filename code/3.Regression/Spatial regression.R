####################################################################################
###############################4years###################################
################################################################################
################################################################################
df0 <- read.csv('logistic_data.csv')
library(sf)
library(spdep)
#library(spData)
#library(spatialEco)
library(rms)
source('logistic.regression.R')
# load shape file for research
map <- read_sf("00.data/Ruili.shp")
# dim(map)
map <- merge(map, df0, by='tid')
# Create adjacency matrix
nb.map <- poly2nb(as_Spatial(map$geometry))
# Create weight matrix
W_mat<-nb2listw(nb.map, style="W", zero.policy=TRUE)

######2019
#moran.test
moran.test(map$logis, listw=W_mat,zero.policy=T)

######logistic regression
df0 <- st_as_sf(df0, coords = c("x", "y"), crs = 4326,
                  agr = "constant")
####standardize
df0$perdis <- scale(df0$perdis)
df0$total <- scale(df0$total)
df0$intercity <- scale(df0$intercity)
df0$population <- scale(df0$population)



######SLM
###X1-intercity-crossborder：
lmodel1 <- logistic.regression(df0, y='logis', x=c('intercity'),
                               autologistic = T,coords=st_coordinates(df0), bw = 100)

lmodel1$model
lmodel1$diagTable
lmodel1$coefTable

#compute OR
coef<-lmodel1$coefTable$Coef    
coef_CI<-matrix(c(coef - 1.96*lmodel1$coefTable$StdError,coef + 1.96*lmodel1$coefTable$StdError),
                nrow = 3, ncol = 2)
OR_Results1<-exp(cbind("OR"=coef,"LL"=coef_CI[,1],"UL"=coef_CI[,2]))  
round(OR_Results1,3)   

###X2-total intracity：
lmodel2 <- logistic.regression(df0, y='logis', x=c('total'),
                               autologistic = T,coords=st_coordinates(df0), 
                               bw=100)

lmodel2$model
lmodel2$diagTable
lmodel2$coefTable

#compute OR
coef<-lmodel2$coefTable$Coef    
coef_CI<-matrix(c(coef - 1.96*lmodel2$coefTable$StdError,coef + 1.96*lmodel2$coefTable$StdError),
                nrow = 3, ncol = 2)
OR_Results2<-exp(cbind("OR"=coef,"LL"=coef_CI[,1],"UL"=coef_CI[,2]))  
round(OR_Results2,3)

###X3-population：
lmodel3 <- logistic.regression(df0, y='logis', x=c('population'),
                               autologistic = T,coords=st_coordinates(df0), 
                               bw=100)

lmodel3$model
lmodel3$diagTable
lmodel3$coefTable

#compute OR
coef<-lmodel3$coefTable$Coef    
coef_CI<-matrix(c(coef - 1.96*lmodel3$coefTable$StdError,coef + 1.96*lmodel3$coefTable$StdError),
                nrow = 3, ncol = 2)

OR_Results3<-exp(cbind("OR"=coef,"LL"=coef_CI[,1],"UL"=coef_CI[,2]))  
round(OR_Results3,3)

###multivariable
lmodel <- logistic.regression(df0, y='logis', x=c('intercity','total','population'),
                              autologistic = T,coords=st_coordinates(df0), 
                              bw=100)
lmodel$model
lmodel$diagTable
lmodel$coefTable

#compute OR
coef<-lmodel$coefTable$Coef 
coef_CI<-matrix(c(coef - 1.96*lmodel$coefTable$StdError,coef + 1.96*lmodel$coefTable$StdError),
                nrow = 5, ncol = 2)
OR_Results<-exp(cbind("OR"=coef,"LL"=coef_CI[,1],"UL"=coef_CI[,2]))  
round(OR_Results,3) 
