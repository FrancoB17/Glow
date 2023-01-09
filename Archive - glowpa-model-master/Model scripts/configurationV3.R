## GENERAL CONFIGURATION SCRIPT
## Run this script before running any other script
## Last modified on: 24-10-2016

#####################Configuration#######################################################
rm(list=ls(all=TRUE))

#Set working directory, load libraries, set working path in and out

setwd("D:/LuciePhD/Model/Cryptomodel/temporary")
library(sp)
library(rgeos)
library(maptools)
library(rworldmap)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rgdal)
working.path.in<-"D:/LuciePhD/Model/Cryptomodel/modeldata/"
working.path.out<-"D:/LuciePhD/Model/Cryptomodel/temporary/"


#define color palettes to be used in map plots. Usage: plot(..., col=colors)
colors <-colorRampPalette(c("green", "yellow", "orange","red"))(100)
colors2<-colorRampPalette(c("blue","blue", "blue", "dodgerblue", "dodgerblue", "dodgerblue", "green", "green", "yellow", "orange","red", "red4"))(100)
colors3<-colorRampPalette(c("blue","blue", "dodgerblue", "dodgerblue", "green", "green", "yellow", "orange","red", "red4"))(100)
colors4<-colorRampPalette(c("blue","dodgerblue", "green", "yellow", "orange","red", "red4"))(100)
colors5<-colorRampPalette(c("blue","blue", "blue","blue","blue", "blue","blue","blue", "blue","blue","blue","blue", "blue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue", "dodgerblue","green", "green", "yellow", "orange","red", "red4"))(100)

## Read in overall parameters file for L1 model (the livestock model)
par<- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/cryptoL1_parameters.csv" )

## Read in overall parameters file for concentrations model
parc<- read.csv("D:/LuciePhD/Model/Cryptomodel/modeldata/crypto_conc_parameters.csv" )


#useful functions to check the datasets: summary() str() class()

