library(deSolve)
library(dplyr)
library(ggplot2)
library(minpack.lm)
library(tidyr)
library(reshape2)

#setwd("~/Documents/WoosterStuff/fall2019/IS_organized/analysis/allData_IS")
load("./IS_analysis/additional_data/logger_and_bm_data.RData")
load("./IS_analysis/additional_data/changeEnf_log1000.RData")


getGraphs <- function(fileName){
  
  plotFile<-read.csv(file = fileName, header=FALSE, stringsAsFactors=FALSE)[,1:2]
  
  i<-1
  j<-1
  out<-data.frame(1:2000)
  while(i<dim(plotFile)[1]){
    if(plotFile[i,2]=="y"){
      out[,plotFile[i-8,1]]<-plotFile[(i+1):(i+200),2]
      j<- j+1
      i<- i+200
      print(i)
    }
    i<-i+1
  }
  
  colNames <- c("K", "unsatisfied", "Payoffs", "TotalBM", "GreenPatches", "beta.i", "k.i", "numLoggers") 
  out<-out[,-1]
  colnames(out)<- colNames
  
  for(i in 1:length(colNames)){
    out[,i] <-as.numeric(out[,i])
  }
  
  return(out)
  
}



#varName is the variable (graph) we're interested in, e.g. total biomass or # of loggers 
# paramSet is the names for the columns for different parameter options run (e.g. if we want the total BM for simulations where we've run 100 loggers, 200 loggers, 300 loggers.... param set may be c("bm100.", "bm200."....)
# ticks is number of ticks run in simulation 
# runs is number of times the simulation was run 
#dataIn is list with all of the lists from the simulation, e.g. data1<-list(log200, log300)
makeDF<- function(dataIn, varName, paramSet, ticks, runs){
  output<-data.frame(1:(ticks/10))
  for(j in 1: length(paramSet)){
    for(i in 1:runs){
      output[paste(paramSet[j], i, sep='')] <- dataIn[[j]][[i]][varName]
    }
  }
  output<-output[,-1]
  
  return(output)
}

ticksAsCols<- function(dataIn){
  output<-t(dataIn)
  output<-as.data.frame(output)
  output["temp"]<-rownames(output)
  output<-output%>%separate(temp, into=c("param", "paramVal", "run"))
  
  output$param <- as.factor(output$param)
  output$paramVal <- as.factor(output$paramVal)
  
  return(output)
}



data1<-list(enf1, enf8)
varNames<- c( "bm.1.", "bm.8.")

logEnfBM<-makeDF(data1, "TotalBM",varNames , 7000, 1)
logEnfBM2<-ticksAsCols(logEnfBM)

varNames<- c( "log.1.", "log.8.")
logEnfLog<-makeDF(data1, "numLoggers",varNames , 7000, 1)
logEnfLog2<-ticksAsCols(logEnfLog)

load("./IS_analysis/additional_data/noMaxTree.RData")
data1<-list(noMaxTree)
varNames<- c( "bm")

noMTbm<-makeDF(data1, "TotalBM",varNames , 7000, 1)
noMTbm2<-ticksAsCols(noMTbm)

varNames<- c( "log")
noMTlog<-makeDF(data1, "numLoggers",varNames , 7000, 1)
noMTlog2<-ticksAsCols(noMTlog)
noMT<- data.frame(bm=noMTbm, log=noMTlog)


############### 
# fit model to LV eqs

#load data 
data1<-getGraphs("./IS_analysis/additional_data/fitToLV.csv") 
lvTest1<-data.frame(data1["TotalBM"], data1["numLoggers"])

#set LV equation 
scale1=5.5
alpha <- 1/scale1
ymax <- 1000
ymin <- ymax * 0.05
xmax <- 40000
beta <- 0.0032/scale1
gamma <- 0.12/scale1
delta <- 0 

params <- c(alpha, beta, delta, gamma, ymin, xmax) 

state <- c(X = xmax, Y=ymax)

lotkaVolterra <- function (t, state, parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    dX <- ( alpha * X - beta * X*Y) 
    dY <- delta * X * Y - gamma *Y
    
    #return rate of change 
    list(c(dX, dY))
  })
}

times <- seq(0,465, by = 0.1)

out <- ode(y = state, times = times, func = lotkaVolterra, parms = params) 
fit_model_to_LV<- as.data.frame(out)



############################
# data with 1000 initial loggers and LV equations 
scale1=16
alpha <- 1/scale1
ymax <- 1000
ymin <- ymax * 0.05
xmax <- 40000
beta <- 0.005/scale1
gamma <- 0.2/scale1
delta <- 0 

params <- c(alpha, beta, delta, gamma, ymin, xmax) 

state <- c(X = xmax, Y=ymax)

lotkaVolterra <- function (t, state, parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    dX <- ( alpha * X - beta * X*Y) 
    dY <- delta * X * Y - gamma *Y
    
    #return rate of change 
    list(c(dX, dY))
  })
}

times <- seq(0,465, by = 0.1)

out <- ode(y = state, times = times, func = lotkaVolterra, parms = params) 
compare_model_to_LV<- as.data.frame(out)


