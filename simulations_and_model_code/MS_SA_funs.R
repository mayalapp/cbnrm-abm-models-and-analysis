library(RNetLogo)
library(tictoc)


testing <- "testing"
setwd("/Applications/NetLogo 6.0.4/models/ISmodels/Vallino_NL6/code")

#ticks<-5 # for testing
#runs<-2
ticks<- 2000
runs<- 50
diff <-6 # this number edits how far to go back on the NetLogo excel output to find the label for the plot that is being exported. If an error occurs (e.g. colNames not correct length, check/change this number appropriately)


setParam <- function(paramName, paramValue){
  NLCommand(paste("set", paramName, paramValue, sep=" "))	
}

getGraphs <- function(fileName){
  
  plotFile<-read.csv(file = fileName, header=FALSE, stringsAsFactors=FALSE)[,1:2]
  
  i<-1
  j<-1
  out<-data.frame(1:(ticks/10))
  while(i<dim(plotFile)[1]){
    if(plotFile[i,2]=="y"){
      
      out[,plotFile[i-diff,1]]<-plotFile[(i+1):(i+(ticks/10)),2]
      j<- j+1
      i<- i+(ticks/10)
    }
    i<-i+1
  }
  
  
  
  colNames <- c("K", "unsatisfied", "Payoffs", "TotalBM", "GreenPatches", "beta.i", "k.i", "numLoggers", "numCheaters") 
  out<-out[,-1]
  
  colnames(out)<- colNames
  
  
  for(i in 1:length(colNames)){
    out[,i] <-as.numeric(out[,i])
  }
  
  return(out)
  
}



# 1. Define a function that runs the simulation model
# for a given parameter combination and returns the
# value(s) of the fitting criteria. 
simExportPlots <- function(params) {
  # params is vector with: cost, max-tree-growth, initial-loggers
  #    reference-threshold, enfocement-level
  #  in that order! 
  # for now high-tolerance = "True" 
  
  # open NetLogo
  nl.path <- "/Applications/NetLogo\ 6.0.4/Java"
  NLStart(nl.path, gui=FALSE, nl.jarname =  "netlogo-6.0.4.jar")
  # open Endogenous Enforcement model
  model.path <-"/Applications/NetLogo\ 6.0.4/models/ISmodels/Vallino_NL6/code/M&Smodel_revised.nlogo"
  NLLoadModel(model.path)
  #ticks <- 2000
  #runs <- 50
  
  
  #set paramters
  paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold", "monitoring-level", "initial-prob-cheat", "sanction-level")
  for(i in 1:length(params)){
    setParam(paramNames[i], params[i]) 
  }
  setParam("high-tolerance", "True") # for now always high tolerance 
  
  output <-list()
  
  for(i in 1:runs){
    # run simulation 
    NLCommand("setup")
    NLDoCommand(ticks, "go")
    
    
    NLCommand("export-all-plots \"exportPlots.csv\"") 
    output[[i]]<-getGraphs("exportPlots.csv") 
    
  }
  
  return(output)
}



base.param <- c(5,20,100, 0.5, 50, 0.5, 0.5)


###############################

runParams<- function(paramValues, base.param, fileName, variable){
  dataOut <- list()
  for(i in 1:length(paramValues)){
    paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold", "monitoring-level", "initial-prob-cheat", "sanction-level")
    varNum <- match(variable, paramNames)
    newParams<-base.param
    newParams[varNum] <- paramValues[i]
    dataOut[[i]]<-simExportPlots(newParams)
    
    save(dataOut, file=fileName)
    
    print(paramValues[i])
    
  }
  
  return(dataOut)
}

###################