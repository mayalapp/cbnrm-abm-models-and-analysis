setParam <- function(paramName, paramValue){
  NLCommand(paste("set", paramName, paramValue, sep=" "))	
}


#run enforcement model - 50 times
# 1. Define a function that runs the simulation model
# for a given parameter combination and returns the
# value(s) of the fitting criteria. 
sim <- function(params) {
  # params is vector with: cost, max-tree-growth, initial-loggers
  #    reference-threshold
  #  in that order! 
  # for now high-tolerance = "True" 
  
  # open NetLogo
  nl.path <- "/Applications/NetLogo\ 6.0.4/Java"
  NLStart(nl.path, gui=FALSE, nl.jarname =  "netlogo-6.0.4.jar")
  # open Endogenous Enforcement model
  model.path <-"/Applications/NetLogo\ 6.0.4/models/ISmodels/Vallino_NL6/code/Enforcement\ Endogenous\ Inst.nlogo"
  NLLoadModel(model.path)
  ticks <- 2000
  runs <- 50
  
  
  #set paramters
  paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold")
  for(i in 1:length(params)){
    setParam(paramNames[i], params[i]) 
  }
  setParam("high-tolerance", "True") # for now always high tolerance 
  
  output <- data.frame()
  for(i in 1:runs){
    # run simulation 
    NLCommand("setup")
    output[i,"iniBM"]<- NLReport("sum [trees] of patches")
    NLDoCommand(ticks, "go")
    output[i,"finalBM"] <- NLReport("sum [trees] of patches")
  }
  return(output)
}



base.param <- c(5,20,100, 0.5)




#export graphs from a file 
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


# simulation that exports all data (in a list) from all of the graphs 
simExportPlots <- function(params) {
  # params is vector with: cost, max-tree-growth, initial-loggers
  #    reference-threshold, enfocement-level
  #  in that order! 
  # for now high-tolerance = "True" 
  
  # open NetLogo
  nl.path <- "/Applications/NetLogo\ 6.0.4/Java"
  NLStart(nl.path, gui=FALSE, nl.jarname =  "netlogo-6.0.4.jar")
  # open Endogenous Enforcement model
  model.path <-"/Applications/NetLogo\ 6.0.4/models/ISmodels/Vallino_NL6/code/Enforcement\ Endogenous\ Inst.nlogo"
  NLLoadModel(model.path)
  ticks <- 2000
  runs <- 50
  
  
  #set paramters
  paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold", "enforcement-level")
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


