
setwd("~/Documents/coding_projects/IS_organized/simulations_and_model_code")
source(file="MS_SA_funs.R") 

# put this simExportPlots function here so that exportPlots.RData do not overlap 

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
    
    
    NLCommand("export-all-plots \"exportPlots_mtg.csv\"") 
    output[[i]]<-getGraphs("exportPlots_mtg.csv") 
    
  }
  
  return(output)
}


############################### ###############################
tic()

#change max-tree-growth 
paramVals <- (1:6)*5
dataMTG<- runParams(paramVals, base.param, "myMod_revised_changeMTG.RData", "max-tree-growth")

toc()

