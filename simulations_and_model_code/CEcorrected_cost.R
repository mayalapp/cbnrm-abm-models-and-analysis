
setwd("~/Documents/coding_projects/IS_organized/simulations_and_model_code")
source(file="CE_SA_funs.R") 

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
  model.path <-"/Applications/NetLogo\ 6.0.4/models/ISmodels/Vallino_NL6/code/Enforcement\ Endogenous\ Inst\ CORRECTED.nlogo"
  NLLoadModel(model.path)
  
  
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
    
    NLCommand("export-all-plots \"exportPlots_cost.csv\"") 
    output[[i]]<-getGraphs("exportPlots_cost.csv") 
    
  }
  
  return(output)
}


tic()
 ############################### ###############################
#change enforcement 
paramVals <- (0:10)*2
dataCost <- runParams(paramVals, base.param, "vallinoCorrected_changeCost.RData", "cost")

toc()
