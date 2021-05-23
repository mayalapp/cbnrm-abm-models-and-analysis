library(RNetLogo)
library(tictoc)


testing <- "testing"
setwd("/Applications/NetLogo 6.0.4/models/ISmodels/Vallino_NL6/code")
setwd("/Applications/NetLogo 6.0.4/models/ISmodels/Vallino_NL6/code")

ticks<-5 # for testing 
runs<-2
#ticks<- 2000
#runs<- 50

setParam <- function(paramName, paramValue){
	NLCommand(paste("set", paramName, paramValue, sep=" "))	
}

getGraphs <- function(fileName){
	setwd("/Applications/NetLogo 6.0.4/models/ISmodels/Vallino_NL6/code")
plotFile<-read.csv(file = fileName, header=FALSE, stringsAsFactors=FALSE)[,1:2]

i<-1
j<-1
out<-data.frame(1:(ticks/10))
while(i<dim(plotFile)[1]){
	if(plotFile[i,2]=="y"){
		
		out[,plotFile[i-8,1]]<-plotFile[(i+1):(i+(ticks/10)),2]
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
	model.path <-"/Applications/NetLogo\ 6.0.4/models/ISmodels/Vallino_NL6/code/Enforcement\ Endogenous\ Inst\ CORRECTED.nlogo"
	NLLoadModel(model.path)
	#ticks <- 2000
	#runs <- 50
	
	
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



base.param <- c(5,20,100, 0.5, 50)


 ###############################

runParams<- function(paramValues, base.param, fileName, variable){
	dataOut <- list()
	for(i in 1:length(paramValues)){
		paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold", "enforcement-level")
		varNum <- match(variable, paramNames)
		newParams<-base.param
		newParams[varNum] <- paramValues[i]
		dataOut[[i]]<-simExportPlots(newParams)
		
	save(dataOut, file=fileName)
	}
	
	return(dataOut)
}

tic()

##
# tryign to make sims run faster.... 
#runParams<- function(paramValues, base.param, fileName, variable){
#	dataOut <- list()
#
#	newParams <- list()
#	for(i in 1:length(paramValues)){
#		paramNames <- c("cost", "max-tree-growth", "initial-loggers", "reference-threshold", "enforcement-level")
#		varNum <- match(variable, paramNames)
#		newParams[[i]] <- base.param
#		newParams[[i]][varNum] <- paramValues[i]
#		
#	}
#	
#	dataOut <-lapply(newParams, simExportPlots)
#	save(dataOut, file=fileName)
#	return(dataOut)
#}
#
 ############################### ###############################
# base params 

#
#sim1 = list((0:10)*10, base.param, "test1.RData", "enforcement-level")
#sim2 = list((0:10)*2, base.param, "test2.RData", "cost")
#test = list(sim1, sim2)
#paramVals = list((0:10)*10, (0:10)*2), base.param = rep(base.param,2), fileName = list("test1.RData", "test2.RData"), variable =  list("enforcement-level", "cost"))
#dataGrouped <- lapply(test, runParams)
#dataBase<- runParams(paramVals, base.param, "vallinoCorrected_base.RData", "enforcement-level")
#
#toc()

tic()
 ############################### ###############################
#change enforcement 
paramVals <- (0:10)*10
dataMonitoring<- runParams(paramVals, base.param, "vallinoCorrected_changeMonitoring.RData", "enforcement-level")

###############################
#change cost 
paramVals <- (0:10)*2
dataCost<- runParams(paramVals, base.param, "vallinoCorrected_changeCost.RData", "cost")

###############################
#change max-tree-growth 
paramVals <- (1:6)*5
dataMTG<- runParams(paramVals, base.param, "vallinoCorrected_changeMTG.RData", "max-tree-growth")

####################################
# reference threshold 
paramVals <- (1:10)/10
dataRT<- runParams(paramVals, base.param, "vallinoCorrected_changeRT.RData", "reference-threshold")

###############################
#change loggers
paramVals <- seq(50, 300, by=50)
dataLog<- runParams(paramVals, base.param, "vallinoCorrected_changeLog.RData", "initial-loggers")

toc()