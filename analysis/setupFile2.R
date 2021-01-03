###############################
#setup

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(stats)


## functions code

#varName is the variable (graph) we're interested in, e.g. total biomass or # of loggers 
# paramSet is the names for the columns for different parameter options run (e.g. if we want the total BM for simulations where we've run 100 loggers, 200 loggers, 300 loggers.... param set may be c("bm100.", "bm200."....)
# ticks is number of ticks run in simulation 
# runs is number of times the simulation was run 
#dataIn is list with all of the lists from the simulation, e.g. data1<-list(log200, log300)
makeDF<- function(dataIn, varName, paramSet, ticks, runs){
  output<-data.frame(1:(as.integer(ticks/10)))
  for(j in 1: length(paramSet)){
    for(i in 1:runs){
      output[paste(paramSet[j], i, sep='.')] <- dataIn[[j]][[i]][varName]
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

#converts data into dataframe with final statistic for each run 
#dataIn: is list generated from RNetlogo simulation run 
# variableValues: the different values tested for the parameter of interest
# variableOfInterest: the output variable we are interested in (e.g. TotalBM, numCheaters, etc.)
# ticks: number of ticks Netlogo sim was run for (usually 2000)
# runs: number of runs Netlogo sim was run for (usually 50)
# statType: either "final" or "perc" (percentage of the orginal number) 

finalStatsDataframe <- function(dataIn, variableValues, variableOfInterest, ticks, runs, statType){
  
  varNames<- paste(variableOfInterest, variableValues, sep=".")
  out<-makeDF(dataIn, variableOfInterest,varNames , ticks, runs)
  
  out1<-data.frame(1:runs)
  for(i in 1:length(variableValues)){
    x<-t(out[1,(runs*(i-1)+1):(runs*i)]); colnames(x)<-paste("ini", varNames[i], sep=".")
    out1[,paste("ini", varNames[i], sep=".")] <- x
    
    x<-t(out[as.integer(ticks/10),(runs*(i-1)+1):(runs*i)]); colnames(x)<-paste("final", varNames[i], sep=".")
    out1[,paste("final", varNames[i], sep=".")] <- x
  }
  out1<-out1[,-1]
  
  if(statType=="final"){
    out2 <- out1%>%gather(key="variableValue", value=variableOfInterest)%>%filter(grepl("final", variableValue))
  }else{
    if(statType=="perc"){
      for(i in 1:length(variableValues)){
        x<-out1[, paste("final", varNames[i], sep=".")]/out1[, paste("ini", varNames[i], sep=".")];   colnames(x)<-paste("perc", varNames[i], sep=".")
        out1[, paste("perc", varNames[i], sep=".")] <- x
      }
      out2 <- out1%>%gather(key="variableValue", value=variableOfInterest)%>%filter(grepl("perc", variableValue))
    }
  }
  
  out2<-out2%>%separate(col = variableValue, into = c("type", "variable", "variableValue"), sep = "\\.")%>%mutate(variableValue=as.factor(as.numeric(variableValue)))
  
  out2[, variableOfInterest]<-out2[,"variableOfInterest"]
  
  return(out2)
}


finalStats_allOutputs <- function(dataIn, paramVals, ticks, runs){
  # final, %loggers, average last 10?, % orig, %orig, final, final, average last 10?, average last 10?, %orig, %loggers 
  modelOutputs<- c("K", "unsatisfied", "Payoffs", "TotalBM", "GreenPatches", "beta.i", "k.i", "numLoggers", "numCheaters")
  percOutput <- c("final", "final", "final", "perc", "perc", "final", "final", "perc", "final")
  
  dataOut<-finalStatsDataframe(dataIn, paramVals, modelOutputs[1], ticks, runs, percOutput[1])
  for(i in 2:length(modelOutputs)){
    temp<- finalStatsDataframe(dataIn, paramVals, modelOutputs[i], ticks, runs, percOutput[i])
    dataOut[,modelOutputs[i]]<-temp[5]
  }
  
  del <-c(1,2,4)
  dataOut<- dataOut[,-del]
  
  return(dataOut)
}

finalStats_allOutputs_extra <- function(dataIn, paramVals, ticks, runs){
  # final, %loggers, average last 10?, % orig, %orig, final, final, average last 10?, average last 10?, %orig, %loggers 
  modelOutputs<- c("K", "unsatisfied", "Payoffs", "TotalBM", "GreenPatches", "beta.i", "k.i", "numLoggers", "numCheaters", "highMinCut", "lowMinCut", "avgRTremoved")
  percOutput <- c("final", "final", "final", "perc", "perc", "final", "final", "perc", "final", "final", "final", "final")
  
  dataOut<-finalStatsDataframe(dataIn, paramVals, modelOutputs[1], ticks, runs, percOutput[1])
  for(i in 2:length(modelOutputs)){
    temp<- finalStatsDataframe(dataIn, paramVals, modelOutputs[i], ticks, runs, percOutput[i])
    dataOut[,modelOutputs[i]]<-temp[5]
  }
  
  del <-c(1,2,4)
  dataOut<- dataOut[,-del]
  
  return(dataOut)
}


#find jumps in the institution 

findKJumps<-function(dataIn, variableValues, ticks, runs){ 
  varNames<- paste("K", variableValues, sep=".")
  
  dataFormatted<- makeDF(dataIn, "K",varNames , ticks, runs) 
  print(dim(dataFormatted))
  #x2<-dataFormatted
  mostRecent_k<-rep(0, times=runs)
  mostRecent<-data.frame(1:runs)
  
  for(k in 1:length(variableValues)){
    for(j in (runs*(k-1)+1):(runs*k)){
      jump<-rep(0, times=as.integer(ticks/10))
      for(i in 1:((as.integer(ticks/10))-1)){
        if(dataFormatted[i+1,j]!=dataFormatted[i,j]){
          if(dataFormatted[i+1,j]<dataFormatted[i,j]){
            jump[i+1]<- -1
            mostRecent_k[j%%runs]<- mostRecent_k[j%%runs]+1
            
          }
          else{
            jump[i+1]<- 1
            mostRecent_k[j%%runs]<- mostRecent_k[j%%runs]+1
          }
        }
        
      }
      #x2[,j]<- jump
    }
    mostRecent[,varNames[k]]<- mostRecent_k
    mostRecent_k<-rep(0, times=runs)
    
    
  }
  mostRecent<-mostRecent[,-1]
  
  return(mostRecent)
}

findFinalKJumps<-function(dataIn, variableValues, ticks, runs){ 
  varNames<- paste("K", variableValues, sep=".")
  
  dataFormatted<- makeDF(dataIn, "K",varNames , ticks, runs) 
  
  #x2<-dataFormatted
  mostRecent_k<-rep(0, times=runs)
  mostRecent<-data.frame(1:runs)
  
  for(k in 1:length(variableValues)){
    for(j in (runs*(k-1)+1):(runs*k)){
      jump<-rep(0, times=as.integer(ticks/10))
      for(i in 1:((as.integer(ticks/10))-1)){
        if(dataFormatted[i+1,j]!=dataFormatted[i,j]){
          if(dataFormatted[i+1,j]<dataFormatted[i,j]){
            jump[i+1]<- -1
            mostRecent_k[j%%runs]<- i
            
          }
          else{
            jump[i+1]<- 1
            mostRecent_k[j%%runs]<- i
          }
        }
        
      }
      #x2[,j]<- jump
    }
    mostRecent[,varNames[k]]<- mostRecent_k
    mostRecent_k<-rep(0, times=runs)
    
    
  }
  mostRecent<-mostRecent[,-1]
  
  return(mostRecent)
}

# add mean of all runs to the dataframe (for graphing)
addMeans<- function(dataIn){
  meansK<- dataIn%>%group_by(variableValue)%>%summarize(meanK=mean(K))
  dataIn<-dataIn%>%mutate(meanK=0)
  
  x<-rep(0, times=500)
  for( i in 1:10){
    dataIn[((i-1)*50+1):((i-1)*50+50),"meanK"]<-as.numeric(meansK[i,2])
  }
  
  meansCheat<- dataIn%>%group_by(variableValue)%>%summarize(meanCheat=mean(numCheaters))
  dataIn<-dataIn%>%mutate(meanCheat=0)
  for( i in 1:10){
    dataIn[((i-1)*50+1):((i-1)*50+50),"meanCheat"]<-as.numeric(meansCheat[i,2])
  }
  
  
  meansBM<- dataIn%>%group_by(variableValue)%>%summarize(meanBM=mean(TotalBM))
  dataIn<-dataIn%>%mutate(meanBM=0)
  for( i in 1:10){
    dataIn[((i-1)*50+1):((i-1)*50+50),"meanBM"]<-as.numeric(meansBM[i,2])
  }
  
  
  return(dataIn)
}

# find maximum institution 
maxK<-function(dataIn, variableValues, ticks, runs){
  varNames<- paste("K", variableValues, sep=".")
  dataFormatted<- makeDF(dataIn, "K",varNames , ticks, runs) 
  maxK<-sapply(dataFormatted, max, na.rm = TRUE)
  
  return(maxK)
}

# find trough of a distribution between two points (a and b)
findTrough<-function(distribution, a, b){
  DensityFinalJumpY <- density(distribution)$y
  DensityFinalJumpX <- density(distribution)$x
  
  MinYDensity<- min(DensityFinalJumpY[DensityFinalJumpX > a & DensityFinalJumpX < b], na.rm=TRUE)
  trough = DensityFinalJumpX[which(DensityFinalJumpY == MinYDensity)]
  
  return(trough)
}


# find what tick a change in K occurs) 
findKJumpTimes<-function(dataIn, variableValues, ticks, runs){ 
  varNames<- paste("K", variableValues, sep=".")
  
  dataFormatted<- makeDF(dataIn, "K",varNames , ticks, runs) 
  
  x2<-dataFormatted
  
  for(k in 1:length(variableValues)){
    for(j in (runs*(k-1)+1):(runs*k)){
      for(i in 2:((as.integer(ticks/10))-1)){
        if(dataFormatted[i+1,j]==dataFormatted[i,j]){
          x2[i+1,j]= 0
        }
        else if(dataFormatted[i+1,j]<dataFormatted[i,j]){
          x2[i+1,j]= -1
          
        }
        else{
          x2[i+1,j]= 1
        }
      }
      
    }
  }
  
  
  
  return(x2)
}


#find what tick a jump or decline in BM occurs 
findBMJumpTimes<-function(dataIn, variableValues, ticks, runs){ 
  varNames<- paste("TotalBM", variableValues, sep=".")
  
  dataFormatted<- makeDF(dataIn, "TotalBM",varNames , ticks, runs) 
  
  x2<-dataFormatted
  
  for(k in 1:length(variableValues)){
    for(j in (runs*(k-1)+1):(runs*k)){
      for(i in 6:((as.integer(ticks/10))-6)){
        test = mean(dataFormatted[(i+1):(i+3),j])-mean(dataFormatted[(i-3):i,j])
        if(abs(test)<1000){
          x2[i+1,j]= 0
        }
        else if(test<0){
          x2[i+1,j]= -1
          
        }
        else{
          x2[i+1,j]= 1
        }
      }
      
    }
  }
  
  
  x2[1:6,]=-1
  x2[(as.integer(ticks/10)-5):(as.integer(ticks/10)),]=0
  
  return(x2)
}

### processing data

# setwd("~/Documents/WoosterStuff/fall2019/IS_organized/analysis/allData_IS/M&S_model_data")

# have two sets of data (ran everything again to make sure I ran it correctly because at one point I thought I had messed everything up and ran the wrong model)
# can choose either first set of data or second - doesn't matter which because they're pretty much the same 

if(TRUE){
  
  load("./allData_IS/M&S_model_data/myMod_changeCost_setUnsat.RData")
  dataCost<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeMTG_setUnsat.RData")
  dataMTG<-dataOut
  #dataMTG[[1]]<-NULL
  load("./allData_IS/M&S_model_data/myMod_changeLog_setUnsat.RData")
  dataLog<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeRT_setUnsat.RData")
  dataRT<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeCheat_setUnsat.RData")
  dataCheat<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeMonitoring_setUnsat.RData")
  dataMonitoring<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeSanction_setUnsat.RData")
  dataSanction<-dataOut
  
}

# change to true if you want to use the second set of data
if(FALSE){
  load("./allData_IS/M&S_model_data/myMod_changeCost_setUnsat2.RData")
  dataCost<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeMTG_setUnsat2.RData")
  dataMTG<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeLog_setUnsat2.RData")
  dataLog<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeRT_setUnsat2.RData")
  dataRT<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeCheat_setUnsat2.RData")
  dataCheat<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeMonitoring_setUnsat2.RData")
  dataMonitoring<-dataOut
  load("./allData_IS/M&S_model_data/myMod_changeSanction_setUnsat2.RData")
  dataSanction<-dataOut
}


# convert raw data to end-of-simulation summary statistics 
costData<-finalStats_allOutputs(dataCost, seq(0,20, by=2),  2000, 50)
mtgData<-finalStats_allOutputs(dataMTG, seq(5,30, by=5), 2000, 50)
logData<-finalStats_allOutputs(dataLog, seq(60,200, by=20),  2000, 50)
rtData<-finalStats_allOutputs(dataRT, seq(1,10, by=1),  2000, 50)
monitoringData<-finalStats_allOutputs(dataMonitoring, seq(0,100, by=10), 2000, 50)
cheatData<-finalStats_allOutputs(dataCheat, seq(0,10, by=1), 2000, 50)
sanctionData<-finalStats_allOutputs(dataSanction, seq(0,10, by=1), 2000, 50)

# get all base-data runs 
dataBase<-list()
dataBase[[1]]<- dataMTG[[4]]
dataBase[[2]]<-dataRT[[5]]
dataBase[[3]]<-dataMonitoring[[6]]
dataBase[[4]]<- dataCheat[[6]]
dataBase[[5]]<- dataSanction[[6]]
dataBase[[6]]<- dataLog[[3]]
baseData<-finalStats_allOutputs(dataBase, seq(1,6, by=1), 2000, 50)

# combine all raw data into 1 list 
dataAll = c(dataMTG, dataRT, dataMonitoring, dataCheat, dataSanction, dataLog, dataCost)

# add parameter name to each dataframe 
mtgData<-mtgData%>%mutate(param="mtg")#%>%addMeans()
costData<-costData%>%mutate(param="cost")#%>%addMeans()
rtData<-rtData%>%mutate(param="rt")#%>%addMeans()
logData<-logData%>%mutate(param="log")#%>%addMeans()
cheatData<-cheatData%>%mutate(param="cheat")#%>%addMeans()
sanctionData<-sanctionData%>%mutate(param="sanction")#%>%addMeans()
monitoringData<-monitoringData%>%mutate(param="monitoring")#%>%addMeans()

# combine all dataframes into 1 
#allData<-rbind(mtgData, costData, cheatData, sanctionData, monitoringData, rtData)
allData<-rbind(mtgData, costData, rtData, logData, cheatData, sanctionData, monitoringData)
allData<-allData%>%mutate(param=as.factor(param))

# calculate if bimass and/or K is in lower mode
sanctionData<- sanctionData%>%mutate(depleted = TotalBM<0.1, lowK = K<10.12)
monitoringData<- monitoringData%>%mutate(depleted = TotalBM<0.1, lowK = K<10.12)
allData<- allData%>%mutate(depleted = TotalBM<0.1, lowK = K<10.12)



#######################
# calculate jumps for base data 
kjumps = findKJumpTimes(dataBase, 1:6, 2000, 50)
bmjumps = findBMJumpTimes(dataBase, 1:6, 2000, 50)

jumps= kjumps%>%gather(key="run", value = "k.jump")
jumps2= bmjumps%>%gather(key="run", value = "bm.jump")
jumps = jumps%>%mutate(bm.jump=jumps2$bm.jump, yep=0)

distance = 2
for(i in distance:(dim(jumps)[1])){
  if (jumps$k.jump[i]== 1){
    if(any(jumps$bm.jump[i:(i+distance)]==1)){
      jumps$yep[i] = "BM jumps up"
    }
    else if(all(jumps$bm.jump[(i-distance):i]==-1) & any(jumps$bm.jump[i:(i+distance)]==0)){
      jumps$yep[i] = "BM stops decreasing"
    }
    else{jumps$yep[i] = "BM remains constant"}
  }
}

#original simulation 


#########
# get graphs  for monitoring = 0
getGraphs_export <- function(fileName, colNames){
  
  plotFile<-read.csv(file = fileName, header=FALSE, stringsAsFactors=FALSE)[,1:2]
  
  ticks = 2000
  i<-1
  j<-1
  out<-data.frame(1:(as.integer(ticks/10)))
  while(i<dim(plotFile)[1]){
    if(plotFile[i,2]=="y"){
      
      out[,plotFile[i-6,1]]<-plotFile[(i+1):(i+(as.integer(ticks/10))),2]
      j<- j+1
      i<- i+(as.integer(ticks/10))
    }
    i<-i+1
  }
  
  out<-out[,-1]
  
  
  colnames(out)<- colNames
  
  
  for(i in 1:length(colNames)){
    out[,i] <-as.numeric(out[,i])
  }
  
  return(out)
  
}

columnNames <- c("K", "unsatisfied", "Payoffs", "TotalBM", "GreenPatches", "beta.i", "k.i", "numLoggers", 
              "numCheaters", "highMinCut", "lowMinCut", "meanProbCheat", "midMinCut", "LegalBM", "IllegalBM", 
              "PercIllegalBM", "BMPerCheating", "BMPerLogging", "PayoffCheaters", "PayoffNonCheaters", "timesCheated", 
              "timesLogged") 

data_mon0 = getGraphs_export("./allData_IS/data_mon0.csv", columnNames)
