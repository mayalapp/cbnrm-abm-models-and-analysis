## data cleaning functions 

#varName is the variable (graph) we're interested in, e.g. total biomass or # of loggers 
# paramSet is the names for the columns for different parameter options run (e.g. if we want the total BM for simulations where we've run 100 loggers, 200 loggers, 300 loggers.... param set may be c("bm100.", "bm200."....)
# ticks is number of ticks run in simulation 
# runs is number of times the simulation was run 
#dataIn is list with all of the lists from the simulation, e.g. data1<-list(log200, log300)
makeDF<- function(dataIn, varName, paramSet, ticks, runs){
  output<-data.frame(1:(as.integer(ticks/10)))
  for(j in 1: length(paramSet)){
    for(i in 1:runs){
      output[paste(paramSet[j], i, sep='_')] <- dataIn[[j]][[i]][varName]
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
  
  varNames<- paste(variableOfInterest, variableValues, sep="_")
  out<-makeDF(dataIn, variableOfInterest,varNames , ticks, runs)
  
  out1<-data.frame(1:runs)
  for(i in 1:length(variableValues)){
    x<-t(out[1,(runs*(i-1)+1):(runs*i)]); colnames(x)<-paste("ini", varNames[i], sep="_")
    out1[,paste("ini", varNames[i], sep="_")] <- x
    
    x<-t(out[as.integer(ticks/10),(runs*(i-1)+1):(runs*i)]); colnames(x)<-paste("final", varNames[i], sep="_")
    out1[,paste("final", varNames[i], sep="_")] <- x
  }
  out1<-out1[,-1]
  
  if(statType=="final"){
    out2 <- out1%>%gather(key="variableValue", value=variableOfInterest)%>%filter(grepl("final", variableValue))
  }else{
    if(statType=="perc"){
      for(i in 1:length(variableValues)){
        x<-out1[, paste("final", varNames[i], sep="_")]/out1[, paste("ini", varNames[i], sep="_")];   colnames(x)<-paste("perc", varNames[i], sep="_")
        out1[, paste("perc", varNames[i], sep="_")] <- x
      }
      out2 <- out1%>%gather(key="variableValue", value=variableOfInterest)%>%filter(grepl("perc", variableValue))
    }
  }
  
  out2<-out2%>%separate(col = variableValue, into = c("type", "variable", "variableValue"), sep = "\\_")%>%mutate(variableValue=as.factor(as.numeric(variableValue)))
  
  out2[, variableOfInterest]<-out2[,"variableOfInterest"]
  
  return(out2)
}


finalStats_allOutputs <- function(dataIn, paramVals, ticks, runs){
  
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
  varNames<- paste("K", variableValues, sep="_")
  
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
  varNames<- paste("K", variableValues, sep="_")
  
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
  varNames<- paste("K", variableValues, sep="_")
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
  varNames<- paste("K", variableValues, sep="_")
  
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
  varNames<- paste("TotalBM", variableValues, sep="_")
  
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
