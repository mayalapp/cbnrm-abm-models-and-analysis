setwd("~/Documents/coding_projects/IS_organized/analysis")
source(file="dataCleaningFunctions.R") # file containing all functions needed for data cleaning process
library(tidyverse)

load("./allData_IS/vallinoCorrectedData/vallinoCorrected_changeCost.RData")
vallinoCorrected_dataCost<-dataOut
load("./allData_IS/vallinoCorrectedData/vallinoCorrected_changeMTG.RData")
vallinoCorrected_dataMTG<-dataOut
load("./allData_IS/vallinoCorrectedData/vallinoCorrected_changeLog.RData")
vallinoCorrected_dataLog<-dataOut
load("./allData_IS/vallinoCorrectedData/vallinoCorrected_changeRT.RData")
vallinoCorrected_dataRT<-dataOut
load("./allData_IS/vallinoCorrectedData/vallinoCorrected_changeMonitoring.RData")
vallinoCorrected_dataEnf <-dataOut

costData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataCost, seq(0,20, by=2),  2000, 50)
mtgData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataMTG, seq(5,30, by=5), 2000, 50)
logData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataLog, seq(50,300, by=50),  2000, 50)
rtData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataRT, seq(1,10, by=1),  2000, 50)
enfData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataEnf, seq(0,100, by=10), 2000, 50)
enfData_vallinoCorrected<-finalStats_allOutputs(vallinoCorrected_dataEnf, seq(0,100, by=10), 2000, 50)

#removedRTData_vallinoCorrected<-finalStats_allOutputs_extra(vallinoCorrected_data_removed_rt, c(50), 2000, 50)

mtgData_vallinoCorrected<-mtgData_vallinoCorrected%>%mutate(param="mtg")
costData_vallinoCorrected<-costData_vallinoCorrected%>%mutate(param="cost")
rtData_vallinoCorrected<-rtData_vallinoCorrected%>%mutate(param="rt")
logData_vallinoCorrected<-logData_vallinoCorrected%>%mutate(param="log")
enfData_vallinoCorrected<-enfData_vallinoCorrected%>%mutate(param="enf")

allData_vallinoCorrected<-rbind(mtgData_vallinoCorrected, costData_vallinoCorrected,  enfData_vallinoCorrected, rtData_vallinoCorrected)

allData_vallinoCorrected<-rbind(mtgData_vallinoCorrected, costData_vallinoCorrected, rtData_vallinoCorrected, logData_vallinoCorrected,  enfData_vallinoCorrected)
allData_vallinoCorrected<-allData_vallinoCorrected%>%mutate(param=as.factor(param))


# all simulations using base parameteres
baseData_vallinoCorrected<-allData_vallinoCorrected%>%filter((param=="cost" & variableValue==5)|(param=="mtg" & variableValue==20)|(param=="rt" & variableValue==5)|(param=="log" & variableValue==100)|(param=="enf" & variableValue==50))


save(vallinoCorrected_dataCost, vallinoCorrected_dataMTG, vallinoCorrected_dataLog, vallinoCorrected_dataRT, vallinoCorrected_dataEnf, 
     costData_vallinoCorrected, mtgData_vallinoCorrected, logData_vallinoCorrected, rtData_vallinoCorrected, enfData_vallinoCorrected, 
     baseData_vallinoCorrected, allData_vallinoCorrected, file = "CE_Corrected_model_data_clean.RData")

