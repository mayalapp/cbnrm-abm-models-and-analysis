###############################
setwd("~/Documents/coding_projects/IS_organized/analysis")

  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeCost.RData")
  dataCost_revised<-dataOut
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeMTG.RData")
  dataMTG_revised<-dataOut
  #dataMTG[[1]]<-NULL
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeLog.RData")
  dataLog_revised<-dataOut
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeRT.RData")
  dataRT_revised<-dataOut
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeCheat.RData")
  dataCheat_revised<-dataOut
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeMonitoring.RData")
  dataMonitoring_revised<-dataOut
  load("./allData_IS/M&S_revised_model_data/myMod_revised_changeSanction.RData")
  dataSanction_revised<-dataOut
  
# convert raw data to end-of-simulation summary statistics 
costData_revised<-finalStats_allOutputs(dataCost_revised, seq(0,20, by=2),  2000, 50)
mtgData_revised<-finalStats_allOutputs(dataMTG_revised, seq(5,30, by=5), 2000, 50)
logData_revised<-finalStats_allOutputs(dataLog_revised, seq(60,200, by=20),  2000, 50)
rtData_revised<-finalStats_allOutputs(dataRT_revised, seq(1,10, by=1),  2000, 50)
monitoringData_revised<-finalStats_allOutputs(dataMonitoring_revised, seq(0,100, by=10), 2000, 50)
cheatData_revised<-finalStats_allOutputs(dataCheat_revised, seq(0,10, by=1), 2000, 50)
sanctionData_revised<-finalStats_allOutputs(dataSanction_revised, seq(0,10, by=1), 2000, 50)

# collect all runs conducted under base parameters 
dataBase_revised<-list()
dataBase_revised[[1]]<- dataMTG_revised[[4]]
dataBase_revised[[2]]<-dataRT_revised[[5]]
dataBase_revised[[3]]<-dataMonitoring_revised[[6]]
dataBase_revised[[4]]<- dataCheat_revised[[6]]
dataBase_revised[[5]]<- dataSanction_revised[[6]]
dataBase_revised[[6]]<- dataLog_revised[[3]]
baseData_revised<-finalStats_allOutputs(dataBase_revised, seq(1,6, by=1), 2000, 50)

# add parameter name to each dataframe 
mtgData_revised<-mtgData_revised%>%mutate(param="mtg")
costData_revised<-costData_revised%>%mutate(param="cost")
rtData_revised<-rtData_revised%>%mutate(param="rt")
logData_revised<-logData_revised%>%mutate(param="log")
cheatData_revised<-cheatData_revised%>%mutate(param="cheat")
sanctionData_revised<-sanctionData_revised%>%mutate(param="sanction")
monitoringData_revised<-monitoringData_revised%>%mutate(param="monitoring", variableValue = as.factor(as.numeric(as.character(variableValue))/100))

# combine all raw data into 1 list 
dataAll_revised = c(dataMTG_revised, dataRT_revised, dataMonitoring_revised, dataCheat_revised, dataSanction_revised, dataLog_revised, dataCost_revised)
# combine all cleaned data into one dataframe 
allData_revised<-rbind(mtgData_revised, costData_revised, rtData_revised, logData_revised, cheatData_revised, sanctionData_revised, monitoringData_revised)%>%mutate(param=as.factor(param)) 


save(dataCost_revised, dataMTG_revised, dataLog_revised, dataRT_revised, dataCheat_revised, dataMonitoring_revised, dataSanction_revised, costData_revised, mtgData_revised, logData_revised, rtData_revised, monitoringData_revised, cheatData_revised, sanctionData_revised, dataBase_revised, baseData_revised, dataAll_revised, allData_revised, file = "MS_revised_model_data_clean.RData")
