###############################
#setwd("~/Documents/coding_projects/IS_organized/analysis")

# Two sets of data. They were run under identical conditions (run to ensure the same results were obtained) and either can be used to create the figures.  

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

# collect all runs conducted under base parameters 
dataBase<-list()
dataBase[[1]]<- dataMTG[[4]]
dataBase[[2]]<-dataRT[[5]]
dataBase[[3]]<-dataMonitoring[[6]]
dataBase[[4]]<- dataCheat[[6]]
dataBase[[5]]<- dataSanction[[6]]
dataBase[[6]]<- dataLog[[3]]
baseData<-finalStats_allOutputs(dataBase, seq(1,6, by=1), 2000, 50)

# add parameter name to each dataframe 
mtgData<-mtgData%>%mutate(param="mtg")
costData<-costData%>%mutate(param="cost")
rtData<-rtData%>%mutate(param="rt")
logData<-logData%>%mutate(param="log")
cheatData<-cheatData%>%mutate(param="cheat")
sanctionData<-sanctionData%>%mutate(param="sanction")
monitoringData<-monitoringData%>%mutate(param="monitoring", variableValue = as.factor(as.numeric(as.character(variableValue))/100))

# combine all raw data into 1 list 
dataAll = c(dataMTG, dataRT, dataMonitoring, dataCheat, dataSanction, dataLog, dataCost)
# combine all cleaned data into one dataframe 
allData<-rbind(mtgData, costData, rtData, logData, cheatData, sanctionData, monitoringData)%>%mutate(param=as.factor(param)) 


save(dataCost, dataMTG, dataLog, dataRT, dataCheat, dataMonitoring, dataSanction, costData, mtgData, logData, rtData, monitoringData, cheatData, sanctionData, dataBase, baseData, dataAll, allData, file = "MS_model_data.RData")
