# setwd("~/Documents/WoosterStuff/fall2019/IS_organized/analysis/allData_IS/vallinoData")

load("./allData_IS/vallinoData/vallino_changeCost.RData")
vallino_dataCost<-dataOut
load("./allData_IS/vallinoData/vallino_changeMTG.RData")
vallino_dataMTG<-dataOut
load("./allData_IS/vallinoData/vallino_changeLog.RData")
vallino_dataLog<-dataOut
load("./allData_IS/vallinoData/vallino_changeRT.RData")
vallino_dataRT<-dataOut
load("./allData_IS/vallinoData/vallino_changeEnf.RData")
vallino_dataEnf <-dataOut

costData_vallino<-finalStats_allOutputs(vallino_dataCost, seq(0,20, by=2),  2000, 50)
mtgData_vallino<-finalStats_allOutputs(vallino_dataMTG, seq(5,30, by=5), 2000, 50)
logData_vallino<-finalStats_allOutputs(vallino_dataLog, seq(50,300, by=50),  2000, 50)
rtData_vallino<-finalStats_allOutputs(vallino_dataRT, seq(1,10, by=1),  2000, 50)
enfData_vallino<-finalStats_allOutputs(vallino_dataEnf, seq(0,100, by=10), 2000, 50)
enfData_vallino<-finalStats_allOutputs(vallino_dataEnf, seq(0,100, by=10), 2000, 50)

#removedRTData_vallino<-finalStats_allOutputs_extra(vallino_data_removed_rt, c(50), 2000, 50)

mtgData_vallino<-mtgData_vallino%>%mutate(param="mtg")
costData_vallino<-costData_vallino%>%mutate(param="cost")
rtData_vallino<-rtData_vallino%>%mutate(param="rt")
logData_vallino<-logData_vallino%>%mutate(param="log")
enfData_vallino<-enfData_vallino%>%mutate(param="enf")

allData_vallino<-rbind(mtgData_vallino, costData_vallino,  enfData_vallino, rtData_vallino)

allData_vallino<-rbind(mtgData_vallino, costData_vallino, rtData_vallino, logData_vallino,  enfData_vallino)
allData_vallino<-allData_vallino%>%mutate(param=as.factor(param))


# all simulations using base parameteres
baseData_vallino<-allData_vallino%>%filter((param=="cost" & variableValue==5)|(param=="mtg" & variableValue==20)|(param=="rt" & variableValue==5)|(param=="log" & variableValue==100)|(param=="enf" & variableValue==50))

