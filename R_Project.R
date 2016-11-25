Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre')

install.packages("doBy")
install.packages("data.table")
install.packages("plyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("ggmap")
install.packages("dplyr")
library(doBy)
library(data.table)
library(plyr)
library(ggplot2)
library(maps)
library(ggmap)
library(dplyr)

dailyCall <- read.csv("~/Documents/KPUDocument/Programming/DataFile/DailyCall.csv")
LFBus <- read.csv("~/Documents/KPUDocument/Programming/DataFile/LowFloorBus.csv")
Destination <- read.csv("~/Documents/KPUDocument/Programming/DataFile/Destination.csv")
BusData <- read.csv("~/Documents/KPUDocument/Programming/DataFile/BusInfo.csv")
Data <- read.csv("~/Documents/KPUDocument/Programming/DataFile/PeopleFac.csv")


#################################장애인 생활패턴 분석#####################################

totalDestination <- aggregate(Times~Area_Gu, Destination, sum)
od_Destinetion <- totalDestination[order(totalDestination$Times, decreasing = TRUE), ]
colnames(od_Destinetion) <- c("Area_Gu", "Destinetion_Times")

dailyCall$Day <- weekdays(as.Date(dailyCall$Date))
dayCall <- aggregate(CallTotal~Day, dailyCall, mean)
colnames(dayCall) <- c("Day", "AvgCall")
od_dayCall <- dayCall[order(dayCall$Day, decreasing = TRUE), ]
head(od_dayCall)

# n <- mean(dailyCall$AvgWait) #1년간의 평균대시기간 34.59375min
waitDailyCall <- subset(dailyCall, dailyCall$AvgWait > mean(dailyCall$AvgWait))
waitDayCall <- aggregate(AvgWait~Day, waitDailyCall, mean)
od_waitDayCall <- waitDayCall[order(waitDayCall$Day, decreasing = TRUE), ]

dataOfDay <- merge(od_dayCall, od_waitDayCall) # 요일별 평균 콜건수, 대기시간
dataOfArea <- merge(od_Destinetion, Data) # 자치구별 방문횟수, 노인시설/현황, 장애인시설/현황, 병원 수
dt_sort <- dataOfArea[order(dataOfArea$Silver_Facilities, decreasing = TRUE), ]
dt_sort

#################################저상버스 분포 분석#####################################
str(LFBus)
str(BusData)
LFBusData <- merge(LFBus, BusData, by="BusNumber", all=FALSE)
LFBusData <- LFBusData[order(LFBusData$Date, decreasing = FALSE), ]
head(LFBusData)

JenBusData <- subset(LFBusData, LFBusData$Date == 201501)
FebBusData <- subset(LFBusData, LFBusData$Date == 201502)
MarBusData <- subset(LFBusData, LFBusData$Date == 201503)
AplBusData <- subset(LFBusData, LFBusData$Date == 201504)
MayBusData <- subset(LFBusData, LFBusData$Date == 201505)
JunBusData <- subset(LFBusData, LFBusData$Date == 201506)
JulBusData <- subset(LFBusData, LFBusData$Date == 201507)
AugBusData <- subset(LFBusData, LFBusData$Date == 201508)
SepBusData <- subset(LFBusData, LFBusData$Date == 201509)
OctBusData <- subset(LFBusData, LFBusData$Date == 201510)
NomBusData <- subset(LFBusData, LFBusData$Date == 201511)
DecBusData <- subset(LFBusData, LFBusData$Date == 201512)

# n <- split(JenBusData, JenBusData$BusNumber)
Jen <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Jen <- Jen[c(-2,-3)]
Feb <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Feb <- Jen[c(-2,-3)]
Mar <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Mar <- Jen[c(-2,-3)]
Apl <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Apl <- Jen[c(-2,-3)]
May <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
May <- Jen[c(-2,-3)]
Jun <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Jun <- Jen[c(-2,-3)]
Jul <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Jul <- Jen[c(-2,-3)]
Aug <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Aug <- Jen[c(-2,-3)]
Sep <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Sep <- Jen[c(-2,-3)]
Oct <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Oct <- Jen[c(-2,-3)]
Nom <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Nom <- Jen[c(-2,-3)]
Dec <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Dec <- Jen[c(-2,-3)]

head(Jen)
head(Feb)
ggplot(Jen, aes(x=Jen$Group.1, y=Jen$X00.Input)) + geom_bar(stat = "identity")




