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
Subway <- read.csv("~/Documents/KPUDocument/Programming/DataFile/amongSubway.csv")
CallTime <- read.csv("~/Documents/KPUDocument/Programming/DataFile/dailyCallTime.csv", header = FALSE) #월별 시간대별 콜택시 이용현황


#################################장애인 생활권 분석#####################################

totalDestination <- aggregate(Times~Area_Gu, Destination, sum)
od_Destinetion <- totalDestination[order(totalDestination$Times, decreasing = TRUE), ]
colnames(od_Destinetion) <- c("Area_Gu", "Destinetion_Times")
Area_data <- merge(od_Destinetion, Subway)
str(Area_data)

dailyCall$Day <- weekdays(as.Date(dailyCall$Date))
avgMove <- aggregate(Distance~Day, dailyCall, mean)
dayCall <- aggregate(CallTotal~Day, dailyCall, mean)
colnames(avgMove) <- c("Day", "AvgDist")
colnames(dayCall) <- c("Day", "AvgCall")
dataOfAvgDays <- merge(avgMove, dayCall)

# n <- mean(dailyCall$AvgWait) #1년간의 평균대시기간 34.59375min
waitDailyCall <- subset(dailyCall, dailyCall$AvgWait > mean(dailyCall$AvgWait))
waitDayCall <- aggregate(AvgWait~Day, waitDailyCall, mean)
od_waitDayCall <- waitDayCall[order(waitDayCall$Day, decreasing = TRUE), ]

dataOfDay <- merge(dataOfAvgDays, od_waitDayCall) # 요일별 평균 이동거리, 콜건수, 대기시간
dataOfArea <- merge(Area_data, Data) # 자치구별 평균 방문횟수, 노인시설/현황, 장애인시설/현황, 병원 수, 지하철역 인근정류장 수, 면적, 
dataOfDay[order(dataOfDay$AvgCall, decreasing = TRUE), ]
dataOfArea[order(dataOfArea$Area_Gu, decreasing = TRUE), ]

model1 <- lm(Destinetion_Times~Area_km.2+Silver_Facilities+Disabled_Facilities+Silver_Total, data=dataOfArea)
summary.aov(model1)
with(dataOfArea, plot(Area_km.2, Disabled_Facilities, cex=.7, pch=as.numeric(dataOfArea$Area_Gu)))

####################################시계열 분 석(시간별)#########################################

m_CallTime <- as.matrix(CallTime)
v_CallTime <- as.vector(m_CallTime)
v_CallTime.ts <- ts(data = v_CallTime, start = 1, frequency = 24) # 장애인 콜택시 월별 이용현황 시각화 
plot(v_CallTime.ts)

m_JanCallTime <- as.matrix(CallTime[,1])
v_JanCallTime <- as.vector(m_JanCallTime)
v_JanCallTime.ts <- ts(data = v_JanCallTime, start = 1, frequency = 1) # 1월 장애인 콜택시 시간별 이용현황 시각화
m_FebCallTime <- as.matrix(CallTime[,2])
v_FebCallTime <- as.vector(m_FebCallTime)
v_FebCallTime.ts <- ts(data = v_FebCallTime, start = 1, frequency = 1) # 2월 장애인 콜택시 시간별 이용현황 시각화
m_MarCallTime <- as.matrix(CallTime[,3])
v_MarCallTime <- as.vector(m_MarCallTime)
v_MarCallTime.ts <- ts(data = v_MarCallTime, start = 1, frequency = 1) # 3월 장애인 콜택시 시간별 이용현황 시각화
m_AprCallTime <- as.matrix(CallTime[,4])
v_AprCallTime <- as.vector(m_AprCallTime)
v_AprCallTime.ts <- ts(data = v_AprCallTime, start = 1, frequency = 1) # 4월 장애인 콜택시 시간별 이용현황 시각화
m_MayCallTime <- as.matrix(CallTime[,5])
v_MayCallTime <- as.vector(m_mayCallTime)
v_MayCallTime.ts <- ts(data = v_mayCallTime, start = 1, frequency = 1) # 5월 장애인 콜택시 시간별 이용현황 시각화

s_CallTime <- apply(CallTime[,1:10], 1, sum)
plot(s_CallTime)

####################################회 귀 분 석(지역별)#########################################

Disabled <- data.frame(dataOfArea$Area_Gu,dataOfArea$Destinetion_Times, dataOfArea$Disabled_Total, dataOfArea$Disabled_Facilities)
plot(Disabled$dataOfArea.Destinetion_Times~Disabled$dataOfArea.Disabled_Total+Disabled$dataOfArea.Disabled_Facilities)
cor(Disabled$dataOfArea.Destinetion_Times, Disabled$dataOfArea.Disabled_Total) #상관계수 -1에 가까울수록 역의 상관관계 0: 관계없음 1: 정의 상관관계
r <- lm(Disabled$dataOfArea.Destinetion_Times~Disabled$dataOfArea.Disabled_Total)

#################################저상버스 분포 분석#####################################
str(LFBus)
str(BusData)
LFBusData <- merge(LFBus, BusData, by="BusNumber", all=FALSE)
LFBusData <- LFBusData[order(LFBusData$Date, decreasing = FALSE), ]
head(LFBusData)

JanBusData <- subset(LFBusData, LFBusData$Date == 201501)
FebBusData <- subset(LFBusData, LFBusData$Date == 201502)
MarBusData <- subset(LFBusData, LFBusData$Date == 201503)
AprBusData <- subset(LFBusData, LFBusData$Date == 201504)
MayBusData <- subset(LFBusData, LFBusData$Date == 201505)
JunBusData <- subset(LFBusData, LFBusData$Date == 201506)
JulBusData <- subset(LFBusData, LFBusData$Date == 201507)
AugBusData <- subset(LFBusData, LFBusData$Date == 201508)
SepBusData <- subset(LFBusData, LFBusData$Date == 201509)
OctBusData <- subset(LFBusData, LFBusData$Date == 201510)
NovBusData <- subset(LFBusData, LFBusData$Date == 201511)
DecBusData <- subset(LFBusData, LFBusData$Date == 201512)

# n <- split(JenBusData, JenBusData$BusNumber)
Jan <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Jan <- Jen[c(-2,-3)]
Feb <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Feb <- Jen[c(-2,-3)]
Mar <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Mar <- Jen[c(-2,-3)]
Apr <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Apr <- Jen[c(-2,-3)]
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
Nov <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Nov <- Jen[c(-2,-3)]
Dec <-aggregate(JenBusData[,3:52], by=list(JenBusData$BusNumber), FUN=sum)
Dec <- Jen[c(-2,-3)]

str(Jan)
head(Feb)
ggplot(Jen, aes(x=Jen$Group.1, y=Jen$X00.Input)) + geom_bar(stat = "identity")




