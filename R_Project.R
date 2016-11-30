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
SubwayFree <- read.csv("~/Documents/KPUDocument/Programming/DataFile/SubwayFree.csv")
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
dataOfArea[order(dataOfArea$Destinetion_Times, decreasing = FALSE), ]

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

model1 <- lm(Destinetion_Times~Area_km.2+Silver_Facilities+Disabled_Facilities+Silver_Total, data=dataOfArea)
summary(model1)
cor(Disabled$dataOfArea.Destinetion_Times, Disabled$dataOfArea.Disabled_Total) #상관계수 -1에 가까울수록 역의 상관관계 0: 관계없음 1: 정의 상관관계

with(dataOfArea, plot(Destinetion_Times, Disabled_Facilities, cex=.7, pch=as.numeric(dataOfArea$Area_Gu))) # 장애인 시설과 장애인 이동패턴의 상관관계

