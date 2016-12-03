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
WaitTime <- read.csv("~/Documents/KPUDocument/Programming/DataFile/dailyWait.csv")

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

# 콜택시 대기시간 분석
m_WaitTime <- as.matrix(WaitTime)
v_WaitTime <- as.vector(m_WaitTime)
v_WaitTime.ts <- ts(data = v_WaitTime, start = 1, frequency = 24)
plot(v_WaitTime.ts)
abline(a=400, b=0, col="red")

fill_colors = c() # list or vector in color info 
for (i in 1:length(s1_WaitTime)){ # 40분 이상의 대기가 이루어지는 시간대와 그렇지않은 시간대의 구분
  if (s1_WaitTime[i] > 40){ # s1 or s2
    fill_colors = c(fill_colors, "#821122")
  }else {
    fill_colors = c(fill_colors, "#cccccc")
  }
}

s1_WaitTime <- apply(WaitTime[,1:6], 1, mean)
barplot(s1_WaitTime, main = "Jan to Jun", col= fill_colors, border = NA)
s2_WaitTime <- apply(WaitTime[,7:10], 1, mean)
barplot(s2_WaitTime, main = "Jul to Oct", col= fill_colors, border = NA)

# 콜택시 사용건수 분석
m_CallTime <- as.matrix(CallTime)
v_CallTime <- as.vector(m_CallTime)
v_CallTime.ts <- ts(data = v_CallTime, start = 1, frequency = 24) # 장애인 콜택시 월별 이용현황 시각화 
plot(v_CallTime.ts)
abline(a=400, b=0, col="red")

fill_colors = c() # list or vector in color info 
for (i in 1:length(s2_CallTime)){ # 400명 이상의 교통이동이 이루어지는 시간대와 그렇지않은 시간대의 구분
  if (s1_CallTime[i] > 400){ # s1 or s2
    fill_colors = c(fill_colors, "#821122")
  }else {
    fill_colors = c(fill_colors, "#cccccc")
  }
}

# 1월~6월까지의 데이터
s1_CallTime <- apply(CallTime[,1:6], 1, mean)
barplot(s1_CallTime, main = "Jan to Jun", names.arg = 0:23, col= fill_colors, border = NA)
#plot(s1_CallTime, type = "h", main = "Jan to Jun")
#abline(a=1550, b=0, col="red")

# 7월~10월까지의 데이터
s2_CallTime <- apply(CallTime[7:10], 1, mean)
barplot(s2_CallTime, main = "Jul to Oct", names.arg = 0:23, col= fill_colors, border = NA)
#plot(s2_CallTime, type = "h", main = "Jul to Oct")
#abline(a=1500, b=0, col="red")

##저상버스 패턴분석
LFBusData <- merge(LFBus, BusData, by="BusNumber", all=FALSE)
LFBusData <- LFBusData[order(LFBusData$Date, decreasing = FALSE), ]
BusData <- aggregate(LFBusData[,3:52], by = list(LFBusData$BusNumber), FUN=mean)
BusData <- BusData[c(-1,-2,-3)]
m_Bus <- as.matrix(BusData)
m_Bus.ts <- ts(data = m_Bus, start =1, frequency = 48 )
head(m_Bus.ts)
barplot(m_Bus.ts, main = "LowFloorBus")
abline(a=60000, b=0, col="red")

####################################회 귀 분 석(지역별)#########################################
cor(dataOfArea$Disabled_Facilities, dataOfArea$Destinetion_Times) # 장애인 시설 - 0.5892295 정상관
cor(dataOfArea$Disabled_Total, dataOfArea$Destinetion_Times) # 장애인 거주 - 0.6170059 정상관
Disabled1 <- lm(Destinetion_Times~Disabled_Total, data=dataOfArea)
Disabled2 <- lm(Destinetion_Times~Disabled_Facilities, data=dataOfArea)
summary(Disabled1) # model 평가
summary(Disabled2)
with(dataOfArea, plot(Destinetion_Times, Disabled_Total, cex=.7, pch=as.numeric(dataOfArea$Area_Gu))) # 장애인 거주현황과 장애인 이동패턴의 상관관계
with(dataOfArea, plot(Destinetion_Times, Disabled_Facilities, cex=.7, pch=as.numeric(dataOfArea$Area_Gu))) # 장애인 시설과 장애인 이동패턴의 상관관계

plot(Disabled1)
plot(Disabled2)