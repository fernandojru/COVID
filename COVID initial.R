library(dplyr)
library(dygraphs)
library(xts)
library(forecast)

COVID<-read.csv("covid_19_data.csv")

Date<-as.Date(COVID$ObservationDate, format="%m/%d/%Y") 

COVID$Date<-Date

COVID_Day<- COVID %>% group_by(Date) %>% summarise(World_confirmed=sum(Confirmed))


COVID_Day_series<-xts(COVID_Day$World_confirmed, order.by=COVID_Day$Date)

dygraph(COVID_Day_series, main = "SARS-COV2-outbreak: Total worldwide cases", xlab="Date", ylab="Total cases") %>% 
  dySeries("V1", "Total cases",drawPoints = TRUE, pointSize = 3, color=rgb(53/255,116/255,199/255),
           fillGraph = TRUE) %>% 
  dyRangeSelector()


ARIMA1<-auto.arima(COVID_Day_series)

P.AR<-predict(ARIMA1,n.ahead=10,prediction.interval=TRUE)

Low_lim<-P.AR$pred-qnorm(0.975,mean=0,sd=1)*P.AR$se
Upp_lim<-P.AR$pred+qnorm(0.975,mean=0,sd=1)*P.AR$se

## Data periods
per_1 <- as.Date(as.character(COVID_Day$Date))
per_2 <- seq(as.Date("2020-03-23",format="%Y-%m-%d"), as.Date("2020-04-01",format="%Y-%m-%d"),"1 day")


# Merge forecast + actuals
data <- xts(COVID_Day_series,order.by=per_1) 
dataNA <- rep(NA, length(data))
A <- cbind(data,dataNA,dataNA,dataNA)

Low_lim <- xts(Low_lim,order.by=per_2)
Forecast <- xts(P.AR$pred,order.by=per_2)
Upp_lim <- xts(Upp_lim,order.by=per_2)
predNA <- rep(NA, length(Forecast))
B <- cbind(predNA, Low_lim, Forecast, Upp_lim)

all_series <- data.frame(rbind(as.matrix(A),as.matrix(B)))
colnames(all_series) <- c('Actual', 'Lower_limit', 'Forecast', 'Upper_limit')

dygraph(all_series, main="SARS-COV2-outbreak: Total worldwide cases",xlab="Date", ylab="Cases")%>%
  dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast")%>%
  dyRangeSelector()

#======With new data from Kaggle (Johns Hopkins)

COVID_2<-read.csv("COVID19_3-Apr.csv")


Date<-as.Date(COVID_2$Date, format="%m/%d/%y") 

COVID_2$Date2<-Date


COVID_2_Day<- COVID_2 %>% group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))


COVID_Day_series<-xts(COVID_2_Day$World_confirmed, order.by=COVID_2_Day$Date2)

dygraph(COVID_Day_series, main = "SARS-COV2-outbreak: Total worldwide cases", xlab="Date", ylab="Total cases") %>% 
  dySeries("V1", "Total cases",drawPoints = TRUE, pointSize = 3, color=rgb(53/255,116/255,199/255),
           fillGraph = TRUE) %>% 
  dyRangeSelector()

COVID_2_Day_Lebanon<- COVID_2 %>% 
  filter(Country.Region %in% c("Lebanon")) %>% 
  group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))

COVID_2_Day_Chile<- COVID_2 %>% 
  filter(Country.Region %in% c("Chile")) %>% 
  group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))

COVID_2_Day_Colombia<- COVID_2 %>% 
  filter(Country.Region %in% c("Colombia")) %>% 
  group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))

COVID_2_Day_CostaRica<- COVID_2 %>% 
  filter(Country.Region %in% c("Costa Rica")) %>% 
  group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))


COVID_Day_series_Lebanon<-xts(COVID_2_Day_Lebanon$World_confirmed, order.by=COVID_2_Day_Lebanon$Date2)
COVID_Day_series_Chile<-xts(COVID_2_Day_Chile$World_confirmed, order.by=COVID_2_Day_Chile$Date2)
COVID_Day_series_Colombia<-xts(COVID_2_Day_Colombia$World_confirmed, order.by=COVID_2_Day_Colombia$Date2)
COVID_Day_series_CostaRica<-xts(COVID_2_Day_CostaRica$World_confirmed, order.by=COVID_2_Day_CostaRica$Date2)

Our_Countries<-cbind(COVID_Day_series_Lebanon,COVID_Day_series_Chile,COVID_Day_series_Colombia,COVID_Day_series_CostaRica)

dygraph(Our_Countries, main = "SARS-COV2-outbreak: Total cases by country", xlab="Date", ylab="Total cases") %>% 
  dySeries("COVID_Day_series_Lebanon", "Lebanon",drawPoints = TRUE, pointSize = 3) %>% 
  dySeries("COVID_Day_series_Chile", "Chile",drawPoints = TRUE, pointSize = 3) %>% 
  dySeries("COVID_Day_series_Colombia", "Colombia",drawPoints = TRUE, pointSize = 3) %>% 
  dySeries("COVID_Day_series_CostaRica", "Costa Rica",drawPoints = TRUE) %>% 
  dyRangeSelector()

#Auto Arima for Chile:

ARIMA1_Chile<-auto.arima(COVID_Day_series_Chile)

ARIMA1_Chile<-arima(COVID_Day_series_Chile, order = c(1,2,3))

P.AR_Chile<-predict(ARIMA1_Chile,n.ahead=10,prediction.interval=TRUE)

Low_lim_Chile<-P.AR_Chile$pred-qnorm(0.975,mean=0,sd=1)*P.AR_Chile$se
Upp_lim_Chile<-P.AR_Chile$pred+qnorm(0.975,mean=0,sd=1)*P.AR_Chile$se

##Data periods
per_1 <- as.Date(as.character(COVID_2_Day_Chile$Date2))
per_2 <- seq(as.Date("2020-04-01",format="%Y-%m-%d"), as.Date("2020-04-10",format="%Y-%m-%d"),"1 day")


# Merge forecast + actuals
data <- xts(COVID_Day_series_Chile,order.by=per_1) 
dataNA <- rep(NA, length(data))
A <- cbind(data,dataNA,dataNA,dataNA)


Low_lim_Chile <- xts(Low_lim_Chile,order.by=per_2)
Forecast_Chile <- xts(P.AR_Chile$pred,order.by=per_2)
Upp_lim_Chile <- xts(Upp_lim_Chile,order.by=per_2)
predNA <- rep(NA, length(Forecast))
B <- cbind(predNA, Low_lim_Chile, Forecast_Chile, Upp_lim_Chile)

all_series <- data.frame(rbind(as.matrix(A),as.matrix(B)))
colnames(all_series) <- c('Actual', 'Lower_limit', 'Forecast', 'Upper_limit')

dygraph(all_series, main="SARS-COV2-outbreak: Total Chile cases",xlab="Date", ylab="Cases")%>%
  dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast")%>%
  dyRangeSelector()




  