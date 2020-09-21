remove(list = ls())
#Import Necessary Library
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("astsa")
# install.packages("forecast")
# install.packages("readxl")
# install.packages("urca")
# install.packages("ggfortify")
# install.packages("tsutils")
# install.packages("hydroTSM")
# install.packages("writexl")
# install.packages("zoo")
#install.packages("caret")
# install.packages("hrbrthemes")
install.packages("ggpubr")
# library(hrbrthemes)

library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(tidyverse)
library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(urca)
library(ggfortify)
library(tsutils)
library(hydroTSM)
library(writexl)
library(zoo)
library(scales)
library(caret)
library(tidyr)
library(ggpubr)

options(stringsAsFactors = FALSE)

## NAROK DATA
#Import Data
narok <- read_excel("/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/Narok rainfall (1960-2014).xlsx")
#View(narok)
summary(narok)

#Cleaning the data
      ## Return the column names containing missing observations
      list_na <- colnames(narok)[ apply(narok, 2, anyNA) ]
      list_na

      # Create mean
      average_missing <- apply(narok[,colnames(narok) %in% list_na],
                               2,
                               mean,
                               na.rm =  TRUE)
      average_missing

      # Quick code to replace missing values with the mean
      narok_impute_mean <- data.frame(
        sapply(
          narok,
          function(x) ifelse(is.na(x),
                             mean(x, na.rm = TRUE),
                             x)))
      #View(narok_impute_mean)

      #Export to excel
      # library("writexl")
      # write_xlsx(narok_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/narok_impute_mean.xlsx")

      #Group by year
      narok_by_year<-aggregate(.~narok_impute_mean$Year, narok_impute_mean[c(3:14)], sum)
      View(narok_by_year)

      #reshape data
      res <- reshape(narok_by_year, direction="long", varying=list(2:13), v.names=c("Rainfall"), times = month.name, timevar = "Month")
      colnames(res)[1]<-"Year"
      narok_res<-res[-c(0,4)]
      view(narok_res)

      #Creating date column
      narok_res$Date <- as.yearmon(paste(narok_res$Month,narok_res$Year))
      narok_res<-narok_res[-c(1:2)]
      narok_res<-mutate(narok_res, narok_res$Date <- as.Date(narok_res$Date, format= "%Y-%m-%d"))
      narok_data<-narok_res[-c(2)]
      colnames(narok_data)[2]<-"Date"
      class(narok_data$Date)
      class(narok_data$Rainfall)

# plot the data using ggplot
ggplot(data = narok_data, aes(x = Date, y = Rainfall)) +
  geom_line() +
  labs(x = "Date",
       y = "Total Precipitation (mm)",
       title = "Precipitation Data",
       subtitle = "Narok Rainfall 2000 - 2014")+
  scale_x_date(limits = c(as.Date("2000-01-01","%Y-%m-%d"),as.Date("2014-01-01","%Y-%m-%d")),labels = date_format("%b-%Y"), date_breaks=("1 year"))

# #Converting dataframe To Time Series
narok_data<-narok_data[order(as.Date(narok_data$Date, format="%Y-%m-%d")),]
# view(narok_data)
narok_ts <- ts(narok_data$Rainfall,frequency = 12,start = c(1960,1),end=c(2014,12))

# #Selecting Data
narok_ts <- window(narok_ts, start=c(2000,1),end=c(2014,12))
narok_ts
str(narok_ts)

#divide the time series into three components (Trend, Seasonality, Remainder)
narok_ts.stl <- stl(narok_ts, s.window = 'periodic')

#Plot narok_ts.stlosition
autoplot(narok_ts.stl) + theme_bw() + scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  ggtitle("Three Component Time series")

# Measure trend and seasonality strength:
Tt <- trendcycle(narok_ts.stl)
St <- seasonal(narok_ts.stl)
Rt <- remainder(narok_ts.stl)
#Trend Strength Calculation
Ft <- round(max(0,1 - (var(Rt)/var(Tt + Rt))),1)
#Seasonal Strength Calculation
Fs <- round(max(0,1 - (var(Rt)/var(St + Rt))),1)
data.frame('Trend Strength'= Ft , 'Seasonal Strength' =Fs)

#SEASONAL ANALYSIS
      #Seasonal Plot
      seasonplot(narok_ts, year.labels = TRUE, col = 1:13,
                 main =  "Seasonal Plot", ylab= "Rainfall (mm)",xlab = "Month")

      #Seasonal Sub-Series Plot
      seasplot(narok_ts, outplot = 3, trend = FALSE,
               main = "Seasonal Subseries Plot", ylab= "Rainfall (mm)",xlab = "Month")

      #Seasonal Boxplot
      seasplot(narok_ts, outplot = 2, trend = FALSE,
               main = "Seasonal Box Plot", ylab= "Rainfall (mm)",xlab = "Month")

## UASIN GISHU DATA
#Import Data
UAG <- read.csv('~/Desktop/twiga data/narok analysis/Uasin Gishu rainfall 1981-2020.csv')
view(UAG)
#Cleaning the data
      # Return the column names containing missing observations
      list_na2 <- colnames(UAG)[ apply(UAG, c(1,2), anyNA) ]
      list_na2
      # Create mean
      average_missing2 <- apply(UAG[,colnames(UAG) %in% list_na2],
                               2,
                               mean,
                               na.rm =  TRUE)
      average_missing2

      # Quick code to replace missing values with the mean
      UAG_impute_mean <- data.frame(
        sapply(
          UAG,
          function(x) ifelse(is.na(x),
                             mean(x, na.rm = TRUE),
                             x)))
      View(UAG_impute_mean)

      #Export to excel
      # library("writexl")
      # write_xlsx(UAG_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/UAG_impute_mean.xlsx")

      #Remove first column
      UAG_impute_mean<- UAG_impute_mean[-c(1)]
      summary(UAG_impute_mean)

      #reshape data
      YearDate <- as.Date(UAG_impute_mean$Date, format= "%d/%m/%Y")
      UAG_impute_mean<-mutate(UAG_impute_mean, YearDate)
      class(UAG_impute_mean$YearDate)
      Rainfall<-as.numeric(as.character(UAG_impute_mean$Rainfall.mm.))
      UAG_impute_mean<-mutate(UAG_impute_mean, Rainfall)
      class(UAG_impute_mean$Rainfall)
      UAG_impute_mean$Year <- format(as.Date(UAG_impute_mean$YearDate), "%Y")
      UAG_impute_mean$Month <- format(as.Date(UAG_impute_mean$YearDate), "%m")
      UAG_impute_mean<-UAG_impute_mean[-c(1:3)]
      View(UAG_impute_mean)

      #Sum by year
      UAG_by_monthyear<-aggregate(.~UAG_impute_mean$Year+UAG_impute_mean$Month, UAG_impute_mean[c(1)], sum)
      colnames(UAG_by_monthyear)[1]<-"Year"
      colnames(UAG_by_monthyear)[2]<-"Month"
      UAG_by_monthyear<-UAG_by_monthyear[order(as.Date(UAG_by_monthyear$Month, format="%m")),]
      View(UAG_by_monthyear)

      #Make months columns
      UAG_res <- reshape(UAG_by_monthyear, idvar="Year",timevar = "Month",direction="wide")
      monthnames<-month.abb
      colnames(UAG_res)[c(2:13)]<-monthnames

      #reshape data
      UAG_res <- reshape(UAG_res, direction="long", varying=list(2:13), v.names=c("Rainfall"), times = month.name, timevar = "Month")
      UAG_res<-UAG_res[-c(4)]
      view(UAG_res)

      #Creating class date and year column
      UAG_res$Date <- as.yearmon(paste(UAG_res$Month,UAG_res$Year))
      UAG_res<-UAG_res[-c(1:2)]
      UAG_res<-mutate(UAG_res, UAG_res$Date <- as.Date(UAG_res$Date, format= "%Y-%m-%d"))
      UAG_data<-UAG_res[-c(2)]
      colnames(UAG_data)[2]<-"Date"
      class(UAG_data$Date)
      class(UAG_data$Rainfall)
      view(UAG_data)

# plot the data using ggplot
ggplot(data = UAG_data, aes(x = Date, y = Rainfall)) +
  geom_line() +
  labs(x = "Date",
       y = "Total Precipitation (mm)",
       title = "Precipitation Data",
       subtitle = "UAG Rainfall 2005 - 2019")+
  scale_x_date(limits = c(as.Date("2005-01-01","%Y-%m-%d"),as.Date("2019-01-01","%Y-%m-%d")),labels = date_format("%b-%Y"), date_breaks=("1 year"))

#Converting dataframe To Time Series
UAG_data<-UAG_data[order(as.Date(UAG_data$Date, format="%Y-%m-%d")),]
UAG_ts <- ts(UAG_data$Rainfall,frequency = 12,start = c(1981,1),end=c(2019,12))

#Selecting Data
UAG_ts <- window(UAG_ts, start=c(2005,1),end=c(2019,12))
UAG_ts
str(UAG_ts)

#divide the time series into three components (Trend, Seasonality, Remainder)
UAG_ts.stl <- stl(UAG_ts, s.window = 'periodic')

#Plot UAG_ts.stlosition
autoplot(UAG_ts.stl) + theme_bw() + scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  ggtitle("Three Component Time series")

# Measure trend and seasonality strength:
Tt <- trendcycle(UAG_ts.stl)
St <- seasonal(UAG_ts.stl)
Rt <- remainder(UAG_ts.stl)
#Trend Strength Calculation
Ft <- round(max(0,1 - (var(Rt)/var(Tt + Rt))),1)
#Seasonal Strength Calculation
Fs <- round(max(0,1 - (var(Rt)/var(St + Rt))),1)
data.frame('Trend Strength'= Ft , 'Seasonal Strength' =Fs)

#SEASONAL ANALYSIS
      #Seasonal Plot
      seasonplot(UAG_ts, year.labels = TRUE, col = 1:13,
                 main =  "Seasonal Plot", ylab= "Rainfall (mm)",xlab = "Month")

      #Seasonal Sub-Series Plot
      seasplot(UAG_ts, outplot = 3, trend = FALSE,
               main = "Seasonal Subseries Plot", ylab= "Rainfall (mm)",xlab = "Month")

      #Seasonal Boxplot
      seasplot(UAG_ts, outplot = 2, trend = FALSE,
               main = "Seasonal Box Plot", ylab= "Rainfall (mm)",xlab = "Month")
      
# MAIZE YIELD DATA ANALYSIS
      #Cleaning the data
      maize_data <- read.csv("/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/FAOSTAT_data_8-22-2020.csv")
      view(maize_data)
      summary(maize_data)
      
      ##Remove columns not needed
      maize_data<-maize_data[-c(1:5,7:9,11,13,14)]
      view(maize_data)
      
      ##Reshape data
      maize_data_res <- reshape(maize_data,idvar="Year",timevar = "Element",direction="wide")
      view(maize_data_res)
      
      ##Check for NAs
      list_na3 <- colnames(maize_data_res)[ apply(maize_data_res, 2, anyNA) ]
      list_na3
      
      #Create mean
      average_missing3 <- apply(maize_data_res[,colnames(maize_data_res) %in% list_na3],
                               2,
                               mean,
                               na.rm =  TRUE)
      average_missing3
      
      # Quick code to replace missing values with the mean
      maize_data_imputedmean <- data.frame(
        sapply(
          maize_data_res,
          function(x) ifelse(is.na(x),
                             mean(x, na.rm = TRUE),
                             x)))
      colnames(maize_data_imputedmean)<-c("Year","Areaharvested","Yield","Production")
      view(maize_data_imputedmean)
      str(maize_data_imputedmean)
      summary(maize_data_imputedmean)
      
      # Simple plot
      ggplot(maize_data_imputedmean, aes(x=Year)) + 
        # geom_line(aes(y =Areaharvested), color = "darkred") #+ 
        # geom_line(aes(y =Yield), color="steelblue", linetype="twodash") #+
        geom_line(aes(y =Production), color="green")
     
       # plot(maize_data_imputedmean)
      
      #Export to excel
      # library("writexl")
      # write_xlsx(narok_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/narok_impute_mean.xlsx")
      
      
      
      
      
      
      