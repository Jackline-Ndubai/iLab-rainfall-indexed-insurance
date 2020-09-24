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
# install.packages("writexl")
# install.packages("zoo")
# install.packages("caret")
# install.packages("data.table")
# install.packages("olsrr")
# install.packages("ggpubr")
# install.packages("heatmaply")

library(plyr)
library(devtools)
library(data.table)
library(olsrr)
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
library(writexl)
library(zoo)
library(scales)
library(caret)
library(tidyr)
library(ggpubr)
library(heatmaply)

options(stringsAsFactors = FALSE)

## NAROK DATA
#Import Data
narok <- read_excel("/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/Narok rainfall (1960-2014).xlsx")
# View(narok)
summary(narok)

#Cleaning the data
    # Check for outliers
      ## Going into daily details
        #count number of times daily amounts were more than 40mm in each column
          dailyover_50mm <- ldply(narok[3:14], function(c) sum(c > 50,na.rm=T))
          ggplot(dailyover_50mm, aes(x=.id, y=V1)) +
            geom_bar(stat="identity", colour="black", fill="red") +
            xlab("") + ylab("")

        #Impute daily amount over 50mm
          narok_nonseason<-apply(narok[c(3,4,8:12)], 2, function(x) ifelse(x > 50, 0.1*x, x))
          narok_inseason<-apply(narok[c(5:7,13,14)], 2, function(x) ifelse(x > 50, 0.5*x, x))
          # View(narok_nonseason)
          # View(narok_inseason)
          narok<-cbind(narok[c(1,2)],narok_nonseason,narok_inseason)
          summary(narok)

        #Return the column names containing missing observations
        list_na <- colnames(narok)[ apply(narok, 2, anyNA) ]
        list_na

        # Create mean
        average_missing <- apply(narok[,colnames(narok) %in% list_na],
                                 2,
                                 mean,
                                 na.rm =  TRUE)
        average_missing

        # Quick code to replace missing values with the mean
        narok <- data.frame(
          sapply(
            narok,
            function(x) ifelse(is.na(x),
                               mean(x, na.rm = TRUE),
                               x)))
        # View(narok)

#Export to excel
# library("writexl")
# write_xlsx(narok_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/narok_impute_mean.xlsx")

      ## Going into yearly details  
      #Group by year
      narok_by_year<-aggregate(.~narok$Year, narok[c(3:14)], sum)
      # View(narok_by_year)
      summary(narok_by_year)
      
      narok_outliers<-narok_by_year # Copy old dataset
      outvalue<-list() # Create empty lists
      outindex<-list()
      # histvalue<-list()
      for (i in 2:ncol(narok_outliers)) { # For every column in dataset
        # histvalue[[i]]<-hist(narok_outliers[,i]) #Plot histogram
        outvalue[[i]]<-boxplot.stats(narok_outliers[,i])$out # Get the outlier value
        outindex[[i]]<-match(outvalue[[i]],narok_outliers[,i]) # Get the outlier index
        # narok_outliers[outindex[[i]],i] <- NA # Remove the outliers
      }
      
      outvalue
      outindex
      # View(narok_outliers)
      
      #Plotting histogram with outliers removed and gauge the difference
      histvalue<-list()
      for (i in 2:ncol(narok_by_year)) { # For every column in your dataset
        histvalue[[i]]<-hist(narok_by_year[,i]) #Plot histogram
      }
      
      
      #Return the column names containing NA values
      list_na11 <- colnames(narok_outliers)[ apply(narok_outliers, 2, anyNA) ]
      list_na11
      
      # Create mean
      average_missing11 <- apply(narok_outliers[,colnames(narok_outliers) %in% list_na11],
                               2,
                               mean,
                               na.rm =  TRUE)
      average_missing11
      
      # Quick code to replace missing values with the mean
      narok_by_year <- data.frame(
        sapply(
          narok_outliers,
          function(x) ifelse(is.na(x),
                             mean(x, na.rm = TRUE),
                             x)))
      
      summary(narok_by_year)
      # View(narok_by_year)
      
      #reshape data
      narok_by_year<-narok_by_year[,c(1:3,9:11,4:8,12,13)] #reordering columns to follow according to month
      res <- reshape(narok_by_year, direction="long", varying=list(2:13), v.names=c("Rainfall"), times = month.name, timevar = "Month") #Creating one month column with corresponding rainfall values
      colnames(res)[1]<-"Year"
      narok_res<-res[-c(0,4)]
      # View(narok_res)
      
      ## For use in regression model=> Sum by year
      narok_by_year_rainfall<- rowSums(narok_by_year[, c(3:6)]) #taking 4 months from February
      # View(narok_by_year_rainfall)
      narok_by_year_regr<-cbind(narok_by_year_rainfall,narok_by_year[,1])
      narok_by_year_regr<-as.data.frame(narok_by_year_regr)
      colnames(narok_by_year_regr)<-c("Rainfall","Year")
      narok_by_year_regr$Rainfall<-as.integer(as.character(narok_by_year_regr$Rainfall))
      narok_by_year_regr$Year<-as.integer(as.character(narok_by_year_regr$Year))
      str(narok_by_year_regr)

      #Creating date column
      narok_res$Date <- as.yearmon(paste(narok_res$Month,narok_res$Year))
      narok_res<-narok_res[-c(1:2)]
      narok_res$Date<- as.Date(narok_res$Date, format= "%Y-%m-%d")
      class(narok_res$Date)
      # View(narok_res)
      narok_data<-narok_res
      # View(narok_data)
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