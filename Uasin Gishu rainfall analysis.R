# remove(list = ls())

# library(plyr)
# library(devtools)
# library(data.table)
# library(olsrr)
# library(lubridate)
# library(ggplot2)
# theme_set(theme_bw())
# library(tidyverse)
# library(dplyr)
# library(astsa)
# library(forecast)
# library(readxl)
# library(urca)
# library(ggfortify)
# library(tsutils)
# library(writexl)
# library(zoo)
# library(scales)
# library(caret)
# library(tidyr)
# library(ggpubr)
# library(heatmaply)
# 
# options(stringsAsFactors = FALSE)

## UASIN GISHU DATA
#Import Data
  UAG <- read.csv('~/Desktop/twiga data/narok analysis/Uasin Gishu rainfall 1981-2020.csv')
  # view(UAG)
  summary(UAG)

  #Return the column names containing missing observations (before reshaping)
  UAGlist_na <- colnames(UAG)[ apply(UAG, 2, anyNA) ]
  UAGlist_na

#Reshape data
  YearDate <- as.Date(UAG$Date, format= "%d/%m/%Y")
  UAG<-mutate(UAG, YearDate)
  colnames(UAG)[3]<-"Rainfall"
  UAG<-UAG[-c(1,2)]
  UAG<-UAG[order(as.Date(UAG$YearDate, format="%Y-%m-%d")),]
  class(UAG$YearDate)
  class(UAG$Rainfall)
  # view(UAG)
  
  #Adding Year, month and day columns
  UAG$Year <- format(as.Date(UAG$YearDate), "%Y")
  UAG$Month <- format(as.Date(UAG$YearDate), "%m")
  UAG$Day<- format(as.Date(UAG$YearDate), "%d")
  UAG<-UAG[-c(2)]
  # View(UAG)
  
  #Make months columns
  UAG<- reshape(UAG, idvar=c("Year","Day"),timevar = "Month",direction="wide")
  monthnames<-month.abb
  colnames(UAG)[c(3:14)]<-monthnames
  # View(UAG)
  summary(UAG)

#Cleaning the data
  #Check for outliers (daily data)==> Check if reasonable to remove and/or replace if not leave as is
    UAG_out_daily<-UAG # Copy old dataset
    outvalue21<-list() # Create empty lists
    outindex21<-list()
    # histvalue<-list()
    for (i in 3:ncol(UAG_out_daily)) { # For every column in dataset
      # histvalue[[i]]<-hist(UAG_outliers[,i]) #Plot histogram
      outvalue21[[i]]<-boxplot.stats(UAG_out_daily[,i])$out # Get the outlier value
      outindex21[[i]]<-match(outvalue21[[i]],UAG_out_daily[,i]) # Get the outlier index
      # UAG_outliers[outindex[[i]],i] <- NA # Remove the outliers
    }
    
    outvalue21
    outindex21
    # View(UAG_outliers)
  
  #count number of times daily amounts were more than 50mm in each column
    UAGdailyover_50mm <- ldply(UAG[3:14], function(c) sum(c > 50,na.rm=T))
    UAGdailyover_50mm
    ggplot(UAGdailyover_50mm, aes(x=.id, y=V1)) +
      geom_bar(stat="identity", colour="black", fill="red") +
      xlab("Months") + ylab("Number of times daily rainfall more than 50mm")
  
  #Impute daily amount over 50mm 
    ## Use code accordingly after looking at: the rainy seasons to gauge what maximum value to give for each season
                                            # after smoothing out the maximums look at the difference in original data means and imputed data means, if not much difference use original daily data as was
    # UAG<-apply(UAG[c(3:14)], 2, function(x) ifelse(x > 50, 50, x))
    # UAG_inseason<-apply(UAG[c(5:7,13,14)], 2, function(x) ifelse(x > 50, 0.5*x, x))
    # View(UAG_nonseason)
    # View(UAG_inseason)
    # UAG<-cbind(UAG[c(1,2)],UAG_nonseason,UAG_inseason)
    # summary(UAG)

    #Return the column names containing missing observations (inspect changes from previous NA values check)
    UAGlist_na1 <- colnames(UAG)[ apply(UAG, 2, anyNA) ]
    UAGlist_na1
    
    # # Create mean==> Use code if NA values identified are same as the first check
    # UAGaverage_missing <- apply(UAG[,colnames(UAG) %in% UAGlist_na1],
    #                          2,
    #                          mean,
    #                          na.rm =  TRUE)
    # UAGaverage_missing

    # Quick code to replace missing values with the mean or 0
    UAG <- data.frame(
      sapply(
        UAG,
        function(x) ifelse(is.na(x),
                           0,
                           # mean(x, na.rm = TRUE),
                           x)))
    # View(UAG)

#Export to excel
# library("writexl")
# write_xlsx(UAG_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/UAG analysis/UAG_impute_mean.xlsx")

## Going into yearly details  
  #Group by year
  UAG_rainfall<-sapply(UAG[c(3:14)],as.numeric) # convert character columns to numeric
  UAG<-cbind(UAG[,c(1:2)],UAG_rainfall[,c(1:12)]) #combine columns removed
  # View(UAG)
  UAG_by_year<-aggregate(.~UAG$Year, UAG[c(3:14)], sum)
  # View(UAG_by_year)
  summary(UAG_by_year)
  
    ## For use in regression model=> Sum by year
      UAG_by_year_rainfall<- rowSums(UAG_by_year[, c(4:7)]) #taking 4 months from March
      # View(UAG_by_year_rainfall)
      UAG_by_year_regr<-cbind(UAG_by_year_rainfall,UAG_by_year[,1])
      UAG_by_year_regr<-as.data.frame(UAG_by_year_regr)
      colnames(UAG_by_year_regr)<-c("Rainfall","Year")
      UAG_by_year_regr$Rainfall<-as.integer(as.character(UAG_by_year_regr$Rainfall))
      UAG_by_year_regr$Year<-as.integer(as.character(UAG_by_year_regr$Year))
      str(UAG_by_year_regr)
  
  #Check for outliers (monthly)
  UAG_out_monthly<-UAG_by_year # Copy old dataset
  # View(UAG_out_monthly)
  outvalue22<-list() # Create empty lists
  outindex22<-list()
  for (i in 2:ncol(UAG_out_monthly)) { # For every column in dataset
    outvalue22[[i]]<-boxplot(UAG_out_monthly[,i])$out # Get the outlier value
    outindex22[[i]]<-match(outvalue22[[i]],UAG_out_monthly[,i]) # Get the outlier index
    # UAG_out_monthly[outindex22[[i]],i] <- 0 # Remove the outliers
  }
  
  outvalue22
  outindex22
  # View(UAG_out_monthly)

  # #Return the column names containing NA values
  # UAGlist_na22 <- colnames(UAG_out_monthly)[ apply(UAG_out_monthly, 2, anyNA) ]
  # UAGlist_na22
  # 
  # # Quick code to replace missing values with the mean
  # UAG_by_year <- data.frame(
  #   sapply(
  #     UAG_out_monthly,
  #     function(x) ifelse(is.na(x),
  #                        mean(x, na.rm = TRUE),
  #                        x)))

  # #Plotting histogram with outliers removed and gauge the difference
  # for (i in 2:ncol(UAG_by_year)) { # For every column in your dataset
  #   histvalue[[i]]<-hist(UAG_by_year[,i]) #Plot histogram
  # }
  # 
  # summary(UAG_by_year)

  #reshape data
  # UAG_by_year<-UAG_by_year[,c(1:3,9:11,4:8,12,13)] #reordering columns to follow according to month
  UAG_res <- reshape(UAG_by_year, direction="long", varying=list(2:13), v.names=c("Rainfall"), times = month.name, timevar = "Month") #Creating one month column with corresponding rainfall values
  # View(UAG_res)
  colnames(UAG_res)[1]<-"Year"
  UAG_res<-UAG_res[-c(0,4)]
  # View(UAG_res)
  
  #Creating date column
  UAG_res$Date <- as.yearmon(paste(UAG_res$Month,UAG_res$Year))
  UAG_res<-UAG_res[-c(1:2)]
  UAG_res$Date<- as.Date(UAG_res$Date, format= "%Y-%m-%d")
  class(UAG_res$Date)
  # View(UAG_res)
  UAG_data<-UAG_res
  class(UAG_data$Date)
  class(UAG_data$Rainfall)

# plot the data using ggplot
ggplot(data = UAG_data, aes(x = Date, y = Rainfall)) +
  geom_line() +
  labs(x = "Date",
       y = "Total Precipitation (mm)",
       title = "Precipitation Data",
       subtitle = "UAG Rainfall 2000 - 2014")+
  scale_x_date(limits = c(as.Date("2000-01-01","%Y-%m-%d"),as.Date("2019-01-01","%Y-%m-%d")),labels = date_format("%b-%Y"), date_breaks=("1 year"))

# #Converting dataframe To Time Series
UAG_data<-UAG_data[order(as.Date(UAG_data$Date, format="%Y-%m-%d")),]
# View(UAG_data)
UAG_ts <- ts(UAG_data$Rainfall,frequency = 12,start = c(1981,1),end=c(2020,12))

# #Selecting Data
UAG_ts <- window(UAG_ts, start=c(2000,1),end=c(2019,12))
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
