remove(list = ls())

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

## UASIN GISHU DATA
#Import Data
UAG <- read.csv('~/Desktop/twiga data/narok analysis/Uasin Gishu rainfall 1981-2020.csv')
# view(UAG)
summary(UAG)

#Return the column names containing missing observations (before reshaping)
UAGlist_na <- colnames(UAG)[ apply(UAG, 2, anyNA) ]
UAGlist_na

# Create mean
uag_average_missing <- apply(UAG[,colnames(UAG) %in% UAGlist_na],
                         2,
                         mean,
                         na.rm =  TRUE)
uag_average_missing

# Quick code to replace missing values with the mean
UAG <- data.frame(
  sapply(
    UAG,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))

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
View(UAG)

## Reshape data for use in Excel
res1 <- reshape(UAG, idvar=c("Month","Day"),timevar = "Year", direction="wide")
values1<-c(1981:2020)
View(res1)
colnames(res1)[c(3:42)]<-values1
res1$Year<-2020
res1<-res1[c(43,1:42)]
res1<-sapply(res1[c(1:43)],as.numeric)
res1<-as.data.frame(res1)
str(res1)
View(res1)

#Export to excel
write_xlsx(res1,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/data/UAG_model_data.xlsx")