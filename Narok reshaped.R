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
# install.packages("rstudioapi")

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
library(rstudioapi)

options(stringsAsFactors = FALSE)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

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

## Reshape data for use in Excel
res <- reshape(narok, idvar=c("Year","Date"), direction="long", varying=list(3:14), v.names=c("Rainfall"), times = month.name, timevar = "Month") #Creating one month column with corresponding rainfall values
res <- reshape(res, idvar=c("Month","Date"),timevar = "Year", direction="wide")
values<-c(1960:2014)
colnames(res)[c(3:57)]<-values
res$Year<-2020
res<-res[c(58,1:57)]
View(res)

#Export to excel
write_xlsx(res,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/data/Narok_model_data.xlsx")