# MAIZE YIELD DATA ANALYSIS
# install.packages("fitdistrplus")
#Import data
  maize_data <- read.csv("/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/FAOSTAT_data_8-22-2020.csv")
  # View(maize_data)
  summary(maize_data)

#Cleaning the data
  ##Remove columns not needed
    maize_data<-maize_data[-c(1:5,7:9,11,13,14)]
    # View(maize_data)
  
  ##Reshape data
    maize_data_res <- reshape(maize_data,idvar="Year",timevar = "Element",direction="wide")
    # View(maize_data_res)
  
  ##Check for NAs
    list_na3 <- colnames(maize_data_res)[ apply(maize_data_res, 2, anyNA) ]
    list_na3
  
  ##Check for Outliers
    maize_outliers<-maize_data_res # Copy old dataset
    outvalue31<-list() # Create empty lists
    outindex31<-list()
    # histvalue<-list()
    for (i in 2:ncol(maize_outliers)) { # For every column in dataset
      # histvalue[[i]]<-hist(UAG_outliers[,i]) #Plot histogram
      outvalue31[[i]]<-boxplot(maize_outliers[,i])$out # Get the outlier value
      outindex31[[i]]<-match(outvalue31[[i]],maize_outliers[,i]) # Get the outlier index
      # maize_outliers[outindex31[[i]],i] <- NA # Remove the outliers
    }
    
    outvalue31
    outindex31
    # View(maize_outliers)

  # Quick code to replace missing values with the mean==> Use code if NAs exist
    maize_data_imputedmean <- data.frame(
      sapply(
        maize_data_res,#maize_outliers,
        function(x) ifelse(is.na(x),
                           mean(x, na.rm = TRUE),
                           x)))
    
    colnames(maize_data_imputedmean)<-c("Year","Areaharvested","Yield","Production") #Rename columns
    # view(maize_data_imputedmean)
    summary(maize_data_imputedmean)

# Simple plot
  ggplot(maize_data_imputedmean, aes(x=Year)) + 
    # geom_line(aes(y =Areaharvested), color = "darkred") #+ 
    geom_line(aes(y =Yield), color="steelblue", linetype="twodash") #+
    # geom_line(aes(y =Production), color="green")

# Create combined yield rainfall df
  #Check structure of data
    str(UAG_by_year_regr)
    str(narok_by_year_regr)
    str(maize_data_imputedmean)
  
  #Combine dataframes by id=year
    yield_UAGrainfall <- merge(maize_data_imputedmean,UAG_by_year_regr,by="Year")
    yield_rainfall <- merge(yield_UAGrainfall,narok_by_year_regr,by="Year")
    colnames(yield_rainfall)[5:6]<-c("UAG_rainfall","Narok_rainfall")
    str(yield_rainfall)
    rownames(yield_rainfall)<-yield_rainfall$Year
    yield_rainfall<-yield_rainfall[-c(1)]
    View(yield_rainfall)

## Check for multicollinearity
    plot(yield_rainfall)

## Normalize/ Standardize data
    # heatmaply(
    #   yield_rainfall, 
    #   xlab = "Annual rainfal(mm)",
    #   ylab = "Year", 
    #   main = "Annual Yield/Rainfall plot"
    # )
    
    standardized_data<-as.data.frame(scale(yield_rainfall))
    normalized_data<-as.data.frame(normalize(yield_rainfall))
    
    str(standardized_data)
    str(normalized_data)
    
    View(standardized_data)
    View(normalized_data)
    
    # heatmaply(
    #   scale(yield_rainfall), 
    #   xlab = "Annual rainfal(mm)",
    #   ylab = "Year", 
    #   main = "Annual Yield/Rainfall plot"
    # )
   
# Fitting a linear model
    model <- lm(Production~ UAG_rainfall + Narok_rainfall, data = yield_rainfall)
    ols_coll_diag(model)
    summary(model)
    
# Fitting a model
    library(fitdistrplus)
    descdist(yield_rainfall$Production)
    descdist(yield_rainfall$Yield) 
    descdist(yield_rainfall$Areaharvested) 
    descdist(yield_rainfall$UAG_rainfall)
    descdist(yield_rainfall$Narok_rainfall)
    
    ## more checks
    hist(yield_rainfall$Production, # histogram
         col="gray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies
         xlab = "Production",
         main = "Annual Production")
    lines(density(yield_rainfall$Production), # density plot
          lwd = 2, # thickness of line
          col = "chocolate3")
    
    # Fit normal distribution
    modelgauss<-glm(Production~ UAG_rainfall + Narok_rainfall, family = gaussian(link = identity),  data = yield_rainfall)
    summary(modelgauss)
    summary(residuals(modelgauss))
    
    # ols_plot_resid_fit_spread(model)
    # ols_correlations(model)


  
        

#     aov_fun<-aov(yield_rainfall$Yield~yield_rainfall$Rainfall) 
#     summary(aov_fun)
#     
#     
#     
#     
#     anova(modelquasi,modelquasipois,modelpois,modelgauss,modelgamma,modelinverse)
#     anova(modelquasipois)
#     anova(modelpois)
#     anova(modelgauss)
#     anova(modelgamma)
#     anova(modelinverse)
#     
# linear_model <- lm(Yield ~ Annualrainfall, data = Data)
# summary(model)
# model1 <- glm(Yield ~ Annualrainfall, family = poisson(link = log),  data = Data)
# summary(model1)
#  # plot(maize_data_imputedmean)
# 
# #Export to excel
# # library("writexl")
# # write_xlsx(narok_impute_mean,"/Users/ndubaijacklinemwendwa/Desktop/twiga data/narok analysis/narok_impute_mean.xlsx")
# 
# 
# 
# 
# 
# 
# 