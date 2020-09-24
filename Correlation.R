# MAIZE YIELD DATA ANALYSIS
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
    # 
    # heatmaply(
    #   scale(yield_rainfall), 
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
    
    # Fitting a model
    model <- lm(Yield~ UAG_rainfall + Narok_rainfall, data = yield_rainfall)
    ols_coll_diag(model)
    summary(model)
    
    # Fitting a model
    model <- lm(Production~ UAG_rainfall + Narok_rainfall, data = standardized_data)
    ols_coll_diag(model)
    summary(model)
    # ols_plot_resid_fit_spread(model)
    # ols_correlations(model)

    install.packages("fitdistrplus")
    descdist(yield_rainfall$Yield)    
    modelquasipois<-glm(Areaharvested~ UAG_rainfall + Narok_rainfall, family = quasipoisson(link = log),  data = normalized_data)
        summary(modelquasipois)
        modelpois<-glm(Areaharvested~ UAG_rainfall + Narok_rainfall, family = poisson(link = log),  data = normalized_data)
        summary(modelpois)
        modelgauss<-glm(Yield~ UAG_rainfall + Narok_rainfall, family = gaussian(link = identity),  data = standardized_data)
        summary(modelgauss)
        modelgamma<-glm(Areaharvested~ UAG_rainfall + Narok_rainfall, family = Gamma(link = inverse),  data = normalized_data)
        summary(modelgamma)
        modelinverse<-glm(Areaharvested~ UAG_rainfall + Narok_rainfall, family = inverse.gaussian(link = 1/mu^2),  data = normalized_data)
        summary(modelinverse)
        modelquasi<-glm(Yield ~ Rainfall, family = quasi(link = identity,variance = constant),  data = yield_rainfall)
        summary(modelquasi)

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