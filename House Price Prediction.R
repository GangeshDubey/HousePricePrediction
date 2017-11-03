############################################################
#  House Price Prediction - A Kaggle competition           #  
############################################################

############################################################
# 1. load required packages                                #  
############################################################

library('dplyr') # data manipulation
library('ggplot2') # Graphs and Data Visualization

############################################################
# 2. read csv files, combine training and testing data     #  
############################################################

setwd("E://Gangesh//01 Working//01 Working- K")
test.inData <- read.csv("test_house_prices.csv",stringsAsFactors = FALSE)
train.inData <- read.csv("train_house_prices.csv",stringsAsFactors = FALSE)
raw.combined.df <- bind_rows(train.inData,test.inData)

View(test.inData)
View(train.inData)
View(raw.combined.df)

colnames(raw.combined.df)
cbind(names(raw.combined.df))
############################################################
# 3. Transorm discrete variables to factors                #  
############################################################

raw.combined.df$MSSubClass <- as.factor(raw.combined.df$MSSubClass)
raw.combined.df$MSZoning <- as.factor(raw.combined.df$MSZoning)
raw.combined.df$Street <- as.factor(raw.combined.df$Street)
raw.combined.df$Alley <- as.factor(raw.combined.df$Alley)
raw.combined.df$LotShape <- as.factor(raw.combined.df$LotShape)
raw.combined.df$LandContour <- as.factor(raw.combined.df$LandContour)
raw.combined.df$Utilities <- as.factor(raw.combined.df$Utilities)
raw.combined.df$LotConfig <- as.factor(raw.combined.df$LotConfig)
raw.combined.df$LandSlope <- as.factor(raw.combined.df$LandSlope)
raw.combined.df$Neighborhood <- as.factor(raw.combined.df$Neighborhood)
raw.combined.df$Condition1 <- as.factor(raw.combined.df$Condition1)
raw.combined.df$Condition2 <- as.factor(raw.combined.df$Condition2)
raw.combined.df$BldgType <- as.factor(raw.combined.df$BldgType)
raw.combined.df$HouseStyle <- as.factor(raw.combined.df$HouseStyle)
raw.combined.df$OverallQual <- as.factor(raw.combined.df$OverallQual)
raw.combined.df$OverallCond <- as.factor(raw.combined.df$OverallCond)
raw.combined.df$RoofStyle <- as.factor(raw.combined.df$RoofStyle)
raw.combined.df$RoofMatl <- as.factor(raw.combined.df$RoofMatl)
raw.combined.df$Exterior1st <- as.factor(raw.combined.df$Exterior1st)
raw.combined.df$Exterior2nd <- as.factor(raw.combined.df$Exterior2nd)
raw.combined.df$MasVnrType <- as.factor(raw.combined.df$MasVnrType)
raw.combined.df$ExterQual <- as.factor(raw.combined.df$ExterQual)
raw.combined.df$ExterCond <- as.factor(raw.combined.df$ExterCond)
raw.combined.df$BsmtQual <- as.factor(raw.combined.df$BsmtQual)
raw.combined.df$BsmtCond <- as.factor(raw.combined.df$BsmtCond)
raw.combined.df$BsmtExposure <- as.factor(raw.combined.df$BsmtExposure)
raw.combined.df$BsmtFinType1 <- as.factor(raw.combined.df$BsmtFinType1)
raw.combined.df$BsmtFinType2 <- as.factor(raw.combined.df$BsmtFinType2)
raw.combined.df$Heating <- as.factor(raw.combined.df$Heating)
raw.combined.df$HeatingQC <- as.factor(raw.combined.df$HeatingQC)
raw.combined.df$CentralAir <- as.factor(raw.combined.df$CentralAir)
raw.combined.df$Electrical <- as.factor(raw.combined.df$Electrical)
raw.combined.df$KitchenQual <- as.factor(raw.combined.df$KitchenQual)
raw.combined.df$Functional <- as.factor(raw.combined.df$Functional)
raw.combined.df$FireplaceQu <- as.factor(raw.combined.df$FireplaceQu)
raw.combined.df$GarageType <- as.factor(raw.combined.df$GarageType)
raw.combined.df$GarageFinish <- as.factor(raw.combined.df$GarageFinish)
raw.combined.df$GarageQual <- as.factor(raw.combined.df$GarageQual)
raw.combined.df$GarageCond <- as.factor(raw.combined.df$GarageCond)
raw.combined.df$PavedDrive <- as.factor(raw.combined.df$PavedDrive)
raw.combined.df$PoolQC <- as.factor(raw.combined.df$PoolQC)
raw.combined.df$Fence <- as.factor(raw.combined.df$Fence)
raw.combined.df$SaleType <- as.factor(raw.combined.df$SaleType)
raw.combined.df$SaleCondition <- as.factor(raw.combined.df$SaleCondition)

############################################################
# 4. Data Preparation                                      #
############################################################

############################################################
# A summary of the dataset, inspect for missing values     #
############################################################
summary(raw.combined.df)
sum(is.na(raw.combined.df))


dim(raw.combined.df)
missing.data.table <- data.frame(Column_Name =c(1:81),Missing_Values=c(1:81))
total.missing.values <- 0
for (col_nums in c(1:81))
{ 
  missing.data.table[col_nums,1] <- colnames(raw.combined.df[col_nums])
  missing.data.table[col_nums,2] <- sum(is.na(raw.combined.df[col_nums]))
  total.missing.values = total.missing.values + sum(is.na(raw.combined.df[col_nums]))
}

View(missing.data.table[with(missing.data.table,order(-Missing_Values)),])

paste("Total missing values : ", total.missing.values)

############################################################
# Let us inspect each of the variables  in detail          #
############################################################

# MSSubClass
which(is.na(raw.combined.df$MSSubClass))
levels(raw.combined.df$MSSubClass) #Looks all clean

# MSZoning
which(is.na(raw.combined.df$MSZoning)) # Missing values for rows 1916,2217,2251,2905
levels(raw.combined.df$MSZoning) # Missing levels (A,C,I,RP) & "C (all)" is incorrectly populated 

# LotFrontage
which(is.na(raw.combined.df$LotFrontage)) # Total 486 missing values
summary(raw.combined.df$LotFrontage)

# LotArea
which(is.na(raw.combined.df$LotArea))#Looks all clean
sum(is.na(raw.combined.df$LotArea))

# Street
which(is.na(raw.combined.df$Street))#Looks all clean
sum(is.na(raw.combined.df$Street))

############################################################
# 5. Exploratory Data Analysis                             #
############################################################

ggplot(data = raw.combined.df, aes(x=LotArea/1000,y=SalePrice/1000, label = SalePrice) ) +geom_point() + 
  scale_x_continuous(limits = c(0, 25)) + 
  scale_y_continuous(limits = c(0, 400))# We see an upward trend, this shows that generally the prices go up with Lot Area

ggplot(data = raw.combined.df, aes(x=MSSubClass,y=SalePrice/1000))+ geom_boxplot()
ggplot(data = raw.combined.df, aes(x=SalePrice))+ geom_histogram(binwidth = 1000)
ggplot(data=raw.combined.df) + facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x))

lm1<- lm(raw.combined.df$SalePrice ~ raw.combined.df$LotArea)
lm1

summary(lm1)

packageStatus()
############################################################
# 99. Save data in a RData file                            #  
############################################################

save(list = ls(all.names = TRUE), file = "house_pricing.RData", envir = .GlobalEnv)
