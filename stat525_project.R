house_data <- read.csv(file = "/Users/haogao/study/stat525/project/Housing Data.csv", header=TRUE, sep = ",")
dim(house_data)

data <- data.frame(house_data$Lot.Frontage, house_data$Lot.Area, house_data$Year.Built, house_data$Year.Remod.Add, 
                   house_data$Mas.Vnr.Area, house_data$BsmtFin.SF.1, house_data$BsmtFin.SF.2, house_data$Total.Bsmt.SF, house_data$Bsmt.Full.Bath, 
                   house_data$Bsmt.Half.Bath, house_data$Full.Bath, house_data$Half.Bath, house_data$Bedroom.AbvGr, 
                   house_data$Kitchen.AbvGr, house_data$TotRms.AbvGrd, house_data$Fireplaces, house_data$Garage.Yr.Blt,
                   house_data$Garage.Cars, house_data$Garage.Area, house_data$Wood.Deck.SF, house_data$Open.Porch.SF,
                   house_data$Enclosed.Porch, house_data$X3Ssn.Porch, house_data$Screen.Porch, house_data$Pool.Area, 
                   house_data$Misc.Val, house_data$Mo.Sold, house_data$Yr.Sold, house_data$SalePrice)
dim(data)

names(data) <- c("LotFrontage", "LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "BsmFinSF2", 
                 "TotalBsmtSF","BsmtFullBath", "BsmtHalfBath","FullBath", "HalfBath", "Bedroom", "Kitchen", "TotRmsAbvGrd",
                 "Fireplaces", "GarageYrBlt", "GarageCars", 
                 "GarageArea", "WoodDeckSf", "OpenPorchSF", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "Miscval"
                 , "Mosold", "YrSold", "Price")

data = data[complete.cases(data), ]

reg1 = lm(Price ~ ., data = data)
summary(reg1)
anova(reg1)["Residuals","Mean Sq"]
library(car)

residuals = resid(reg1)
length(data$Price)
length(residuals)
plot(data$LotFrontage, residuals)
plot(data$LotArea, residuals)


SS2 = Anova(reg1, type = 2)
SS2


data2 <- data[,c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "TotalBsmtSF", "BsmtFullBath", "FullBath", 
                 "HalfBath", "Bedroom", "Kitchen", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "GarageArea", "WoodDeckSf", "EnclosedPorch", 
                 "ScreenPorch", "Miscval", "Price")]
dim(data2)

reg2 = lm(Price ~ ., data = data2)
anova(reg2)["Residuals","Mean Sq"]

write.csv(summary(reg2, corr = T)$correlation, file = "/Users/haogao/study/stat525/project/data/cor_matrix.csv")

data_centered = data.frame(matrix(ncol = 0, nrow = 2274))
for (i in 1:19){
  data_centered[,i] = data2[,i] - mean(data2[,i], na.rm = TRUE)
}

names(data_centered) = c("LotArea", "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "TotalBsmtSF", "BsmtFullBath", "FullBath", 
"HalfBath", "Bedroom", "Kitchen", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "GarageArea", "WoodDeckSf", 
"EnclosedPorch", "ScreenPorch", "Miscval")

data_centered[, "Price"] = data2[, "Price"]
reg_c = lm(Price ~ .^2, data = data_centered)

anova_c = Anova(reg_c, type = 2)
data_with_interaction = data_centered
for(i in 1:18){
  helper = 0
  for(k in 1:i){
    helper = helper + 19-(k-1)
  }
  for(j in (i+1):19){
    if(anova_c[helper + j-i, 4] <= 0.01){
      data_with_interaction[, rownames(anova_c)[helper + j-i]] = data_centered[,i]*data_centered[,j]
    }
  }
}
dim(data_with_interaction)
reg_final = lm(Price~., data = data_with_interaction)
summary(reg_final)
Anova(reg_final, type =2)





