library(readxl)
library(dplyr)
library(varhandle)
library(yarrr)
library(XLConnect)


commodities <- read_excel("/Users/wemigliari/Documents/R/data/commodities_price.xlsx",
                      sheet = "commodities")
commodities <- as.data.frame(commodities)



library(extrafont)
library(RColorBrewer)

## Mineral - Iron, Colombian Coal, South African Coal and Petrol

par(family= "Arial", cex = 0.6, oma = c(4, 1, 1, 1))

plot(commodities$Month, commodities$PriceIron, type = "b", bty = "l", pch = 1, col = "steelblue",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Metric Ton/Barrel", cex = 1,
     ylim =c(0, 300))

lines(commodities$Month, commodities$PriceCoalCol, type = "b", bty = "l", pch=2, col = "darkgreen", cex = 0.5)
lines(commodities$Month, commodities$PriceCoalSA, type = "b", bty = "l", pch=3, col = "gray", cex = 0.5)
lines(commodities$Month, commodities$PriceCrudeOil, type = "b", bty = "l", pch=4, col = "black", cex = 0.5)

legend("topright", legend = c("Iron", "Colombian Coal", "South African Coal", "Crude Oil"),
       col = c("steelblue", "darkgreen", "gray", "black"), 
       pch = c(1:4),
       bty = "n", cex = 1)
abline(v=as.numeric(commodities$Month[168]), lty = 2, lwd=1, col='gray')
abline(v=as.numeric(commodities$Month[204]), lty = 2, lwd=1, col='#ff7f7f')
abline(v=as.numeric(commodities$Month[288]), lty = 1, lwd=1, col='gray')
text(x=as.numeric(commodities$Month[232]),y = 18, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[320]),y = 23, lty = 2, lwd=1, col='black', "Paris Agreement (2015)")
text(x=as.numeric(commodities$Month[320]),y = 18, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[202]),y = 242, lty = 2, lwd=1, col='#ff7f7f', "2008 Global Economic Crisis", srt = 90)


## Food - Palm Oil, Soy Bean and Hard Logs

par(family= "Arial", cex = 0.6, oma = c(4, 1, 1, 1))

plot(commodities$Month, commodities$PricePalmOil, type = "b", bty = "l", pch = 5, col = "red",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Metric Ton/Barrel", cex = 0.3,
     ylim =c(0, 1800))
lines(commodities$Month, commodities$PriceSoyBean, type = "b", bty = "l", pch=6, col = "gray", cex = 0.3)
lines(commodities$Month, commodities$PriceHardLogs, type = "b", bty = "l", pch=7, col = "steelblue", cex = 0.3)

legend("topright", legend = c("Palm Oil", "Soy Bean", "Hard Logs"),
       col = c("red", "gray", "steelblue"), 
       pch = c(5:7),
       bty = "n", cex = 1)

abline(v=as.numeric(commodities$Month[168]), lty = 2, lwd=1, col='gray')
abline(v=as.numeric(commodities$Month[204]), lty = 2, lwd=1, col='#ff7f7f')
abline(v=as.numeric(commodities$Month[288]), lty = 1, lwd=1, col='gray')
text(x=as.numeric(commodities$Month[232]),y = 6, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[320]),y = 43, lty = 2, lwd=1, col='black', "Paris Agreement (2015)")
text(x=as.numeric(commodities$Month[320]),y = 6, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[202]),y = 1400, lty = 2, lwd=1, col='#ff7f7f', "2008 Global Economic Crisis", srt = 90)

## Food -Food Index and Sugar

plot(commodities$Month, commodities$PriceFoodIndex, type = "l", bty = "l", pch = 1, col = "red",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Metric Ton/Barrel", cex = 0.3,
     ylim =c(0, 200))
lines(commodities$Month, commodities$PriceSug, type = "l", bty = "l", pch=2, col = "darkgreen", cex = 0.3)
abline(v=as.numeric(commodities$Month[288]), lwd=2, col='gray')

## Food - Beef and Poultry

plot(commodities$Month, commodities$PriceBeef, type = "b", bty = "l", pch = 9, col = "red",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Kg", cex = 0.3,
     ylim =c(0, 7))
lines(commodities$Month, commodities$PricePoultry, type = "b", bty = "l", pch=10, col = "darkgreen", cex = 0.3)
abline(v=as.numeric(commodities$Month[168]), lty = 2, lwd=1, col='gray')
abline(v=as.numeric(commodities$Month[288]), lty = 1, lwd=1, col='gray')

legend("topright", legend = c("Beef", "Poultry"),
       col = c("red", "darkgreen"), 
       pch = c(9:10),
       bty = "n", cex = 0.7)

text(2005-01-01, 1.0,  "Emissions Trade System",
     cex=0.3, pos=3, col="black") 

# Description: Commodity Food Price Index, 2005 = 100, includes Cereal, 
## Vegetable Oils, Meat, Seafood, Sugar, Bananas, and Oranges Price Indices


lines(data_bf$year, data_bf$SHAF...10, type = "b", bty = "l", pch=9, col = "steelblue", cex = 0.5)
lines(data_bf$year, data_bf$BOAS...11, type = "b", bty = "l", pch=10, col = "darkgray", cex = 0.5)
lines(data_bf$year, data_bf$TEAS...12, type = "b", bty = "l", pch=11, col = "lightblue", cex = 0.5)
lines(data_bf$year, data_bf$SEAS...13, type = "b", bty = "l", pch=12, col = "darkgreen", cex = 0.5)
lines(data_bf$year, data_bf$EQAS...14, type = "b", bty = "l", pch=13, col = "darkgray", cex = 0.5)
lines(data_bf$year, data_bf$AUST...15, type = "b", bty = "l", pch=14, col = "lightblue", cex = 0.5)
lines(data_bf$year, data_bf$Global...16, type = "b", bty = "l", pch=15, col = "darkgreen", cex = 0.5)

legend("topright", legend = c("Iron", "Aluminium", "Copper", "Lead", "Coal Colombia",
                              "Coal South Africa", "Crude Oil", "NHAF", "SHAF", "BOAS",
                              "TEAS", "SEAS", "EQAS", "AUST", "Global"),
       col = c("blue", "gray", "darkgreen", "gold", "darkgray", "lightblue",
               "gray", "orange ", "steelblue", "darkgray", "lightblue", "darkgreen",
               "gray", "yellow", "darkgreen"), 
       pch = c(1:15),
       bty = "n", cex = 0.7)

#abline(v=c(9,11), col= "gray")
#abline(v=c(21,23), col="gray")
abline(v=c(33,35), col="gray")
#abline(v=c(45,47), col="gray")
