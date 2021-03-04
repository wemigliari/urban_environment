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
text(x=as.numeric(commodities$Month[202]),y = 1600, lty = 2, lwd=1, col='#ff7f7f', "2008 Global Economic Crisis", srt = 90)

## Food - Food Index and Sugar

par(family= "Arial", cex = 0.6, oma = c(4, 1, 1, 1))

plot(commodities$Month, commodities$PriceFoodIndex, type = "b", bty = "l", pch = 8, col = "red",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Metric Ton/Barrel", cex = 0.3,
     ylim =c(0, 200))
lines(commodities$Month, commodities$PriceSug, type = "b", bty = "l", pch = 9, col = "darkgreen", cex = 0.3)

legend("topright", legend = c("Food Index", "Sugar"),
       col = c("red", "darkgreen"), 
       pch = c(8:9),
       bty = "n", cex = 1)

abline(v=as.numeric(commodities$Month[168]), lty = 2, lwd=1, col='gray')
abline(v=as.numeric(commodities$Month[204]), lty = 2, lwd=1, col='#ff7f7f')
abline(v=as.numeric(commodities$Month[288]), lty = 1, lwd=1, col='gray')
text(x=as.numeric(commodities$Month[232]),y = 6, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[320]),y = 10, lty = 2, lwd=1, col='black', "Paris Agreement (2015)")
text(x=as.numeric(commodities$Month[320]),y = 6, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[202]),y = 160, lty = 2, lwd=1, col='#ff7f7f', "2008 Global Economic Crisis", srt = 90)

## Food - Beef and Poultry

par(family= "Arial", cex = 0.6, oma = c(4, 1, 1, 1))

plot(commodities$Month, commodities$PriceBeef, type = "b", bty = "l", pch = 10, col = "red",
     main = "Commodity Prices in the International Market", 
     xlab = "Source: World Bank. Elaborated by Migliari, W. (2021).", 
     ylab = "US Dollars per Kg", cex = 0.3,
     ylim =c(0, 7))
lines(commodities$Month, commodities$PricePoultry, type = "b", bty = "l", pch=11, col = "darkgreen", cex = 0.3)

legend("topright", legend = c("Beef", "Poultry"),
       col = c("red", "darkgreen"), 
       pch = c(8:9),
       bty = "n", cex = 1)

abline(v=as.numeric(commodities$Month[168]), lty = 2, lwd=1, col='gray')
abline(v=as.numeric(commodities$Month[204]), lty = 2, lwd=1, col='#ff7f7f')
abline(v=as.numeric(commodities$Month[288]), lty = 1, lwd=1, col='gray')
text(x=as.numeric(commodities$Month[232]),y = 0.5, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[320]),y = 0.66, lty = 2, lwd=1, col='black', "Paris Agreement (2015)")
text(x=as.numeric(commodities$Month[320]),y = 0.5, lty = 2, lwd=1, col='black', "Emissions Trade System (2005)")
text(x=as.numeric(commodities$Month[202]),y = 5, lty = 2, lwd=1, col='#ff7f7f', "2008 Global Economic Crisis", srt = 90)

# Description: Commodity Food Price Index, 2005 = 100, includes Cereal, 
## Vegetable Oils, Meat, Seafood, Sugar, Bananas, and Oranges Price Indices
