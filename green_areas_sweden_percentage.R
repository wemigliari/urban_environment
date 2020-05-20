
library(readxl)
library(yarrr)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(writexl)
library(dplyr)
library(varhandle)
library(xlsx)


green_table_05 <- unfactor(read_excel("/Users/wemigliari/Documents/R/tabelas/green_areas_2005_percentage.xlsx"))
green_table_05a <- data.frame(green_table_05)
### If you still have % sign, use: green_table_05b <- data.frame(lapply(green_table_05a, function(x) as.numeric(sub("%", "", x)))) ### Percentage is read as character 

green_areas_05 <-cor(green_table_05a[,c(3:5, 6:7)])

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(green_areas_05)

corrplot(green_areas_05, type="upper", order="hclust",
         col=brewer.pal(n=6, name="Blues"), mar=c(1,1,1,1))

plot(green_table_05$DWV, green_table_05$NDWV, pch = 15,
     lty = 3, frame = FALSE,
     col = "#78736c",
     xlab = "(%)",
     ylab = "(%)",
     xlim = c(0,80), ylim = c(0,80))
points(green_table_05$DVORSOME, green_table_05$NDVORSOME, pch = 16, col = "#c8c1b4")
points(green_table_05$SUM...6, green_table_05$SUM...9, pch = 17, col = "#000000")
legend(50,80, 
       legend = c("Corr. Developed and Non-Developed Plots, No Vegetation (%)",
                  "Corr. Developed and Non-Developed Plots, With Vegetation (%)",
                  "Corr. of Sums Total Areas (%)"), 
       col = c("#78736c", "#c8c1b4", "#000000"), cex = 0.6, box.lty = 0,
       pch = c(15, 16, 17))



sort(green_table_05$DWV, decreasing = TRUE)
pairs(green_table_05a [, c(3:5, 6:7)])


