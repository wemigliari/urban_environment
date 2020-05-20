
library(readxl)
library(yarrr)
library(corrplot)
library(RColorBrewer)
library(stringr)
library(writexl)
library(dplyr)
library(varhandle)
library(xlsx)

## Table 1

ga2005 <- read_excel("/Users/wemigliari/Documents/R/tabelas/green_areas_2005_numbers.xlsx")

# Eliminating white spaces

ga2005a <- apply(ga2005,2,function(x)gsub('\\s+', '',x)) ## Result is a matrix
gt5 <- data.frame(apply(ga2005a,2,function(x)gsub('\\s+', '',x))) ## Converting into a data.frame

### Using the package "varhandle" to assigne factors as characters and numbers
gt05 <- unfactor(gt5)

### Save the table as Excel file in your directory

write_xlsx(x = gt05, path = "/Users/wemigliari/Documents/R/tabelas/green_areas_2005_numbers.xlsx", col_names = TRUE)


#####

green_table_05 <- unfactor(read_excel("/Users/wemigliari/Documents/R/tabelas/green_areas_2005_numbers.xlsx", sheet =
                                        "green_areas"))

green_table_05a <- data.frame(green_table_05)

green_areas_05 <-cor(green_table_05a [, c(3:5, 7:8)])

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(green_areas_05)

corrplot(green_areas_05, type="upper", order="hclust",
         col=brewer.pal(n=6, name="Blues"), mar=c(1,1,1,1))

plot(green_table_05$DWV, green_table_05$NDWV, pch = 15,
     lty = 3, frame = FALSE,
     col = "#78736c",
     xlab = "(ha)",
     ylab = "(ha)")
points(green_table_05$DVORSOME, green_table_05$NDVORSOME, pch = 16, col = "#c8c1b4")
points(green_table_05$SUM...6, green_table_05$SUM...9, pch = 17, col = "#000000")
legend(1800, 400, 
       legend = c("Corr. Developed and Non-Developed Plots, No Vegetation (ha)",
                  "Corr. Developed and Non-Developed Plots, With Vegetation (ha)",
                  "Corr. of Sums Total Areas (ha)"), 
       col = c("#78736c", "#c8c1b4", "#000000"), cex = 0.6, box.lty = 0,
       pch = c(15, 16, 17))



pairs(green_table_05a [, c(3:5, 7:8)], col = "blue")


















