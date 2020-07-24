library(plotly)
library(tidyr)
library(readxl)

options(digits = 3)
carbon <- read_excel("/Users/wemigliari/Documents/R/tabelas/carbon_emissions.xlsx", 
                     col_types = c("text", "text", "text", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric"))

carbon[,2:4] <- NULL


plot_ly(carbon, x = ~`Country Name`, y = carbon$`2007`, color = I("#071317"), name = "2007", 
        type = "scatter", mode = "markers")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2008`, color = I("#194553"), name = "2008")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2009`, color = I("#2c778f"), name = "2009")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2010`, color = I("#44a6c6"), name = "2010")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2011`, color = I("#71bbd4"), name = "2011")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2012`, color = I("#bcdfeb"), name = "2012")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2013`, color = I("#c6cbcd"), name = "2013")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2014`, color = I("darkgray"), name = "2014")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2015`, color = I("green"), name = "2015")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2015`, color = I("yellow"), name = "2016")%>%
  layout(margin = list(b=10, l=10),
         xaxis = list(tickangle=90, title = "", tickfont = list(size = 7), titlefont=list(size=7)),
         yaxis = list(title = "", titlefont=list(size=7)))


plot_ly(carbon, x = ~`Country Name`, y = carbon$`2007`, color = I("#071317"), name = "2007", 
        type = "scatter", mode = "markers", orientation = 'h')%>%
  add_trace(x = ~`Country Name`, y = carbon$`2008`, color = I("#194553"), name = "2008")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2009`, color = I("#2c778f"), name = "2009")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2010`, color = I("#44a6c6"), name = "2010")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2011`, color = I("#71bbd4"), name = "2011")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2012`, color = I("#bcdfeb"), name = "2012")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2013`, color = I("#c6cbcd"), name = "2013")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2014`, color = I("darkgray"), name = "2014")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2015`, color = I("green"), name = "2015")%>%
  add_trace(x = ~`Country Name`, y = carbon$`2015`, color = I("yellow"), name = "2016")%>%
  layout(margin = list(b=10, l=10),
         xaxis = list(tickangle=90, title = "", tickfont = list(size = 7), titlefont=list(size=7)),
         yaxis = list(title = "", titlefont=list(size=7)))

####

library(GGally)
carbon1 <- GGally::ggpairs(carbon, cardinality_threshold = 264)
ggplotly(carbon1)
  
  

