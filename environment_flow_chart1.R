library(readxl)
library(igraph)
library(writexl)
library(RColorBrewer)

flow_chart1 <- read_excel("/Users/wemigliari/Library/Mobile Documents/com~apple~CloudDocs/Pós-Doutorado & Doutorado/Project 2020/environmental_flow_chart1.xlsx",
                          sheet = "data")

chart1 <- data.frame(flow_chart1)
chart2 <- graph_from_data_frame(chart1)

l <- layout_in_circle(chart2)

colrs <- adjustcolor(c("#44e544", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "#262d0b", "darkgreen", "darkgreen", "#262d0b"), alpha=.6)
kc <- coreness(chart2, mode="all")


set.seed(4321)


plot.igraph(chart2, edge.arrow.size=0.1, edge.curved=0, edge.color = c("#44e544"), 
     vertex.color=colrs, vertex.frame.color= c("#262d0b"),
     vertex.label=V(chart2)$source, vertex.label.color=c("#555555"),
     vertex.label.cex=.7, vertex.label.dist=2.5,  margin=0,
     vertex.size = 15, layout = l)


#####

flow_chart2 <- read_excel("/Users/wemigliari/Library/Mobile Documents/com~apple~CloudDocs/Pós-Doutorado & Doutorado/Project 2020/environmental_flow_chart1.xlsx",
                          sheet = "positive")

chart22 <- data.frame(flow_chart2)
chart222 <- graph_from_data_frame(chart22)

l2 <- layout_as_tree(chart222)

colrs <- adjustcolor( c("#44e544", "#262d0b", "#262d0b", "#262d0b"), alpha=.6)
kc <- coreness(chart222, mode="all")


set.seed(4321)
plot.igraph(chart222, edge.arrow.size=0.2, edge.curved=0, 
            vertex.color=colrs, vertex.frame.color="#555555",
            vertex.label=V(chart222)$source, vertex.label.color="black",
            vertex.label.cex=.7, vertex.label.dist=2.5,  margin=0, vertex.size = 10)


###
net.bg <- sample_pa(109) 

V(net.bg)$size <- 7

V(net.bg)$frame.color <- "white"

V(net.bg)$color <- "darkgreen"

V(net.bg)$label <- ""

E(net.bg)$arrow.mode <- 0

list <- as.list(chart1)

tr<- make_tree(109, children = 12, mode= "undirected")
plot(tr, vertex.size=10)

plot(tr, layout = l)


####

ceb <- cluster_edge_betweenness(chart2) 

dendPlot(ceb, mode="hclust", cex = 0.5)

###

plot(ceb, chart2, cex = 0.5) 



