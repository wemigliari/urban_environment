### Trees

net.bg <- sample_pa(109) 

V(net.bg)$size <- 7

V(net.bg)$frame.color <- "white"

V(net.bg)$color <- "darkgreen"

V(net.bg)$label <- ""

E(net.bg)$arrow.mode <- 0

plot(net.bg)

list <- as.list(chart1)

tr<- make_tree(109, children = 12, mode= "undirected")
plot(tr, vertex.size=10)

l <- layout_in_circle(chart2)
plot(tr, layout = l)