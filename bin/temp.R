# ggnet2.R
library(GGally)

# random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)
# vertex names
network.vertex.names(net) = letters[1:10]

ggnet2(net)

ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")
ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")
ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))

bip = data.frame(event1 = c(1, 2, 1, 0),
                 event2 = c(0, 0, 3, 0),
                 event3 = c(1, 1, 0, 4),
                 row.names = letters[1:4])
# weighted bipartite network
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

ggnet2(bip, label = TRUE)

# set colors for each mode
col = c("actor" = "grey", "event" = "gold")
# detect and color the mode
ggnet2(bip, color = "mode", palette = col, label = TRUE)



###################
library(network)

#Draw a random matrix
(m<-matrix(rbinom(25,1,0.5),5))
diag(m)<-0

#Coerce to network form
g<-as.network.matrix(m,matrix.type="adjacency")

# edge list example. Only 4 nodes in the edge list.
m = matrix(c(1,2, 2,3, 3,4), byrow = TRUE, nrow=3)
attr(m, 'n') = 7
m <- as.network(m, matrix.type='edgelist')

ggnet2(m)







