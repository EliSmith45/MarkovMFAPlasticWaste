library(tidyverse)
library(igraph)
library(Matrix)
library(stringr)

mat <- read_csv("Data/network.csv") 
rownames(mat) <- mat$X1
mat <- select(mat, -X1) %>% as.matrix()

graph <- graph_from_adjacency_matrix(mat, mode = "directed")
V(graph)$color[substr(V(graph)$name, 1, 2) == "G1"] <- "#AEFFBB"
#V(graph)$color[substr(V(graph)$name, 4, 4) == "j" | substr(V(graph)$name, 3, 3) == "j"] <- "#FFE57E"
V(graph)$color[V(graph)$name %in% c("Mismanaged", "Managed")] <- "#FF9490"
colors <- V(graph)$color
l <- layout_with_gem(graph)
plot(graph, 
     vertex.color = colors, 
     vertex.size = 30, 
     edge.curved = .3,
     edge.width = 2.5, 
     edge.arrow.size = 1, 
     layout = l)

