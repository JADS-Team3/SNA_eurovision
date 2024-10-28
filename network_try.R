library(igraph)

# Load the node list
nodelist <- read.csv("data/nodelist.csv", stringsAsFactors = FALSE)

# Load the edge list
edgelist <- read.csv("data/edgelist.csv", stringsAsFactors = FALSE)

# Create the graph
network <- graph_from_data_frame(d = edgelist, vertices = nodelist, directed = TRUE)

print(network)
summary(network)

plot(network, vertex.label = V(network)$country_name, edge.arrow.size = 0.5)