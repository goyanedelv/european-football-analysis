library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggrepel)
library("readxl")
library(data.table)
library(broom) 
library(psych)
library(igraph)
library(foreach)
library(doParallel)

cw <- read.xlsx('0. data/cross_walk_player_team_2.xlsx')
player <- read.xlsx('0. data/Player.xlsx')

# simplify cw
cw$year <- NULL
cw <- unique(cw)

# First we create an empty data frame
player_net <- data.frame(from = NULL, to = NULL)

# Then we get all players as a unique vector of players
uvp <- unique(cw$player_fifa_api_id)

# Start parallel computing
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

# Then, we iterate through all players in uvp and find all connections in cw
player_net <- foreach(i=1:length(uvp), .combine=rbind) %dopar% {
     # find teams where player i played 
     teams_i <- cw$home_team_api_id[cw$player_fifa_api_id == uvp[i]]

     # find other players that also played in that team
     players_in_teams_i <- cw$player_fifa_api_id[cw$home_team_api_id %in% teams_i]

     # create a new dataset and append to existing player net
     aux <- data.frame(from = rep(uvp[i], length(players_in_teams_i)), to = players_in_teams_i)
     aux

}
stopCluster(cl)

player_net <- unique(player_net) # kill duplicates

# Pass fom id's to names
player_net_names <- merge(player_net, player[,c('player_api_id', 'player_name')], by.x = 'from', by.y = 'player_api_id')
player_net_names <- merge(player_net_names, player[,c('player_api_id', 'player_name')], by.x = 'to', by.y = 'player_api_id')

player_net_names$to <- NULL
player_net_names$from <- NULL

colnames(player_net_names) <- c('from', 'to')

# kill autoloops
player_net_names$autoloop <- player_net_names$from == player_net_names$to
player_net_names<- player_net_names[player_net_names$autoloop == FALSE,]
player_net_names$autoloop <- NULL

head(player_net_names)

# Theare are 1 M connections! (1,099,476)
nrow(player_net_names)
sampled_player_net_names <-  sample_n(player_net_names, 5000)

# as matrix
matrix_plot = as.matrix(sampled_player_net_names)

#Create the graph object
player_network_plot = graph.edgelist(matrix_plot,directed=FALSE)

plot(player_network_plot, edge.arrow.size = .4, vertex.label = NA, vertex.size = 1,
     edge.width = .1)

# Creating an object will ALL connections (Manu: DO NOT PLOT)
# as matrix
matrix = as.matrix(player_net_names)

#Create the graph object
player_network = graph.edgelist(matrix,directed=FALSE)

#Print degree
sort(degree(player_network),decreasing=T)[1:20] # top 20
sort(betweenness(player_network),decreasing=T)[1:20]  # top 20

# All Shortest paths
all_shortest_paths(player_network, from="Radamel Falcao", to="Alexis Sanchez")

# 1 shortest path (through Danilo)
shortest_paths(player_network, from="Radamel Falcao", to="Alexis Sanchez")$vpath[[1]]

# pick one
PtoA <- get.shortest.paths(player_network, from="Radamel Falcao", to="Alexis Sanchez")

