rm(list=ls())

library(openxlsx)
library(dplyr)
library(distances)
library(RColorBrewer)
library(fastDummies)
library(ggplot2)
library(ggrepel)


team_atts <- read.xlsx('0. data/Team_Attributes.xlsx')

teams <- read.xlsx('0. data/Team.xlsx')
teams$team_api_id <- NULL

ligas <- c('England', 'Spain', 'Germany', 'France', 'Italy')
# ligas <- c('Spain', 'Germany')

teams <- subset(teams, teams$country %in% ligas)

teams <- merge(teams, team_atts, by = 'team_fifa_api_id')

teams$year <- as.numeric(substring(as.Date(teams$date , "%Y-%m-%d"),1,4))

teams_2015 <- subset(teams, teams$year == 2015)

numvars <- c('buildUpPlaySpeed',
                'buildUpPlayDribbling',
                'buildUpPlayPassing',
                'chanceCreationPassing',
                'chanceCreationCrossing',
                'chanceCreationShooting',
                'defencePressure',
                'defenceAggression',
                'defenceTeamWidth')

teams_2015$id.x <- NULL
teams_2015$team_fifa_api_id <- NULL
teams_2015$team_short_name <- NULL
teams_2015$id.y <- NULL
teams_2015$date <- NULL
teams_2015$team_api_id <- NULL
teams_2015$year <- NULL

country_vector <- teams_2015$country
teams_2015$country <- NULL


# 1. Using only numeric
#    teams_2015_numvar <- teams_2015 %>% select(c('team_long_name' , numvars))
#    teams_2015_numvar <- dummy_cols(dataf, select_columns = 'rank')

#    teams_2015_numvar$team_long_name <- as.factor(teams_2015_numvar$team_long_name)
    
    # 1.1. HC clust
#        xteams_2015 <- scale(teams_2015_numvar[,(2:10)])
#        distance_2015 <- dist(xteams_2015, method="euclidean")
#        hc_2015 <- hclust(distance_2015, method = "average")
#        plot(hc_2015, labels = teams_2015_numvar$team_long_name)

    # 1.2. Heatmap of gaming style
#        matrix <- dist(teams_2015_numvar, upper = TRUE, diag = TRUE) #; matrix

#        col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)

#        heatmap(as.matrix(matrix), col = col, 
#            labRow = teams_2015_numvar$team_long_name,
#            labCol = teams_2015_numvar$team_long_name)


# 2. Using dummies over classes

    teams_2015_dummies <- dummy_cols(teams_2015[,(2:length(teams_2015))])
    teams_2015_dummies <- teams_2015_dummies[, !sapply(teams_2015_dummies, is.character)]
    teams_2015_dummies$team_long_name <- teams_2015$team_long_name

    # 2.1. HC clust
        xteams_2015 <- scale(teams_2015_dummies[,(1:length(teams_2015_dummies)-1)])
        distance_2015 <- dist(xteams_2015, method="euclidean")
        hc_2015 <- hclust(distance_2015, method = "average")
        plot(hc_2015, labels = teams_2015_dummies$team_long_name)

    # 2.2. Heatmap of gaming style
        matrix <- dist(teams_2015_dummies[,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = TRUE) #; matrix

        col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)

        heatmap(as.matrix(matrix), col = col, 
            labRow = teams_2015_dummies$team_long_name,
            labCol = teams_2015_dummies$team_long_name)


# 3. PCA clustering
    # 3.1. Prep

        pc_teams <- prcomp(t(teams_2015_dummies[,(1:length(teams_2015_dummies)-1)]), scale = TRUE)

        df <- data.frame(PC1 = pc_teams$rotation[,'PC1'], PC2 = pc_teams$rotation[,'PC2'],
                        label = teams_2015_dummies$team_long_name,
                        country = teams_2015$country)

        X <- as.matrix(df[,c(1,2)])
        k_teams <- kmeans(X, center=5, nstart=10)

        means <- as.data.frame(k_teams$centers)
        means$label <- NA
        means$country <- NA
    
    # 3.2. Plot

        gg <- ggplot(df, aes(PC1, PC2, col = country, label = label)) + geom_point(aes(shape=country), size=2) +
                labs(title = "Teams Clustering", 
                subtitle = "With principal components PC1 and PC2 as X and Y axis") +
                geom_point(data = as.data.frame(means), shape = 4, size = 5, col = 'black')
        gg

        df$cluster <- k_teams$cluster
    
    table(df$cluster, df$country)
    # To-Do, cruzar con los ganadores/perdedores
    
    # Do teams from the same country play similarly?
        # Apparently they don't

