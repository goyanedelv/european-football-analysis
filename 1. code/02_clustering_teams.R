## Gonzalo Oyanedel Vial
## June 2021

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
        
        png("3. output/S7.png", width=2000, height=2000)
        heatmap(as.matrix(matrix), col = col, 
            labRow = teams_2015_dummies$team_long_name,
            labCol = teams_2015_dummies$team_long_name)
        dev.off()

# 3. PCA clustering
    # 3.1. Prep

        pc_teams <- prcomp(t(teams_2015_dummies[,(1:length(teams_2015_dummies)-1)]), scale = TRUE)

        df <- data.frame(PC1 = pc_teams$rotation[,'PC1'], PC2 = pc_teams$rotation[,'PC2'],
                        team_long_name = teams_2015_dummies$team_long_name)

        cty <- unique(teams[,c('team_long_name', 'country')])
        #colnames(cty)[1] <- 'label'

        df <- merge(df, cty, by = 'team_long_name')

        X <- as.matrix(df[,c('PC1', 'PC2')])
        k_teams <- kmeans(X, center=5, nstart=10)

        means <- as.data.frame(k_teams$centers)
        means$team_long_name <- NA
        means$country <- NA
    
    # 3.2. Plot

        gg <- ggplot(df, aes(PC1, PC2, col = country, label = team_long_name)) + geom_point(aes(shape=country), size=2) +
                labs(title = "Teams Clustering", 
                subtitle = "With principal components PC1 and PC2 as X and Y axis") +
                geom_point(data = as.data.frame(means), shape = 4, size = 5, col = 'black')
        gg

        df$cluster <- k_teams$cluster

        df$cluster <- as.factor(df$cluster)
        df$country <- as.factor(df$country)
        df$team_long_name <- as.factor(df$team_long_name)

    # To-Do, cruzar con los ganadores/perdedores
    
    # Do teams from the same country play similarly?
        table(df$cluster, df$country)
        # Apparently they don't

    # Do teams with a certain style win more often?
        win_lose <- read.xlsx('0. data/Match_summary_by_team_year.xlsx')
        win_lose_15 <- win_lose[win_lose$year == 2015,]

        df <- merge(df, win_lose_15[, c('team_long_name', 'win_rate')])

        df_win_rate <- df %>%
            group_by(cluster) %>%
            summarise(avg_win_rate = mean(win_rate),
                        sd_win_rate = sd(win_rate))

        df_win_rate
        plot(df_win_rate$avg_win_rate)

        boxplot(df$win_rate, df$country)

        # Yes! Who are in cluster two?
            df$team_long_name[df$cluster == 2]
        
    # what makes teams in cluster  #2 better?
        df_rich <- merge(df, teams_2015, by = 'team_long_name')
        
        # analysis with lasso
            library(gamlr)
            X <- sparseMatrix(df_rich) # HTF do I create a sparse matrix from a data frame?

        # analysis with glm
            df_rich$is_2 <- 0
            df_rich$is_2[df_rich$cluster == 2] <- 1

            df_rich$is_2 <- as.factor(df_rich$is_2)

            select <- colnames(df_rich)
            select <- select[-which(select %in% c('team_long_name', 'PC1', 'PC2', 'cluster'))]

            model <- glm(is_2 ~ ., data = df_rich[,select], family = 'binomial')
            summary(model) # awful

            model_2 <- glm(win_rate ~ ., data = df_rich[,select], family = 'gaussian')
            summary(model_2) # very weak

        # analysis with random forest
            library(randomForest)
            select_3 <- colnames(df_rich)
            select_3 <- select_3[-which(select_3 %in% c('team_long_name', 'PC1', 'PC2'))]

            data_rf <- df_rich[,select_3]
            data_rf$is_2 <- NULL

            model_3 <- randomForest(cluster ~ ., data = data_rf, importance=TRUE, ntree=2)
            
            predicted <- predict(model_3,data_rf)

            df$pred_cluster <- predicted
            sum(df$cluster == df$pred_cluster)/nrow(df)

            model_3$importance
