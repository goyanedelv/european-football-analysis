## Gonzalo Oyanedel Vial
## June 2021

rm(list=ls())
set.seed(123)

library(openxlsx)
library(dplyr)
library(distances)
library(RColorBrewer)
library(fastDummies)
library(ggplot2)
library(ggrepel)
library(xtable)


team_atts <- read.xlsx('0. data/Team_Attributes.xlsx')

teams <- read.xlsx('0. data/Team.xlsx')
teams$team_api_id <- NULL

ligas <- c('England', 'Spain', 'Germany', 'France', 'Italy')
# ligas <- c('Spain', 'Germany')

teams <- subset(teams, teams$country %in% ligas)

teams <- merge(teams, team_atts, by = 'team_fifa_api_id')

teams$year <- as.numeric(substring(as.Date(teams$date , "%Y-%m-%d"),1,4))

teams_2015 <- subset(teams, teams$year == 2015) # It was 2015, but 2016 looks better

filter_en <- teams_2015$country == 'England'
filter_sp <- teams_2015$country == 'Spain'
filter_it <- teams_2015$country == 'Italy'
filter_gr <- teams_2015$country == 'Germany'
filter_fr <- teams_2015$country == 'France'

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

        # Spain
            xteams_2015 <- scale(teams_2015_dummies[filter_sp,(1:length(teams_2015_dummies)-1)])
            distance_2015 <- dist(xteams_2015, method="euclidean")
            hc_2015_sp <- hclust(distance_2015, method = "average")

            png("3. output/Spain_dendro.png", width=1000, height=500)
            plot(hc_2015_sp, labels = teams_2015_dummies[filter_sp, "team_long_name"], xlab = "Spanish Teams")
            dev.off()

        # France
            xteams_2015 <- scale(teams_2015_dummies[filter_fr,(1:length(teams_2015_dummies)-1)])
            distance_2015 <- dist(xteams_2015, method="euclidean")
            hc_2015_fr <- hclust(distance_2015, method = "average")

            png("3. output/France_dendro.png", width=1000, height=500)
            plot(hc_2015_fr, labels = teams_2015_dummies[filter_fr, "team_long_name"], xlab = "France Teams")
            dev.off()

        # England
            xteams_2015 <- scale(teams_2015_dummies[filter_en,(1:length(teams_2015_dummies)-1)])
            distance_2015 <- dist(xteams_2015, method="euclidean")
            hc_2015_en <- hclust(distance_2015, method = "average")

            png("3. output/England_dendro.png", width=1000, height=500)
            plot(hc_2015_en, labels = teams_2015_dummies[filter_en, "team_long_name"], xlab = "England Teams")
            dev.off()

        # Germany
            xteams_2015 <- scale(teams_2015_dummies[filter_gr,(1:length(teams_2015_dummies)-1)])
            distance_2015 <- dist(xteams_2015, method="euclidean")
            hc_2015_gr <- hclust(distance_2015, method = "average")

            png("3. output/Germany_dendro.png", width=1000, height=500)
            plot(hc_2015_gr, labels = teams_2015_dummies[filter_gr, "team_long_name"], xlab = "Germany Teams")
            dev.off()

        # Italy
            xteams_2015 <- scale(teams_2015_dummies[filter_it,(1:length(teams_2015_dummies)-1)])
            distance_2015 <- dist(xteams_2015, method="euclidean")
            hc_2015_it <- hclust(distance_2015, method = "average")

            png("3. output/Italy_dendro.png", width=1000, height=500)
            plot(hc_2015_it, labels = teams_2015_dummies[filter_it, "team_long_name"], xlab = "Italy Teams")
            dev.off()



    # 2.2. Heatmap of gaming style
        #png("3. output/4X4_similarity_hm.png", width=1000, height=1000)
        #par(mfrow=c(2,2))
        # Spain   
            matrix <- dist(teams_2015_dummies[filter_sp,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/Spain_similarity_hm.png", width=1000, height=1000)
            #heatmap(as.matrix(matrix), col = col, 
            #    labRow = NA,#teams_2015_dummies[filter_sp ,'team_long_name'],
            #    labCol = NA,#teams_2015_dummies[filter_sp ,'team_long_name'],
            #    Colv = NA, Rowv = NA)
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'Spain')

            dev.off()

        # France   
            matrix <- dist(teams_2015_dummies[filter_fr,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            #col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/France_similarity_hm.png", width=1000, height=1000)
            #heatmap(as.matrix(matrix), col = col, 
            #    labRow = teams_2015_dummies[filter_fr ,'team_long_name'],
            #    labCol = teams_2015_dummies[filter_fr ,'team_long_name'],
            #    Rowv=NULL, Colv=NULL )
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'France')

            dev.off()

        # England   
            matrix <- dist(teams_2015_dummies[filter_en,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/England_similarity_hm.png", width=1000, height=1000)
            #heatmap(as.matrix(matrix), col = col, 
            #    labRow = teams_2015_dummies[filter_en ,'team_long_name'],
            #    labCol = teams_2015_dummies[filter_en ,'team_long_name'])
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'England')

            dev.off()

        # Italy   
            matrix <- dist(teams_2015_dummies[filter_it,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/Italy_similarity_hm.png", width=1000, height=1000)
            #heatmap(as.matrix(matrix), col = col, 
            #    labRow = teams_2015_dummies[filter_it ,'team_long_name'],
            #    labCol = teams_2015_dummies[filter_it ,'team_long_name'])
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'Italy')

            dev.off()

        # Germany   
            matrix <- dist(teams_2015_dummies[filter_gr,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/Germany_similarity_hm.png", width=1000, height=1000)
            #heatmap(as.matrix(matrix), col = col, scale='none', 
            #    labRow = teams_2015_dummies[filter_gr ,'team_long_name'],
            #    labCol = teams_2015_dummies[filter_gr ,'team_long_name'])
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'Germany')

            dev.off()

        # ALL
            matrix <- dist(teams_2015_dummies[,(1:length(teams_2015_dummies)-1)], upper = TRUE, diag = FALSE) #; matrix

            col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
            
            png("3. output/ALL_similarity_hm.png", width=1000, height=1000)
            heatmap(as.matrix(matrix), col = col, labRow = NA, labCol = NA, Colv = NA, Rowv = NA, main = 'All countries')

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
        k_teams <- kmeans(X, center=3, nstart=10)

        means <- as.data.frame(k_teams$centers)
        means$team_long_name <- as.character(NA)
        means$country <- as.character(NA)
    
    # 3.2. Plot

        good_teams <- c('Real Madrid CF', 'Liverpool', 'Paris Saint-Germain', 'Milan', 'Arsenal', 'Juventus', 'FC Barcelona', 'Manchester United', 'Manchester City', 'Inter')
        all_teams <- df$team_long_name
        mod_labels <- rep(NA, length(all_teams))

        for (i in 1:length(all_teams)){
            if(all_teams[i] %in% good_teams){
                mod_labels[i] <- all_teams[i]
            }
        }
        
        gg <- ggplot(df, aes(PC1, PC2, col = country, label = mod_labels)) + geom_point(size=2) +
                labs(title = "Teams Clustering", 
                subtitle = "With principal components PC1 and PC2 as X and Y axis") #+
                #geom_point(data = means, shape = 4, size = 5)

        ggsave("3. output/clusters.pdf", width = 20, height = 20, units = "cm")
        gg + geom_text_repel(col = 'black')
        dev.off()
        
        df$cluster <- k_teams$cluster

        df$cluster <- as.factor(df$cluster)
        df$country <- as.factor(df$country)
        df$team_long_name <- as.factor(df$team_long_name)

    # To-Do, cruzar con los ganadores/perdedores
    
    # Do teams from the same country play similarly?
        table_countries <- table(df$cluster, df$country)
        print(xtable(table_countries, type = "latex"), file = "3. output/table_countries.tex")
        # Apparently they don't

    # Do teams with a certain style win more often?
        win_lose <- read.xlsx('0. data/Match_summary_by_team_year.xlsx')
        win_lose_15 <- win_lose[win_lose$year == 2015,]

        df <- merge(df, win_lose_15[, c('team_long_name', 'win_rate')])

        df_win_rate <- df %>%
            group_by(cluster) %>%
            summarise(avg_win_rate = mean(win_rate),
                        sd_win_rate = sd(win_rate))

        print(xtable(df_win_rate, type = "latex"), file = "3. output/table_win_rate.tex")

        # Yes! Who are in cluster two?
        print(xtable(as.data.frame(df$team_long_name[df$cluster == 3])), file = "3. output/teams_in_cluster_3.tex")

        # Now, further characterization
        df <- merge(df, teams_2015, by = 'team_long_name')

       df_characteristics <- df %>%
            group_by(cluster) %>%
            summarise(buildUpPlaySpeed = round(mean(buildUpPlaySpeed),1),
                        buildUpPlayPassing = round(mean(buildUpPlayPassing),1),
                        buildUpPlayDribbling = round(mean(buildUpPlayDribbling),1),
                        defenceAggression = round(mean(defenceAggression),1),
                        buildUpPlayDribbling = round(mean(buildUpPlayDribbling),1),
                        win_rate = round(mean(win_rate),3)            
                        )
        
        charact <- as.data.frame(df_characteristics)
        print(xtable(charact, type = "latex"), file = "3. output/table_characteristics.tex")

    # what makes teams in cluster  #2 better?
        
        # analysis with lasso
            library(gamlr)
            # Data frame to sparse matrix
            
            select <- colnames(df)
            select <- select[-which(select %in% c('team_long_name', 'PC1', 'PC2', 'cluster'))]

            scx <- sparse.model.matrix(win_rate ~ ., data = df[,select])[,-1]

            lasso <- gamlr(scx, df$win_rate)
            
            png("3. output/lasso_clusters.png", width=500, height=500)
            plot(lasso)
            dev.off()
            
            sel_beta <- coef(lasso)
            sel_beta <- sel_beta[-1,]
            names <- names(sel_beta)
            sel_beta_df = data.frame(sel_beta)
            sel_beta_df$attribute <- rownames(sel_beta_df)
            rownames(sel_beta_df) <- NULL

            sel_beta_no0 <- sel_beta_df[sel_beta_df$sel_beta != 0,]
            char_lasso <- sel_beta_no0[,c(2,1)]
        print(xtable(char_lasso, type = "latex"), file = "3. output/table_characteristics_lasso.tex")
