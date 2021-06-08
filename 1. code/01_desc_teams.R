### Description of Team Attributes
#install.packages("readxl")

## --- Preparation

library("readxl")

# Load data and merge tables
team_atts <- read_xlsx('Team_Attributes.xlsx') ; dim(team_atts) # 1458 x 25
teams <- read_xlsx('Team.xlsx') ; dim(teams) # 299 x 6 
teams$team_api_id <- NULL # Remove repeated column 
teams <- merge(teams, team_atts, by = 'team_fifa_api_id') ; dim(teams) # 1473 x 29
teams$id.x <- NULL # Remove column
teams$id.y <- NULL # Remove column
dim(teams) # 1473 x 27 
summary(teams)

# Prepare table 
teams <- as.data.frame(teams) 
teams$date <- as.Date(teams$date)
teams$buildUpPlaySpeedClass <- as.factor(teams$buildUpPlaySpeedClass)
teams$buildUpPlayDribblingClass <- as.factor(teams$buildUpPlayDribblingClass)
teams$buildUpPlayPassingClass <- as.factor(teams$buildUpPlayPassingClass)
teams$buildUpPlayPositioningClass <- as.factor(teams$buildUpPlayPositioningClass)
teams$chanceCreationCrossingClass <- as.factor(teams$chanceCreationCrossingClass)
teams$chanceCreationPassingClass <- as.factor(teams$chanceCreationPassingClass)
teams$chanceCreationShootingClass <- as.factor(teams$chanceCreationShootingClass)
teams$chanceCreationPositioningClass <- as.factor(teams$chanceCreationPositioningClass)
teams$defencePressureClass <- as.factor(teams$defencePressureClass)
teams$defenceAggressionClass <- as.factor(teams$defenceAggressionClass)
teams$defenceTeamWidthClass <- as.factor(teams$defenceTeamWidthClass)
teams$defenceDefenderLineClass <- as.factor(teams$defenceDefenderLineClass)
teams$buildUpPlayDribblingClass <- as.factor(teams$buildUpPlayDribblingClass)
teams$country <- as.factor(teams$country)

summary(teams)

## --- Describing variables

colnames(teams)
dim(teams)
plot <- hist(teams$chanceCreationPassing,
             main = "Distribution of chanceCreationPassing",
             xlab = "")

## --- Cluster Spain teams of 2015

teams$year <- as.numeric(format(teams$date, "%Y"))
teams_subset <- teams[teams$year=="2015",] ; dim(teams_subset)
teams_subset <- teams_subset[teams_subset$country == "Spain",] ; dim(teams_subset)
teams_expanded <- model.matrix(~., data=teams_subset[,7:28])
head(teams_expanded)
teams_expanded <- teams_expanded[,-1]
xteams <- scale(teams_expanded)
distance <- dist(xteams, method="euclidean")
hc <- hclust(distance, method = "average")
plot(hc, labels = teams_subset$team_long_name, main = "Spain teams of 2015")



