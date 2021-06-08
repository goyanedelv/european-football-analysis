## Home advantage analysis

# install.packages("readxl")

setwd("~/Documents/Booth/41201 Big Data/Final project")
library("readxl")

# Load data
matches <- read_xlsx('Match.xlsx') ; dim(matches) # 25979 x 115
matches <- as.data.frame(matches) ; dim(matches)
colnames(matches)

# Clean table
matches <- matches[,1:11]
colnames(matches)
matches <- matches[,-1]
matches$date <- NULL
matches$match_api_id <- NULL
matches$league_id <- NULL

# Change Country_id for Country Name
countries <- read_xlsx('Country.xlsx')
countries <- as.data.frame(countries)
names(countries)[names(countries) == "id"] <- "country_id"
names(countries)[names(countries) == "name"] <- "country"
matches <- merge(matches, countries, by = 'country_id')
matches$country_id <- NULL

head(matches)
summary(matches)

# Difference in means and median of home vs away teams

mean(matches$home_team_goal) - mean(matches$away_team_goal)
median(matches$home_team_goal) - median(matches$away_team_goal)

'We can see that there is a difference in the average number of goals deppending on if
the team was playing local or not, but it is significative?

We can also see that the median for both cases is one goal so given that there can`t be
decimal scores, is the difference in means significant in the sense that it needs to 
produce a difference of at least one goal to have an impact in the result?'

boxplot(matches[,5:6], 
        main = "Number of goals per match", 
        names = c("Home","Away"))

'We can see in the previous boxplot that the boxes overlap so we are not so sure if
the difference that we found is statistically significant or not.'

# Create winner variable
matches$winner <- ifelse(matches$home_team_goal > matches$away_team_goal, "Home",
                        ifelse(matches$home_team_goal < matches$away_team_goal, "Away",
                               "Tie"))
table(matches$winner)

'We can see that from all the matches in the base, locals have a higher number of wins'

# Look results per team

table_per_team <- table(matches$home_team_api_id, matches$winner)
table_per_team <- as.data.frame(unclass(table_per_team))
nrow(table_per_team[table_per_team$Away > table_per_team$Home, ])/nrow(table_per_team)
nrow(table_per_team[table_per_team$Away < table_per_team$Home, ])/nrow(table_per_team)
nrow(table_per_team)

'We can see that 55.8% of the teams have more wins as locals, and 42.4% of teams
have more wins as visit teams.'



## --- Lasso for treatment effect - Doesn't need the upper code

library(gamlr)

# Code to prepare matches
matches <- read_xlsx('Match.xlsx') ; dim(matches) # 25979 x 115
matches <- as.data.frame(matches) ; dim(matches)
matches <- matches[,-(12:77)]
matches <- matches[,1:11]
matches$winner <- ifelse(matches$home_team_goal > matches$away_team_goal, "Home",
                         ifelse(matches$home_team_goal < matches$away_team_goal, "Away", "Tie"))
ties <- matches[matches$winner == 'Tie',] # Code to delete ties
dim(ties)
matches <- matches[-ties$id,]
matches$date <- as.Date(matches$date)
matches$year <- as.numeric(format(matches$date, "%Y"))
matches$date <- NULL
matches$league_id <- NULL
matches$stage  <- NULL
matches$id <- NULL
dim(matches) ; head(matches)

# Code to prepare teams
teams <- read_xlsx('Team_Attributes.xlsx')
teams <- as.data.frame(teams)
colnames(teams)
teams <- teams[, c(3,4,5,7,9,12,14,16,19,21,23)]
teams$date <- as.Date(teams$date)
teams$year <- as.numeric(format(teams$date, "%Y"))
teams$date <- NULL
dim(teams) ; head(teams)

# Matches and teams ready to use
dim(matches) ; head(matches)
dim(teams) ; head(teams)

# Prepare new table
newTable <- rbind(matches,matches) # Add matches under matches
dim(newTable) ; head(newTable)
for (i in 1:19383) {  # Create new variables for the first rows
  newTable$local[i] <- TRUE
  newTable$won[i] <- ifelse(newTable$winner[i] == "Home", TRUE, FALSE)
  newTable$team_api_id[i] <- newTable$home_team_api_id[i]
}
for (i in 19384:38766){ # Create new variables for the second rows
  newTable$local[i] <- FALSE
  newTable$won[i] <- ifelse(newTable$winner[i] == "Away", TRUE, FALSE)
  newTable$team_api_id[i] <- newTable$away_team_api_id[i]
}
newTable$home_team_api_id <- NULL
newTable$away_team_api_id <- NULL
newTable$home_team_goal <- NULL
newTable$away_team_goal <- NULL
newTable$winner <- NULL
dim(newTable) ; head(newTable)

# Create merging aux that considers team_api_id and year
newTable$mergingAux <- paste(newTable$team_api_id, newTable$year)
teams$mergingAux <- paste(teams$team_api_id, teams$year)
dim(newTable) ; head(newTable)
dim(teams) ; head(teams)

# Merge tables
newTable <- merge(newTable, teams, by = 'mergingAux')
dim(newTable) ; head(newTable)
newTable$mergingAux <- NULL
newTable$year.x <- NULL
newTable$year.y <- NULL
newTable$team_api_id.x <- NULL
newTable$team_api_id.y <- NULL
newTable$match_api_id <- NULL
newTable <- newTable[,c(4,3,1,2,5,6,7,8,9,10,11,12,13)]
dim(newTable) ; head(newTable)

# Prepare variables
newTable$season <- as.factor(newTable$season)
newTable$country_id <- as.factor(newTable$country_id)
newTable <- na.omit(newTable)
Y <- newTable[,1]
d <- newTable[,2]
X <- newTable[,-c(1,2)]

# Test treatment
reg <- glm(Y ~ d + ., data=X, family = 'binomial')
summary(reg)

# See if controls predict d
reg2 <- glm(d ~ ., data=X, family = 'binomial')
summary(reg2)
1-13056/13063


  

