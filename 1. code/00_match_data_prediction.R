## Gonzalo Oyanedel Vial
## June 2021

library(openxlsx)

teams <- read.xlsx('0. data/Team.xlsx')
player <- read.xlsx('0. data/Player.xlsx')
match <- read.xlsx('0. data/Match.xlsx')
team_attributes <- read.xlsx('0. data/Team_Attributes.xlsx')
player_attributes <- read.xlsx('0. data/Player_Attributes.xlsx')

cw <- read.xlsx('0. data/cross_walk_player_team_2.xlsx')

# Prediction wishlist
    # Team attributes [OK]
        # Preocuparse de que pegue el último
    # Average of attributes of players

match$goal_diff <- match$home_team_goal - match$away_team_goal

match$who_won <- ifelse(match$goal_diff > 0,"home",ifelse(match$goal_diff < 0,"away","tie"))

# Select only columns that we are interested from the match database
select <- c("country_id", "date", "home_team_api_id", "away_team_api_id", "who_won")
match_reduced <- match[,select]

# Who won as factor
match_reduced$who_won <- as.factor(match_reduced$who_won)

# Date as numeric year
match_reduced$year <- as.numeric(substring(as.Date(match_reduced$date , "%Y-%m-%d"),1,4))
match_reduced$date <- as.Date(match_reduced$date , "%Y-%m-%d")

# Merging with team attributes

    # Remove columns we don't like
    team_attributes$id <- NULL
    team_attributes$team_fifa_api_id <- NULL

    # Date as numeric
    team_attributes$year <- as.numeric(substring(as.Date(team_attributes$date , "%Y-%m-%d"),1,4))
    #team_attributes$date <- NULL
    team_attributes$date <- as.Date(team_attributes$date , "%Y-%m-%d")

    # Duplicate data team_attributes
    ta_home <- team_attributes
    ta_away <- team_attributes

    # Change col names
    cols_ta_home <- colnames(ta_home)
    cols_ta_home <- paste0("home_",cols_ta_home)
    colnames(ta_home) <- cols_ta_home

    cols_ta_away <- colnames(ta_away)
    cols_ta_away <- paste0("away_",cols_ta_away)
    colnames(ta_away) <- cols_ta_away

    # Delete unnecessary fields
    colnames(ta_home)[24] <- 'year'
    colnames(ta_away)[24] <- 'year'
    
    colnames(ta_home)[2] <- 'date'
    colnames(ta_away)[2] <- 'date'

    ta_home$date <- NULL
    ta_away$date <- NULL

    # Proper mergers
    match_merged <- merge(match_reduced, ta_home, by = c('home_team_api_id','year'))
    match_merged <- merge(match_merged, ta_away, by = c('away_team_api_id', 'year'))

    # Dele unnecessary fields, bis
    match_merged$home_date <- NULL
    match_merged$away_date <- NULL
    match_merged$country <- NULL

    # Now, put real names
    teams$id <- NULL
    teams$team_fifa_api_id <- NULL
    teams$team_short_name <- NULL

    colnames(teams)[1] <- 'home_team_api_id'
    colnames(teams)[2] <- 'home_team_long_name'
    colnames(teams)[3] <- 'home_country'

    match_merged <- merge(match_merged, teams, by = 'home_team_api_id')

    colnames(teams)[1] <- 'away_team_api_id'
    colnames(teams)[2] <- 'away_team_long_name'
    colnames(teams)[3] <- 'away_country'

    match_merged <- merge(match_merged, teams, by = 'away_team_api_id')


# Merging with player attributes
    filler_or <- NA #as.numeric(quantile(player_attributes$overall_rating, na.rm = TRUE)["25%"])

    # By default, all overall ratings starts as 25% (so no drama if we don't find a player)
        # it is more likely a bad player is not in attributes rather than a good one
    match_merged$home_or <- filler_or
    match_merged$away_or <- filler_or

    for (i in 1:nrow(match_merged)){
        htai <- match_merged$home_team_api_id[i] # home team api id 
        atai <- match_merged$away_team_api_id[i] # home team api id

        yr <- match_merged$year[i]

        all_home_players <- cw$player_fifa_api_id[cw$home_team_api_id == htai & cw$year == yr]
        all_away_players <- cw$player_fifa_api_id[cw$home_team_api_id == atai & cw$year == yr]

        or_home <- mean(player_attributes$overall_rating[player_attributes$player_fifa_api_id %in% all_home_players], na.rm = TRUE)
        or_away <- mean(player_attributes$overall_rating[player_attributes$player_fifa_api_id %in% all_away_players], na.rm = TRUE)

        if (!is.nan(or_home)){
            match_merged$home_or[i] <- or_home
        }
        else{
            all_home_players <- cw$player_fifa_api_id[cw$home_team_api_id == htai]
            or_home <- mean(player_attributes$overall_rating[player_attributes$player_fifa_api_id %in% all_home_players], na.rm = TRUE)
            match_merged$home_or[i] <- or_home
        }

        if (!is.nan(or_away)){
            match_merged$away_or[i] <- or_away
        }
        else{
            all_away_players <- cw$player_fifa_api_id[cw$home_team_api_id == atai]
            or_away <- mean(player_attributes$overall_rating[player_attributes$player_fifa_api_id %in% all_away_players], na.rm = TRUE)
            match_merged$away_or[i] <- or_away
        }

    }

    # As factor round
    match_merged$home_team_api_id <- as.factor(match_merged$home_team_api_id)
    match_merged$away_team_api_id <- as.factor(match_merged$away_team_api_id)
    match_merged$country_id <- as.factor(match_merged$country_id)
    # to-do: transform all chr to factor

    # Now, we impute NAs
        # too many columns with na's
        # library(mi)
        # imputed <- mi(match_merged[,-27], seed = 335, n.iter = 5)
        # don't do this, it takes too much time :(

write.xlsx(match_merged, "0. data/Match_for_prediction.xlsx")
