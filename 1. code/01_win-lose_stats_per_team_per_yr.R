## Gonzalo Oyanedel Vial
## June 2021

library(openxlsx)
library(dplyr)

match <- read.xlsx('0. data/Match_for_prediction.xlsx')

select_home <- c('home_team_api_id',
            'year',
            'who_won')

home_wins <- match[match$who_won == 'home', select] %>%
    group_by(home_team_api_id, year) %>%
    summarise(home_wins = n()) # ok

away_wins <- match[match$who_won == 'away', select] %>%
    group_by(away_team_api_id, year) %>%
    summarise(away_wins = n()) # ok

home_lose <- match[match$who_won == 'away', select] %>%
    group_by(home_team_api_id, year) %>%
    summarise(home_lose = n()) # ok

away_lose <- match[match$who_won == 'home', select] %>%
    group_by(away_team_api_id, year) %>%
    summarise(away_lose = n()) # ok

home_tie <- match[match$who_won == 'tie', select] %>%
    group_by(home_team_api_id, year) %>%
    summarise(home_tie = n()) # ok

away_tie <- match[match$who_won == 'tie', select] %>%
    group_by(away_team_api_id, year) %>%
    summarise(away_tie = n()) # ok

home_matches <- match[, select] %>%
    group_by(home_team_api_id, year) %>%
    summarise(home_matches = n()) # ok

away_matches <- match[, select] %>%
    group_by(away_team_api_id, year) %>%
    summarise(away_matches = n()) # ok

colnames(home_wins)[1] <- 'team_api_id'
colnames(away_wins)[1] <- 'team_api_id'
colnames(home_lose)[1] <- 'team_api_id'
colnames(away_lose)[1] <- 'team_api_id'
colnames(home_tie)[1] <- 'team_api_id'
colnames(away_tie)[1] <- 'team_api_id'
colnames(home_matches)[1] <- 'team_api_id'
colnames(away_matches)[1] <- 'team_api_id'

# big bad merge
    matches_summary <- merge(home_matches, away_matches, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, home_wins, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, away_wins, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, home_lose, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, away_lose, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, home_tie, all.x = TRUE, by = c('team_api_id', 'year'))
    matches_summary <- merge(matches_summary, away_tie, all.x = TRUE, by = c('team_api_id', 'year'))

    head(matches_summary)

# Intearnally create a api_id <-> team name dictionary
    dict <- match[,c('home_team_api_id', 'home_team_long_name')]
    dict2 <- match[,c('away_team_api_id', 'away_team_long_name')]

    colnames(dict) <- c('team_api_id', 'team_long_name')
    colnames(dict2) <- c('team_api_id', 'team_long_name')

    dict <- rbind(dict, dict2)

    dict <- unique(dict)

# Put names on it
    matches_summary <- merge(matches_summary, dict, all.x = TRUE, by =c('team_api_id'))

# NA's are 0's
    matches_summary$home_wins[is.na(matches_summary$home_wins)] <- 0
    matches_summary$away_wins[is.na(matches_summary$away_wins)] <- 0
    matches_summary$home_lose[is.na(matches_summary$home_lose)] <- 0
    matches_summary$away_lose[is.na(matches_summary$away_lose)] <- 0
    matches_summary$home_tie[is.na(matches_summary$home_tie)] <- 0
    matches_summary$away_tie[is.na(matches_summary$away_tie)] <- 0

# Factors
    matches_summary$team_api_id <- as.factor(matches_summary$team_api_id)
    matches_summary$team_long_name <- as.factor(matches_summary$team_long_name)

# summary variables
    matches_summary$all_matches <- matches_summary$home_matches + matches_summary$away_matches
    matches_summary$all_wins <- matches_summary$home_wins + matches_summary$away_wins
    matches_summary$all_lose <- matches_summary$home_lose + matches_summary$away_lose
    matches_summary$all_tie <- matches_summary$home_tie + matches_summary$away_tie
    matches_summary$win_rate <- matches_summary$all_wins / matches_summary$all_matches

head(matches_summary)

head(matches_summary[matches_summary$team_long_name == 'FC Barcelona',])

# now, pastethe country so we can look up at the interesting leagues more easily
    teams <- read.xlsx('0. data/Team.xlsx')
    teams <- teams[, c('team_api_id', 'country')]
    
    matches_summary <- merge(matches_summary, teams, by ='team_api_id')

write.xlsx(matches_summary, "0. data/Match_summary_by_team_year.xlsx")
