## Gonzalo Oyanedel Vial
## June 2021

rm(list=ls())

library(openxlsx)

player_atts <- read.xlsx('0. data/Player_Attributes.xlsx')

player <- read.xlsx('0. data/Player.xlsx')

player_atts <- merge(player_atts, player, by = 'player_fifa_api_id')

str(player_atts)
player_atts$id.x <- NULL
player_atts$player_api_id.x <- NULL
player_atts$id.y <- NULL
player_atts$player_api_id.x <- NULL

player_atts$date <- as.Date(player_atts$date , "%Y-%m-%d")
player_atts$birthday <- as.Date(player_atts$birthday , "%Y-%m-%d")


player_atts$year <- as.numeric(substring(as.Date(player_atts$date , "%Y-%m-%d"),1,4))
player_atts$birthyear <- as.numeric(substring(as.Date(player_atts$birthday , "%Y-%m-%d"),1,4))
player_atts$age <-  player_atts$year - player_atts$birthyear

example <- player_atts[player_atts$player_fifa_api_id == 2,]
example <- data.frame(age = example$age, or = example$overall_rating)

attach(player_atts)
    plot(x = age, y = overall_rating)
detach(player_atts)

## Killer analysis
    # if I see 3 years, can I predict the fourth year?
        # contract intelligence

