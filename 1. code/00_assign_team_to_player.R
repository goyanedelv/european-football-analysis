rm(list=ls())

library(openxlsx)

match_ <- read.xlsx('0. data/Match.xlsx')
match_$date <- as.numeric(substring(as.Date(match_$date , "%Y-%m-%d"),1,4))

player <- read.xlsx('0. data/Player.xlsx')

# player_simp <- unique(player$player_fifa_api_id)
player_simp <- unique(player$player_api_id)

yr_vector <- 2008:2018

collector <- data.frame(player_fifa_api_id = NULL, year = NULL, home_team_api_id = NULL)

count = 0

for (pid in player_simp){

    for (y in yr_vector){

        cp_state <- (match_$home_player_1 == pid |
                    match_$home_player_3 == pid |
                    match_$home_player_2 == pid |
                    match_$home_player_4 == pid |
                    match_$home_player_5 == pid |
                    match_$home_player_6 == pid |
                    match_$home_player_7 == pid |
                    match_$home_player_8 == pid |
                    match_$home_player_9 == pid |
                    match_$home_player_10 == pid |
                    match_$home_player_11 == pid)

           temp <- subset(match_, cp_state & match_$date == y)

           if (nrow(temp) > 0){
                home_team_api_id <- temp$home_team_api_id[nrow(temp)]
                collector <- rbind(collector, c(pid, y, home_team_api_id))
                # print(nrow(temp))
                # print(home_team_api_id)

           }

           count = count + 1
           x = round(100*count/121660,1)
           message('\r', paste0(x," %"), appendLF = FALSE)
    }
}

write.xlsx(collector, "cross_walk_player_team_2.xlsx")