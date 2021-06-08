### Description of Players
#install.packages("readxl")

## --- Preparation

rm(list=ls())


library(tidyverse)
library(tinytex)
library(openxlsx)
library(dplyr)
library(distances)
library(RColorBrewer)
library(fastDummies)
library(ggplot2)
library(ggrepel)
library(readxl)
library(gamlr)


#read data
Players <- read_excel("Player.xlsx")
Player_Attributes <- read_excel("Player_Attributes.xlsx")
Player_Attributes <- merge(Player_Attributes, Players, 
                           by = "player_fifa_api_id")

#add data for grouping
Player_Attributes$year <- as.numeric(substring(as.Date(Player_Attributes$date , "%Y-%m-%d"),1,4))
Player_Attributes$month<- as.numeric(substring(as.Date(Player_Attributes$date , "%Y-%m-%d"),6,7))
Player_Attributes$year_birth <- as.numeric(substring(as.Date(Player_Attributes$birthday , "%Y-%m-%d"),1,4))


# Distribution of Overall Rating
# Similar to normal distribution
pa_ag_or = Player_Attributes %>%
                      group_by(player_fifa_api_id) %>%
                      summarize( mean_or = mean(overall_rating),
                                 .groups = NULL)

hist(pa_ag_or$mean_or)


# Player Rating by Birth Year.
# Shows older players in general are better than younger players.
pa_ag_bd = Player_Attributes %>%
  group_by(player_fifa_api_id) %>%
  summarize( mean_or = mean(overall_rating),
             year_birth = year_birth, 
             .groups = NULL)

#both plots do the same
plot(y=pa_ag_bd$mean_or, x=pa_ag_bd$year_birth)
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(data = pa_ag_bd, aes(year_birth, mean_or))
g + geom_count(col="tomato3", show.legend=F) +
  labs(y="Potential", 
       x="Year of Birth", 
       title="Player Potencial by Birth Year")



# Player Potential by Birth Year.
# shows potential hasn't changed throughout the years.
pa_ag_po = Player_Attributes %>%
  group_by(player_fifa_api_id) %>%
  summarize( mean_po = mean(potential),
             year_birth = year_birth, 
             .groups = NULL)  

#both plots do the same
plot(y=pa_ag_po$mean_po, x=pa_ag_po$year_birth)
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(data = pa_ag_po, aes(year_birth, mean_po))
g + geom_count(col="tomato3", show.legend=F) +
  labs(y="Potential", 
       x="Year of Birth", 
       title="Player Potencial by Birth Year")


#ready hasta aqui


#crear lasso para ver cual es el atributo mas importante en atribute.

#database for reg
pa_short = Player_Attributes %>%
  select(-id.x, -player_api_id.x, -player_api_id.y, -player_name, -player_fifa_api_id, -date, -birthday, -year, -month, -year_birth)


reg1 = pa_short %>%
  gamlr( ., 
      pa_short$overall_rating,
      family="binomial",
      lambda.min.ratio=1e-3,
      data= .)

