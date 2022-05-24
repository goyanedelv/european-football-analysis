rm(list=ls())
install.packages('tidyr')
install.packages('ellipsis')
install.packages('vctrs')

library(openxlsx)
library(dplyr)
library(ggplot2)
library("readxl")
library(data.table)
library(glmnet)
library(nnet)
library(tidyverse)
library(tidyr)
library(dplyr)
library(broom) 
library(Hmisc) 
library(gamlr)
library(ranger)

source("roc.R")

set.seed(1234)
teams <- read.xlsx('Team.xlsx')
player <- read.xlsx('Player.xlsx')
match <- read.xlsx('Match.xlsx')
team_attributes <- read.xlsx('Team_Attributes.xlsx')
player_attributes <- read.xlsx('Player_Attributes.xlsx')
match_prediction <- read.xlsx('Match_for_prediction.xlsx')

match_prediction_DT = data.table(match_prediction)
match_prediction_DT$who_won = ifelse(match_prediction_DT$who_won == 'home', 1, 0)
match_prediction_DT = subset(match_prediction_DT,select=-c(away_buildUpPlayDribbling,home_buildUpPlayDribbling,home_team_long_name , away_team_long_name , date,country_id,home_team_api_id,away_team_api_id,home_country,away_country,year,home_buildUpPlaySpeedClass,home_buildUpPlayDribblingClass,home_buildUpPlayPassingClass,home_chanceCreationPassingClass,home_chanceCreationCrossingClass,home_chanceCreationShootingClass,home_defencePressureClass,home_defenceAggressionClass,home_defenceTeamWidthClass,away_buildUpPlaySpeedClass,away_buildUpPlayDribblingClass,away_buildUpPlayPassingClass,away_chanceCreationPassingClass,away_chanceCreationCrossingClass,away_chanceCreationShootingClass,away_defencePressureClass,away_defenceAggressionClass,away_defenceAggressionClass,away_defenceTeamWidthClass,home_chanceCreationPositioningClass,home_defenceDefenderLineClass,away_buildUpPlayPositioningClass,away_chanceCreationPositioningClass,away_defenceDefenderLineClass,home_buildUpPlayPositioningClass))
X=match_prediction_DT
####################### Logit regression ################################
fit_Logit = glm(who_won ~ .^2, family = binomial, data = X)

#In sample Prediction
pred = predict(fit_Logit, newdata=X, type="response") 
real = X$who_won
cutoff = 0.5
class=as.numeric(pred>cutoff)
MO=table(real,class)
MO=prop.table(MO,2)
roc(p=as.vector(pred), y=real, bty="n",col='red')
#Plot
boxplot(pred ~ real,
        xlab="", ylab=c("predicted probability of winning"), 
        col=c("navy","red"))

#Out of sample prediction
leaveout <- sample(1:nrow(X), 1000)
fitOOS <- glm(who_won ~ .^2, data=X[-leaveout,], family='binomial')
predOOS <- predict(fitOOS, newdata=X[leaveout,], type="response")
boxplot(predOOS ~ real[leaveout],
     xlab="", ylab=c("predicted probability of winning"), 
     col=c("navy","red"))
class=as.numeric(predOOS>cutoff)
MO=table(real[leaveout],class)
MO=prop.table(MO,2)
#ROC curve
roc(p=predOOS, y=real[leaveout], bty="n",col='blue')


################################# LASSO ##################################################

XL=subset(X, select=-c(who_won))
f=as.formula(real~.^2)
XL=model.matrix(f,XL)[,-1]
LASSO = gamlr(x=XL, y=real, family = "binomial", lambda.min.ratio=1e-4)
plot(LASSO)

#Cross Validation
cv.LASSO = cv.gamlr(x=XL, y=real, family = "binomial", lambda.min.ratio=1e-4)


#In sample prediction
pred = predict(cv.LASSO, newdata=XL, select="min", type="response")
cutoff=0.5
class=as.numeric(pred>cutoff)
MO=table(real,class)
MO=prop.table(MO,2)
#Plot
boxplot(pred ~ real,
        xlab="", ylab=c("predicted probability of winning"), 
        col=c("navy","red"))


#Out of sample prediction
fitOOS2 =  cv.gamlr(x=XL[-leaveout,], y=real[-leaveout], family = "binomial", lambda.min.ratio=1e-4)
predOOS2 <- predict(fitOOS2, newdata=XL[leaveout,], type="response")
boxplot(predOOS2 ~ real[leaveout],
        xlab="", ylab=c("predicted probability of winning"), 
        col=c("navy","red"))
class=as.numeric(predOOS2>cutoff)
MO=table(real[leaveout],class)
MO=prop.table(MO,2) #superior than before

#ROC curve
roc(p=predOOS, y=real[leaveout], bty="n",col='blue')
par(new=TRUE)
roc(p=predOOS2, y=real[leaveout], bty="n",col='red')


#################### Random Forest #####################################
install.packages(ranger)

fit_RF = ranger(who_won ~ .^2, data = X,
             num.trees = 2000,
             seed = 1234, num.threads = NULL)

#In sample prediction
pred = predict(fit_RF, data=XL, type="response")$predictions
class=as.numeric(pred>cutoff)
MO=table(real,class)
MO=prop.table(MO,2)

#Out of sample prediction
install.packages('randomForest')
library(randomForest)

fitOOS3 =  ranger(who_won ~ .^2, data =X[-leaveout],
                  num.trees = 2000,
                  seed = 1234, num.threads = NULL)
predOOS3 = predict(fitOOS3, data=XL[leaveout,], type="response")$predictions
class=as.numeric(predOOS3>cutoff)
MO=table(real[leaveout],class)
MO=prop.table(MO,2)

########## Comparing OOS ROC curves ############
leaveout <- sample(1:nrow(X), 1000)
fitOOS <- glm(who_won ~ .^2, data=X[-leaveout,], family='binomial')
predOOS <- predict(fitOOS, newdata=X[leaveout,], type="response")
fitOOS2 =  cv.gamlr(x=XL[-leaveout,], y=real[-leaveout], family = "binomial", lambda.min.ratio=1e-4)
predOOS2 <- predict(fitOOS2, newdata=XL[leaveout,], type="response")
fitOOS3 =  ranger(who_won ~ .^2, data =X[-leaveout],
                  num.trees = 2000,
                  seed = 1234, num.threads = NULL)
predOOS3 = predict(fitOOS3, data=XL[leaveout,], type="response")$predictions

par(mfrow=c(1,1))
roc(p=predOOS, y=real[leaveout], bty="n",col='blue') #logit
par(new=TRUE)
roc(p=predOOS2, y=real[leaveout], bty="n",col='red') #cv.lasso
par(new=TRUE)
roc(p=predOOS3, y=real[leaveout], bty="n",col='orange') #random forest

