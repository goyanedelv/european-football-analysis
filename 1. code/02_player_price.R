rm(list=ls())

library(openxlsx)
library(gamlr)
library(Hmisc)
library(xtable)

data <- read.xlsx('Match_for_prediction.xlsx')
data1<- read.xlsx('Player_Attributes.xlsx')
data2=read.xlsx('Player.xlsx')
data5=read.csv('players_16.csv')

#Get most recent attributes of the database
data1$player_fifa_api_id=factor(data1$player_fifa_api_id)
x=levels(data1$player_fifa_api_id)
c=c()
for(i in x) {
a=data1[which(data1$player_fifa_api_id==i), ]
b=a[which(a$date==max(a$date)),]
c=rbind(c,b)}
player=c

#Get value and positions of the players from another database
pr_pos=subset(data5,select=c(sofifa_id,value_eur,team_position,player_positions))
head(pr_pos)
a=pr_pos$player_positions
a=as.data.frame(a)

#Select first of player positions
within(a, a<-data.frame(do.call('rbind', strsplit(as.character(a), ',', fixed=TRUE))))
a=data.frame(do.call("rbind", strsplit(as.character(pr_pos$player_positions), ",", fixed = TRUE)))
a[,1]
pr_pos[,c("player_positions")]=a[,1]

#Select team_position and if he is substitute or does not have then select first among player positions
pr_pos$team_position[which(pr_pos$team_position==c("SUB"))]=pr_pos$player_positions[which(pr_pos$team_position==c("SUB"))]
pr_pos$team_position[which(pr_pos$team_position==c(""))]=pr_pos$player_positions[which(pr_pos$team_position==c(""))]
pr_pos$team_position[which(pr_pos$team_position==c("RES"))]=pr_pos$player_positions[which(pr_pos$team_position==c("RES"))]
table(pr_pos$team_position)

#Re-level positions in broader categories
MP=c("CAM","CDM","CM","LAM","LCM","LM","LW","RAM","RCM","RDM","RM","RW")
DP=c("CB","LB","LCB","LDM","LWB","RB","RCB","RWB")
AP=c("CF","LF","LS","RES","RF","RS","ST")
GK=c("GK")
for(i in MP) {
  pr_pos$team_position[which(pr_pos$team_position==i)]="MP"
}
for(i in DP) {
  pr_pos$team_position[which(pr_pos$team_position==i)]="DP"
}
for(i in AP) {
  pr_pos$team_position[which(pr_pos$team_position==i)]="AP"
}
for(i in GK) {
  pr_pos$team_position[which(pr_pos$team_position==i)]="GK"
}
pr_pos=subset(pr_pos,select=-c(player_positions))
head(pr_pos)
posit=rbind(c("CAM,CDM,CM,LAM,LCM,LM,LW,RAM,RCM,RDM,RM,RW,DP,AP,GK"),c("CB,LB,LCB,LDM,LWB,RB,RCB,RWB"),c("CF,LF,LS,RES,RF,RS,ST"),c("GK"))
rownames(posit)=c("Midfield Positions","Defensive Positions","Attacking Positions","Goal Keepers")
colnames(posit)=c("FIFA Positions")
xtable(posit)
#Merger price and position dataset with player attributes dataset
names(pr_pos)=c("player_fifa_api_id","value_eur","team_position")
player=merge(player,pr_pos,by='player_fifa_api_id')
a=subset(data2, select=-c(id,player_api_id,player_name))
player=merge(player,a,by='player_fifa_api_id')
player=subset(player,select= -c(id,player_api_id,overall_rating))

#Get the age
player$date=as.Date(player$date , "%Y-%m-%d")
head(player)
player$birthday <- as.Date(player$birthday , "%Y-%m-%d")
player$year <- as.numeric(substring(as.Date(player$date , "%Y-%m-%d"),1,4))
player$birthyear <- as.numeric(substring(as.Date(player$birthday , "%Y-%m-%d"),1,4))
player$age <-  player$year - player$birthyear
player=subset(player,select= -c(birthyear,birthday,year,date))
head(player)

#add age^2 and weight/height
player$age2=player$age^2
player$weight_height=player$weight/player$height


#Separate datasets in different players positions
player_MP=player[which(player$team_position==c("MP")),]
player_DP=player[which(player$team_position==c("DP")),]
player_AP=player[which(player$team_position==c("AP")),]
player_GK=player[which(player$team_position==c("GK")),]

#take out variables that shouldn't matter
DMP=subset(player_MP,select=-c(team_position, attacking_work_rate, defensive_work_rate, potential))
DDP=subset(player_DP,select=-c(team_position, attacking_work_rate, defensive_work_rate, potential))
DAP=subset(player_AP,select=-c(team_position, attacking_work_rate, defensive_work_rate, potential))
DGK=subset(player_GK,select=-c(team_position, attacking_work_rate, defensive_work_rate, potential))

#Data Analysis
par(mfrow=c(1,1))
boxplot(log(DMP$value_eur),log(DDP$value_eur),log(DAP$value_eur),log(DGK$value_eur),ylab="Value in Euros (log)",xlab=c("Positions"),names=c("MP","DP","AP","GK"))
dev.print(device=pdf, file="val_pos.pdf")

Av=colMeans(subset(rbind(DMP,DDP,DAP,DGK),select=-c(preferred_foot,player_fifa_api_id)))
MPAv=colMeans(subset(DMP,select=-c(preferred_foot,player_fifa_api_id)))
DPAv=colMeans(subset(DDP,select=-c(preferred_foot,player_fifa_api_id)))
APAv=colMeans(subset(DAP,select=-c(preferred_foot,player_fifa_api_id)))
GKAv=colMeans(subset(DGK,select=-c(preferred_foot,player_fifa_api_id)))
par(mar =c(9,3,3,3))
plot(GKAv/Av,col='red',xaxt="n",xlab='',pch=19,cex=2)
axis(1,cex.axis=1,las = 2, at=1:ncol(subset(DMP,select=-c(preferred_foot,player_fifa_api_id))), labels=colnames(subset(DMP,select=-c(preferred_foot,player_fifa_api_id))))
points(DPAv/Av,col='blue',xaxt="n",xlab='',pch=19,cex=2)
points(APAv/Av,col='orange',xaxt="n",xlab='',pch=19, cex=2)
points(MPAv/Av,col='green',xaxt="n",xlab='',pch=19, cex=2)
legend("topleft", bty="n", pch=19, cex=1.5,
       col=c("red","blue","orange","green"),
       legend=c("Goal Keepers","Defensive Positions","Attacking Positions","Midfield Positions"))
dev.print(device=pdf, file="skill_pos.pdf")
dev.off()

#Linear Model
Linear_MP=lm(value_eur~.-player_fifa_api_id,data=DMP)
Linear_DP=lm(value_eur~.-player_fifa_api_id,data=DDP)
Linear_AP=lm(value_eur~.-player_fifa_api_id,data=DAP)
Linear_GK=lm(value_eur~.-player_fifa_api_id,data=DGK)
summary(Linear_MP)
summary(Linear_AP)
summary(Linear_DP)
summary(Linear_GK)

#Plot
PMP=predict(Linear_MP,newdata=DMP)
PDP=predict(Linear_DP,newdata=DDP)
PAP=predict(Linear_AP,newdata=DAP)
PGK=predict(Linear_GK,newdata=DGK)

par(mfrow=c(2,2))
plot(DMP$value_eur,PMP,main=c("Midfield Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DDP$value_eur,PDP,main=c("Defensive Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DAP$value_eur,PAP,main=c("Attacking Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DGK$value_eur,PGK,main=c("Goal Keepers"),ylab=c("Predictions"),xlab=c("Real Values"))
dev.print(device=pdf, file="least_square.pdf")

#R2
source("deviance.R") 
rsMP=R2(y=DMP$value_eur, pred=PMP) 
rsDP=R2(y=DDP$value_eur, pred=PDP)
rsAP=R2(y=DAP$value_eur, pred=PAP)
rsGK=R2(y=DGK$value_eur, pred=PGK)
rs=cbind(rsMP,rsDP,rsAP,rsGK)
colnames(rs)=c("MP","DP","AP","GK")
rownames(rs)=c("R2")
rs
xtable(rs)

#Log model
DMPl=DMP[-which(DMP$value_eur==0),]
DMPl$lval=log(DMPl$value_eur)
DDPl=DDP[-which(DDP$value_eur==0),]
DDPl$lval=log(DDPl$value_eur)
DAPl=DAP[-which(DAP$value_eur==0),]
DAPl$lval=log(DAPl$value_eur)
DGKl=DGK
DGKl$lval=log(DGKl$value_eur)

Log_MP=lm(lval~.-value_eur-player_fifa_api_id,data=DMPl)
Log_DP=lm(lval~.-value_eur-player_fifa_api_id,data=DDPl)
Log_AP=lm(lval~.-value_eur-player_fifa_api_id,data=DAPl)
Log_GK=lm(lval~.-value_eur-player_fifa_api_id,data=DGKl)

summary(Log_MP)
summary(Log_AP)
summary(Log_DP)
summary(Log_GK)

a=as.matrix(coef(Log_MP)[which(summary(Log_MP)$coefficients[,4]<=0.001)])
colnames(a)=c("MP-Coefficients")
logMP=a
xtable(logMP,type = "latex", file = "filename2.tex")

b=as.matrix(coef(Log_DP)[which(summary(Log_DP)$coefficients[,4]<=0.001)])
colnames(b)=c("DP-Coefficients")
logDP=b
xtable(logDP,type = "latex", file = "filename2.tex")

c=as.matrix(coef(Log_AP)[which(summary(Log_AP)$coefficients[,4]<=0.001)])
colnames(c)=c("AP-Coefficients")
logAP=c
xtable(logAP,type = "latex", file = "filename2.tex")

d=as.matrix(coef(Log_GK)[which(summary(Log_GK)$coefficients[,4]<=0.001)])
colnames(d)=c("GK-Coefficients")
logGK=d
xtable(logGK,type = "latex", file = "filename2.tex")

#Plot
PMP=predict(Log_MP,newdata=DMPl)
PDP=predict(Log_DP,newdata=DDPl)
PAP=predict(Log_AP,newdata=DAPl)
PGK=predict(Log_GK,newdata=DGKl)

par(mfrow=c(2,2))
plot(DMPl$lval,PMP,main=c("Midfield Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DDPl$lval,PDP,main=c("Defensive Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DAPl$lval,PAP,main=c("Attacking Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(DGKl$lval,PGK,main=c("Goal Keepers"),ylab=c("Predictions"),xlab=c("Real Values"))
dev.print(device=pdf, file="log.pdf")

#R2
source("deviance.R") 
rsMP=R2(y=DMPl$lval, pred=PMP) 
rsDP=R2(y=DDPl$lval, pred=PDP)
rsAP=R2(y=DAPl$lval, pred=PAP)
rsGK=R2(y=DGKl$lval, pred=PGK)
rslog=cbind(rsMP,rsDP,rsAP,rsGK)
colnames(rslog)=c("MP","DP","AP","GK")
rownames(rslog)=c("R2")
xtable(rslog)



#Lasso Model
#Prepare Dataset
lvalMP=DMPl$lval
DMP1=DMPl
DMP1$right_foot=ifelse(DMP1$preferred_foot == 'right', 1, 0)
DMP1=subset(DMPl,select=-c(value_eur,lval,player_fifa_api_id,preferred_foot))
DMP1=as.data.frame(scale(DMP1))
DMP1 <-sparse.model.matrix(~., data =DMP1 )[,-1]
lvalDP=DDPl$lval
DDP1=DDPl
DDP1$right_foot=ifelse(DDP1$preferred_foot == 'right', 1, 0)
DDP1=subset(DDPl,select=-c(value_eur,lval,player_fifa_api_id,preferred_foot))
DDP1=as.data.frame(scale(DDP1))
DDP1 <-sparse.model.matrix(~., data =DDP1 )[,-1]
lvalAP=DAPl$lval
DAP1=DAPl
DAP1$right_foot=ifelse(DAP1$preferred_foot == 'right', 1, 0)
DAP1=subset(DAPl,select=-c(value_eur,lval,player_fifa_api_id,preferred_foot))
DAP1=as.data.frame(scale(DAP1))
DAP1 <-sparse.model.matrix(~., data =DAP1 )[,-1]
lvalGK=DGKl$lval
DGK1=DGKl
DGK1$right_foot=ifelse(DGK1$preferred_foot == 'right', 1, 0)
DGK1=subset(DGKl,select=-c(value_eur,lval,player_fifa_api_id,preferred_foot))
DGK1=as.data.frame(scale(DGK1))
DGK1 <-sparse.model.matrix(~., data =DGK1 )[,-1]

#Lasso
par(mfrow=c(1,2))
lasso_MP<- gamlr(DMP1, y=lvalMP, lambda.min.ratio=1e-3)
ll=lasso_MP$lambda
AICc=log(ll[which.min(AICc(lasso_MP))]) #lambda AICc
AIC=log(ll[which.min(AIC(lasso_MP))])
BIC=log(ll[which.min(BIC(lasso_MP))])
cv.lassoMP=cv.gamlr(DMP1, y=lvalMP, lambda.min.ratio=1e-3,nfold=10)
CVmin=log(cv.lassoMP$lambda.min)
CV1se=log(cv.lassoMP$lambda.1se)
plot(lasso_MP, main=c("Lasso-MP"))
plot(cv.lassoMP,main=c("Cross Validation Lasso-MP"))
abline(v=AICc, col="black", lty=2)
abline(v=AIC, col="orange", lty=2)
abline(v=BIC, col="green", lty=2)
abline(v=CVmin, col="blue", lty=2)
abline(v=CV1se, col="purple", lty=2)
legend("topleft", bty="n", lwd=3, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
lasso_DP<- gamlr(DDP1, y=lvalDP, lambda.min.ratio=1e-3)
ll=lasso_DP$lambda
AICc=log(ll[which.min(AICc(lasso_DP))]) #lambda AICc
AIC=log(ll[which.min(AIC(lasso_DP))])
BIC=log(ll[which.min(BIC(lasso_DP))])
cv.lassoDP=cv.gamlr(DDP1, y=lvalDP, lambda.min.ratio=1e-3,nfold=10)
CVmin=log(cv.lassoDP$lambda.min)
CV1se=log(cv.lassoDP$lambda.1se)
plot(lasso_DP, main=c("Lasso-DP"))
plot(cv.lassoDP,main=c("Cross Validation Lasso-DP"))
abline(v=AICc, col="black", lty=2)
abline(v=AIC, col="orange", lty=2)
abline(v=BIC, col="green", lty=2)
abline(v=CVmin, col="blue", lty=2)
abline(v=CV1se, col="purple", lty=2)
legend("topleft", bty="n", lwd=3, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

lasso_AP<- gamlr(DAP1, y=lvalAP, lambda.min.ratio=1e-3)
ll=lasso_AP$lambda
AICc=log(ll[which.min(AICc(lasso_AP))]) #lambda AICc
AIC=log(ll[which.min(AIC(lasso_AP))])
BIC=log(ll[which.min(BIC(lasso_AP))])
cv.lassoAP=cv.gamlr(DAP1, y=lvalAP, lambda.min.ratio=1e-3,nfold=10)
CVmin=log(cv.lassoAP$lambda.min)
CV1se=log(cv.lassoAP$lambda.1se)
plot(lasso_AP, main=c("Lasso-AP"))
plot(cv.lassoAP,main=c("Cross Validation Lasso-AP"))
abline(v=AICc, col="black", lty=2)
abline(v=AIC, col="orange", lty=2)
abline(v=BIC, col="green", lty=2)
abline(v=CVmin, col="blue", lty=2)
abline(v=CV1se, col="purple", lty=2)
legend("topleft", bty="n", lwd=3, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

lasso_GK<- gamlr(DGK1, y=lvalGK, lambda.min.ratio=1e-3)
ll=lasso_GK$lambda
AICc=log(ll[which.min(AICc(lasso_GK))]) #lambda AICc
AIC=log(ll[which.min(AIC(lasso_GK))])
BIC=log(ll[which.min(BIC(lasso_GK))])
cv.lassoGK=cv.gamlr(DGK1, y=lvalGK, lambda.min.ratio=1e-3,nfold=10)
CVmin=log(cv.lassoGK$lambda.min)
CV1se=log(cv.lassoGK$lambda.1se)
plot(lasso_GK, main=c("Lasso-GK"))
plot(cv.lassoGK,main=c("Cross Validation Lasso-GK"))
abline(v=AICc, col="black", lty=2)
abline(v=AIC, col="orange", lty=2)
abline(v=BIC, col="green", lty=2)
abline(v=CVmin, col="blue", lty=2)
abline(v=CV1se, col="purple", lty=2)
legend("topleft", bty="n", lwd=3, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
dev.print(device=pdf, file="lasso.pdf")

#Difference predictions and real values
PMP=predict(cv.lassoMP, newdata=DMP1, select="min", type="response")
PDP=predict(cv.lassoDP, newdata=DDP1, select="min", type="response")
PAP=predict(cv.lassoAP, newdata=DAP1, select="min", type="response")
PGK=predict(cv.lassoGK, newdata=DGK1, select="min", type="response")

par(mfrow=c(2,2))
plot(PMP,DMPl$lval,main=c("Midfield Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(PDP,DDPl$lval,main=c("Defensive Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(PAP,DAPl$lval,main=c("Attacking Positions"),ylab=c("Predictions"),xlab=c("Real Values"))
plot(PGK,DGKl$lval,main=c("Goal Keepers"),ylab=c("Predictions"),xlab=c("Real Values"))
dev.print(device=pdf, file="lassopred.pdf")

#R2
source("deviance.R") 
rsMP=R2(y=DMPl$lval, pred=PMP) 
rsDP=R2(y=DDPl$lval, pred=PDP)
rsAP=R2(y=DAPl$lval, pred=PAP)
rsGK=R2(y=DGKl$lval, pred=PGK)
rslasso=cbind(rsMP,rsDP,rsAP,rsGK)
colnames(rslasso)=c("MP","DP","AP","GK")
rownames(rslasso)=c("R2")
xtable(rslasso)

#R2

#Coefficients (Standardized)
CoefMP=coef(cv.lassoMP, select="min")
index=order(CoefMP,decreasing=T)
CoefMP=as.matrix(CoefMP[index,])
CoefMP=as.matrix(CoefMP[2:10,])
xtable(CoefMP,type = "latex", file = "filename2.tex")

CoefDP=coef(cv.lassoDP, select="min")
CoefDP=CoefDP[order(CoefDP,decreasing=T),]
CoefDP=as.matrix(CoefDP[2:10])
xtable(CoefDP,type = "latex", file = "filename2.tex")


CoefAP=coef(cv.lassoAP, select="min")
index=order(CoefAP,decreasing=T)
CoefAP=as.matrix(CoefAP[index,])
CoefAP=as.matrix(CoefAP[2:10,])
xtable(CoefAP,type = "latex", file = "filename2.tex")

CoefGK=coef(cv.lassoGK, select="min")
CoefGK=CoefGK[order(CoefGK,decreasing=T),]
CoefGK=as.matrix(CoefGK[2:10])
xtable(CoefGK,type = "latex", file = "filename2.tex")

#Most undervalued players
#Midfield Positions
Pred=cbind(data.frame(as.matrix(PMP)),data.frame(DMPl))
colnames(Pred)[1]=c("lpred")
Pred$dif=Pred$lpred-Pred$lval
Pred=subset(Pred,select=c(player_fifa_api_id,value_eur,lval,lpred,dif))
Pred$Predvalue_eur=exp(Pred$lpred)            
Pred$dif_eur=Pred$Predvalue_eur-Pred$value_eur
names=subset(data5,select=c(sofifa_id,short_name))
colnames(names)=c("player_fifa_api_id","short_name")
Underval=merge(Pred,names,by='player_fifa_api_id')
Underval=Underval[order(Underval$dif_eur,decreasing=T),]
Underval=Underval[1:10,]
Underval=subset(Underval,select=c(short_name,value_eur,Predvalue_eur,dif_eur))
Underval$Percdif=100*(Underval$dif_eur)/(Underval$value_eur)
colnames(Underval)=c("Name","Value", "Prediction", "Diff.","% Diff.")
rownames(Underval)=Underval$Name
Underval=subset(Underval,select=-c(Name))
UndervalMP=Underval


#Defensive Positions
Pred=cbind(data.frame(as.matrix(PDP)),data.frame(DDPl))
colnames(Pred)[1]=c("lpred")
Pred$dif=Pred$lpred-Pred$lval
Pred=subset(Pred,select=c(player_fifa_api_id,value_eur,lval,lpred,dif))
Pred$Predvalue_eur=exp(Pred$lpred)            
Pred$dif_eur=Pred$Predvalue_eur-Pred$value_eur
names=subset(data5,select=c(sofifa_id,short_name))
colnames(names)=c("player_fifa_api_id","short_name")
Underval=merge(Pred,names,by='player_fifa_api_id')
Underval=Underval[order(Underval$dif_eur,decreasing=T),]
Underval=Underval[1:10,]
Underval=subset(Underval,select=c(short_name,value_eur,Predvalue_eur,dif_eur))
Underval$Percdif=100*(Underval$dif_eur)/(Underval$value_eur)
colnames(Underval)=c("Name","Value", "Prediction", "Diff.","% Diff.")
rownames(Underval)=Underval$Name
Underval=subset(Underval,select=-c(Name))
UndervalDP=Underval

#Attacking Positions
Pred=cbind(data.frame(as.matrix(PAP)),data.frame(DAPl))
colnames(Pred)[1]=c("lpred")
Pred$dif=Pred$lpred-Pred$lval
Pred=subset(Pred,select=c(player_fifa_api_id,value_eur,lval,lpred,dif))
Pred$Predvalue_eur=exp(Pred$lpred)            
Pred$dif_eur=Pred$Predvalue_eur-Pred$value_eur
names=subset(data5,select=c(sofifa_id,short_name))
colnames(names)=c("player_fifa_api_id","short_name")
Underval=merge(Pred,names,by='player_fifa_api_id')
Underval=Underval[order(Underval$dif_eur,decreasing=T),]
Underval=Underval[1:10,]
Underval=subset(Underval,select=c(short_name,value_eur,Predvalue_eur,dif_eur))
Underval$Percdif=100*(Underval$dif_eur)/(Underval$value_eur)
colnames(Underval)=c("Name","Value", "Prediction", "Diff.","% Diff.")
rownames(Underval)=Underval$Name
Underval=subset(Underval,select=-c(Name))
UndervalAP=Underval

#Goal Keepers
Pred=cbind(data.frame(as.matrix(PGK)),data.frame(DGKl))
colnames(Pred)[1]=c("lpred")
Pred$dif=Pred$lpred-Pred$lval
Pred=subset(Pred,select=c(player_fifa_api_id,value_eur,lval,lpred,dif))
Pred$Predvalue_eur=exp(Pred$lpred)            
Pred$dif_eur=Pred$Predvalue_eur-Pred$value_eur
names=subset(data5,select=c(sofifa_id,short_name))
colnames(names)=c("player_fifa_api_id","short_name")
Underval=merge(Pred,names,by='player_fifa_api_id')
Underval=Underval[order(Underval$dif_eur,decreasing=T),]
Underval=Underval[1:11,]
Underval=subset(Underval,select=c(short_name,value_eur,Predvalue_eur,dif_eur))
Underval$Percdif=100*(Underval$dif_eur)/(Underval$value_eur)
colnames(Underval)=c("Name","Value", "Prediction", "Diff.","% Diff.")
Underval=Underval[-10,]
rownames(Underval)=Underval$Name
Underval=subset(Underval,select=-c(Name))
UndervalGK=Underval

xtable(UndervalMP)
xtable(UndervalDP)
xtable(UndervalAP)
xtable(UndervalGK)
