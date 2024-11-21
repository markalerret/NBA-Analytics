#S670 Exploratory Data Analysis
#Final Project
#Group 12: Meghana Kantharaj, Mark Lerret, Mehul Sharma, Brian Trippi
#12/13/2017

library(readr)
nba_data <- read_csv("C:/Users/meghana/Desktop/Coursework/sem 3/eda/project/nba_data.csv")

metric = data.frame(nba_data$PER, nba_data$PIE, nba_data$EFF, nba_data$W_by_GP)
colnames(metric) = c("PER", "PIE", "EFF", "W_by_GP")

#Figure 1
cor(metric)

#Figure 2
summary_data = nba_data[,c(1,2,3,4,5,6,7,8,9,10,28, 32, 35)]
library(GGally)
summary(summary_data)
ggpairs(summary_data[,3:ncol(summary_data)])

library(ggplot2)
PER.cat = cut_number(summary_data$PER, n = 3)
ggpairs(data.frame(summary_data[, 6:ncol(summary_data)], PER.cat), aes(color = PER.cat))

WGP.cat = cut_number(summary_data$W_by_GP, n = 3)
ggpairs(data.frame(summary_data[, 6:ncol(summary_data)], WGP.cat), aes(color = WGP.cat))

#Figure 3
#boxplot with colors for teams with from increasing to decreasing mean pts PER
team_list = unique(factor(summary_data$TEAM))
team_rank=c()
for (id in 1:length(team_list))
{
  team_rank=c(team_rank, c(mean(summary_data[summary_data$TEAM==team_list[id],]$PER)))
}
team_pts_mean=data.frame(team_list,team_rank)
team_pts_mean = team_pts_mean[order(team_pts_mean$team_rank),]
team_list=factor(team_list, levels=team_pts_mean$team_list)
summary_data$TEAM=factor(summary_data$TEAM, levels = levels(team_list))
ggplot(summary_data, aes(x = TEAM, y = PER, fill=c("Blue"))) + geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)

#Figure 4
team_list = unique(factor(summary_data$TEAM))
team_rank=c()
for (id in 1:length(team_list))
{
  team_rank=c(team_rank, c(mean(summary_data[summary_data$TEAM==team_list[id],]$W_by_GP)))
}
team_pts_mean=data.frame(team_list,team_rank)
team_pts_mean = team_pts_mean[order(team_pts_mean$team_rank),]
team_list=factor(team_list, levels=team_pts_mean$team_list)
summary_data$TEAM=factor(summary_data$TEAM, levels = levels(team_list))
ggplot(summary_data, aes(x = TEAM, y = W_by_GP, fill=c("Blue"))) + geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE)

#Figure 5
teamrank <- read_csv("C:/Users/meghana/Desktop/Coursework/sem 3/eda/project/teamrank.csv")
ord = as.character(teamrank$abr)
teamrank$abr = factor(ord, levels=(ord))
colnames(teamrank)=c("rank", "team","abr","wgp","per")

ggplot(teamrank, aes(y=rank, x=abr, group=1)) + geom_point(aes(y =wgp, colour = "W_by_GP")) + geom_point(aes(y = per, colour = "PER"))+ geom_point(aes(y = rank, colour = "Actual")) +geom_line(aes(y =wgp, colour = "W_by_GP",size=1, alpha = 0.25)) +geom_line(aes(y = per, colour = "PER",size=1, alpha = 0.25)) + geom_line(aes(y = rank, colour = "Actual", size=1, alpha=0.25))
per_err=abs(teamrank$rank-teamrank$per)
wgp_err=abs(teamrank$rank-teamrank$wgp)
teamrank = data.frame(teamrank, wgp_err, per_err)

#Figure 6
library(reshape2)
new=data.frame(wgp_err,per_err)
newm=melt(new)
ggplot(newm) + geom_density(aes(x = value, y = ..density.., fill = variable, alpha=0.5)) + ggtitle("ERROR OF TEAM RANK- W_by_GP and PER")

ggplot(summary_data, aes(x = HEIGHT)) + stat_ecdf()
ggplot(summary_data, aes(x = WEIGHT)) + geom_density(adjust = 1, fill=3, alpha=0.25)

phy_WGP = data.frame(summary_data$AGE, summary_data$HEIGHT, summary_data$WEIGHT, summary_data$W_by_GP)
colnames(phy_WGP) = c("AGE", "HEIGHT", "WEIGHT", "WGP")
WPA.cat = cut_number(summary_data$W_by_GP, n = 3)
ggpairs(data.frame(phy_WGP[, 1:ncol(phy_WGP)], WGP.cat), aes(color = WGP.cat))

#Loading the library
library(readr)
library(corrplot)
library(randomForest)
library(MASS)
library(hydroGOF)
library(e1071)
source("http://www.sthda.com/upload/rquery_cormat.r")
library(caret)
library(ggfortify)
library(ggplot2)

#Loading the File
NBA <- read_csv("C:/Users/Mehul Sharma/Downloads/nba_data.csv")
NBA$W_by_GP <- NBA$W_by_GP*100

#Running Correlation
rquery.cormat(NBA[,3:35])

#Removing FGA, FTA, FP, DREB. PIE, W, and 3PA after checking correlation

NBA.U <- NBA[,!names(NBA) %in% c("FGA","FTA","FP","3PA","DREB", "PLAYER", "TEAM","PIE", "EFF", "W" )]
names(NBA.U) <- make.names(names(NBA.U))

#Sampling of the dataset
smp_size <- floor(0.75 * nrow(NBA.U))

#Creating test and train data
set.seed(123)
train_ind <- sample(seq_len(nrow(NBA.U)), size = smp_size)

train <- NBA.U[train_ind, ]
test <- NBA.U[-train_ind, ]

#Figure 7
#Running Random Forest
RF.NBA=randomForest(W_by_GP ~ . , data = train, importance=TRUE, ntree=150)
RFPrediction <- predict(RF.NBA, test)
RFPredVals <- data.frame(WinPercent = test$W_by_GP, PredictedWinPercent = RFPrediction)

#Understanding the variable importance
str(RF.NBA)
str(RF.NBA$importance)
data <- as.data.frame(cbind(rownames(RF.NBA$importance),round(RF.NBA$importance[,"IncNodePurity"],1)))
colnames(data) <- c("Parameters","IncNodePurity")
data$IncNodePurity <- as.numeric(as.character(data$IncNodePurity))
(var.Imp.Plot <- ggplot(data) + geom_point(aes(IncNodePurity,Parameters)))

#Plotting Predicted and True Values based on RF
ggplot(data=RFPredVals, aes(x = WinPercent, y = PredictedWinPercent)) + geom_point(color = 'red') + geom_smooth()
print(RF.NBA)

#Calculating RMSE based on RF
RF.RMSE=rmse(RFPrediction,test$W_by_GP)

#Figure 8
#Running SVM Regression
SVMmodel <- svm(W_by_GP ~ ., data = train)
SVMPrediction <- predict(SVMmodel, test)
SVMPredVals <- data.frame(WinPercent = test$W_by_GP, PredictedWinPercent = SVMPrediction)

#Plotting Predicted and True Values based on SVM
ggplot(data=SVMPredVals, aes(x = WinPercent, y = PredictedWinPercent)) + geom_point(color = 'red') + geom_smooth()

#Calculating RMSE based on SVM
SVM.RMSE=rmse(SVMPrediction,test$W_by_GP)

#Figure 10, 12
#Running Linear Regression 
LM.NBA <- lm(W_by_GP~.,data=train)
LMPrediction <- predict(LM.NBA, test)
LMPredVals <- data.frame(WinPercent = test$W_by_GP, PredictedWinPercent = LMPrediction)

#Plotting Predicted and True Values based on Linear Regression
ggplot(data=LMPredVals, aes(x = WinPercent, y = PredictedWinPercent)) + geom_point(color = 'red') + geom_smooth()

#Calculating RMSE based on LM
LM.RMSE=rmse(LMPrediction,test$W_by_GP)

#Figure 11
#Running Residuals vs Fitted and Normal Q-Q based on the Linear Regression Model
autoplot(LM.NBA, label.size = 3, colour = 'dodgerblue3', smooth.colour = 'black')

#Figure 9
#Running XGBoost
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
XG.NBA <- train(W_by_GP ~ ., data = train, method = "xgbLinear", trControl = TrainControl,verbose = FALSE)
XGPrediction<- predict(XG.NBA, test)
XGPredVals <- data.frame(WinPercent = test$W_by_GP, PredictedWinPercent = XGPrediction)

#Plotting Predicted and True Values based on XGBoost
ggplot(data=XGPredVals, aes(x = WinPercent, y = PredictedWinPercent)) + geom_point(color = 'red') + geom_smooth()

#Calculating RMSE based on XGBoost
XG.RMSE=rmse(XGPrediction,test$W_by_GP)

#load library
library("ggplot2", lib.loc="~/R/win-library/3.4")
library(broom)
library(tidyr)

#load dataset
nba_data <-  read_csv("C:/Users/mark/Downloads/nba_data (1).csv")

#points vs. minutes
#scatterplot
pts.gg =ggplot(nba_data,aes(x =MIN,y =PTS)) +geom_point()
pts.gg

#loess curve
pts.gg + geom_smooth()

#linear model
pts.lm =lm(nba_data$MIN ~ nba_data$PTS,data =nba_data)
pts.lm.df <- augment(pts.lm)
pts.gg + geom_smooth(method = "lm")

cor(nba_data$PTS, nba_data$MIN)
#0.87

#data frame
pts.lm =lm(PTS ~ MIN,data =nba_data)
pts.lm.df =augment(pts.lm)
summary(pts.lm.df)

#residual plot
ggplot(pts.lm.df,aes(x =MIN,y =.resid)) +geom_point() +geom_smooth() 
+geom_abline(slope =0,intercept =0)

#qq plot
ggplot(pts.lm.df, aes(sample = .resid)) + stat_qq()

#homoscedacity
ggplot(pts.lm.df,aes(x =.fitted,y =sqrt(abs(.resid)))) +geom_point() +geom_smooth()

#residual fitted plot
n =nrow(ppm.lm.df)
f.value = (0.5:(n -0.5))/n
ppm.fit =data.frame(f.value,Fitted =sort(ppm.lm.df$.fitted) -mean(ppm.lm.df$.fitted),
                    Residuals =sort(ppm.lm.df$.resid))
ppm.fit.long = ppm.fit %>%gather(type, value, Fitted:Residuals)
ggplot(ppm.fit.long,aes(x =f.value,y =value)) +geom_point() +facet_wrap(~type)

#power transformations
points = nba_data$PTS
minutes = nba_data$MIN
points =length(minutes)
power =rep(seq(-1,1,0.25),each =points)
minutes =c(minutes^-1, minutes^-0.75, minutes^-0.5, minutes^-0.25,log(minutes),minutes^0.25, minutes^0.5, minutes^0.75, minutes)
ggplot(data.frame(power, minutes),aes(sample =minutes)) +stat_qq() +facet_wrap(~power,scales ="free")


#Figure 13 Part 1
#points vs. height
#scatterplot
ph.gg =ggplot(nba_data,aes(x =HEIGHT,y =PTS)) +geom_point()
ph.gg

#loess curve
ph.lc = ph.gg + geom_smooth()
ph.lc
summary(ph.lc)

#linear method
ph.gg + geom_smooth(method = "lm")
#correlation
cor(nba_data$PTS, nba_data$HEIGHT)
#-0.08

#data frame
ph.lm =lm(PTS ~ HEIGHT,data =nba_data)
ph.lm.df =augment(ph.lm)
summary(ph.lm.df)

#residual plot
ggplot(ph.lm.df,aes(x =HEIGHT,y =.resid)) +geom_point() +geom_smooth() 
+geom_abline(slope =0,intercept =0)

#qq plot
ggplot(ph.lm.df, aes(sample = .resid)) + stat_qq()

#homoscedacity
ggplot(ph.lm.df,aes(x =.fitted,y =sqrt(abs(.resid)))) +geom_point() +geom_smooth()

#Figure 14 Part 1
#residual fitted plot
n =nrow(ph.lm.df)
f.value = (0.5:(n -0.5))/n
ph.fit =data.frame(f.value,Fitted =sort(ph.lm.df$.fitted) -mean(ph.lm.df$.fitted),
                   Residuals =sort(ph.lm.df$.resid))
ph.fit.long = ph.fit %>%gather(type, value, Fitted:Residuals)
ggplot(ph.fit.long,aes(x =f.value,y =value)) +geom_point() +facet_wrap(~type)

#power transformations
points = nba_data$PTS
height = nba_data$HEIGHT
points =length(height)
power =rep(seq(-1,1,0.25),each =points)
height =c(minutes^-1, height^-0.75, height^-0.5, height^-0.25,log(height),height^0.25, height^0.5, height^0.75, height)
ggplot(data.frame(power, height),aes(sample =height)) +stat_qq() +facet_wrap(~power,scales ="free")

#Figure 13 Part 2
#points vs. weight
#scatterplot
ptsw.gg =ggplot(nba_data,aes(x =WEIGHT,y =PTS)) +geom_point()
ptsw.gg

#loess curve
ptsw.lc = ptsw.gg + geom_smooth()
ptsw.lc
summary(ptsw.lc)

#linear method
ptsw.gg + geom_smooth(method = "lm")
#correlation
cor(nba_data$PTS, nba_data$WEIGHT)
#-0.007

#data frame
ptsw.lm =lm(PTS ~ WEIGHT,data =nba_data)
ptsw.lm.df =augment(ptsw.lm)
summary(ptsw.lm.df)

#residual plot
ggplot(ptsw.lm.df,aes(x =WEIGHT,y =.resid)) +geom_point() +geom_smooth() 
+geom_abline(slope =0,intercept =0)

#homoscedacity
ggplot(ptsw.lm.df,aes(x =.fitted,y =sqrt(abs(.resid)))) +geom_point() +geom_smooth()

#Figure 14 Part 2
#residual fitted plot
n =nrow(ptsw.lm.df)
f.value = (0.5:(n -0.5))/n
ptsw.fit =data.frame(f.value,Fitted =sort(ptsw.lm.df$.fitted) -mean(ptsw.lm.df$.fitted),
                     Residuals =sort(ptsw.lm.df$.resid))
ptsw.fit.long = ptsw.fit %>%gather(type, value, Fitted:Residuals)
ggplot(ptsw.fit.long,aes(x =f.value,y =value)) +geom_point() +facet_wrap(~type)

#Figure 15 Part 1
#rebounds vs. height
#scatterplot
rbsh.gg =ggplot(nba_data,aes(x =HEIGHT,y =REB)) +geom_point()
rbsh.gg

#loess curve
rbsh.lc = rbsh.gg + geom_smooth()
rbsh.lc
summary(rbsh.lc)

#linear method
rbsh.gg + geom_smooth(method = "lm")
#correlation
cor(nba_data$REB, nba_data$HEIGHT)
#0.44

#data frame
rbsh.lm =lm(REB ~ HEIGHT,data =nba_data)
rbsh.lm.df =augment(rbsh.lm)
summary(rbsh.lm.df)

#residual plot
ggplot(rbsh.lm.df,aes(x =HEIGHT,y =.resid)) +geom_point() +geom_smooth() 
+geom_abline(slope =0,intercept =0)

#homoscedacity
ggplot(rbsh.lm.df,aes(x =.fitted,y =sqrt(abs(.resid)))) +geom_point() +geom_smooth()

#Figure 16 Part 1
#residual fitted plot
n =nrow(rbsh.lm.df)
f.value = (0.5:(n -0.5))/n
rbsh.fit =data.frame(f.value,Fitted =sort(rbsh.lm.df$.fitted) -mean(rbsh.lm.df$.fitted),
                     Residuals =sort(rbsh.lm.df$.resid))
rbsh.fit.long = rbsh.fit %>%gather(type, value, Fitted:Residuals)
ggplot(rbsh.fit.long,aes(x =f.value,y =value)) +geom_point() +facet_wrap(~type)

#####
#Figure 15 Part 2
#rebounds vs. weight
#scatterplot
rbsw.gg =ggplot(nba_data,aes(x =WEIGHT,y =REB)) +geom_point()
rbsw.gg

#loess curve
rbsw.lc = rbsh.gg + geom_smooth()
rbsw.lc
summary(rbsh.lc)

#linear method
rbsw.gg + geom_smooth(method = "lm")
#correlation
cor(nba_data$REB, nba_data$WEIGHT)
#0.48

#data frame
rbsw.lm =lm(REB ~ WEIGHT,data =nba_data)
rbsw.lm.df =augment(rbsw.lm)
summary(rbsw.lm.df)

#residual plot
ggplot(rbsw.lm.df,aes(x =WEIGHT,y =.resid)) +geom_point() +geom_smooth() 
+geom_abline(slope =0,intercept =0)

#homoscedacity
ggplot(rbsw.lm.df,aes(x =.fitted,y =sqrt(abs(.resid)))) +geom_point() +geom_smooth()

#Figre 16 Part 2
#residual fitted plot
n =nrow(rbsw.lm.df)
f.value = (0.5:(n -0.5))/n
rbsw.fit =data.frame(f.value,Fitted =sort(rbsw.lm.df$.fitted) -mean(rbsw.lm.df$.fitted),
                     Residuals =sort(rbsw.lm.df$.resid))
rbsw.fit.long = rbsw.fit %>%gather(type, value, Fitted:Residuals)
ggplot(rbsw.fit.long,aes(x =f.value,y =value)) +geom_point() +facet_wrap(~type)
