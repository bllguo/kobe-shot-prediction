library(calibrate)
library(dplyr)
library(ggplot2)

rm(list=ls())
setwd("C:/Users/bllguo/Dropbox/penn_spring_15-16/STAT471/final")

data = read.csv("data.csv")
test = data[which(is.na(data$shot_made_flag)), ]
train = data[which(!is.na(data$shot_made_flag)), ]

names(train)
str(train)

###############################################################################################################
########################################     data exploration     #############################################
###############################################################################################################

attach(train)
# overall accuracy (in the training data at least)
summary(shot_made_flag)[2] / (summary(shot_made_flag)[2] + summary(shot_made_flag)[1])
# 44.61%

# type of shots made
summary(action_type)
# first is jump shots, with 15836, then layup shots, with 2154, then driving layup shots, with 1628
# lots of jump shots but many other shots have very few occurrences
# what about accuracy by shot type?
accByShotType = summary(action_type)
for(i in 1:nlevels(action_type)) {
  accByShotType[i] = length(which(train[,15]==1 & train[,1]==levels(action_type)[i])) / accByShotType[i]
}
accByShotType = sort(accByShotType)
abst = data.frame(accByShotType)
names(abst) = c("acc")
accByShotType

ggplot(abst, aes(x = as.factor(acc), y = 1)) +
  geom_point(aes(y = acc), size = 3, color = " dark blue", stat = "identity") +
    coord_flip() + scale_x_discrete(labels=rownames(abst))

# Courtesy of Alexandru Papiu, Kaggler - visualization of court locations.
# Helpful for a non-basketball watcher/player like me
courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    ggtitle(paste(feat))
}
courtplot(shot_zone_area)
courtplot(shot_zone_basic) 
courtplot(shot_zone_range)

# range analysis
accByRange = vector()
numByRange = vector()
for(i in 1:nlevels(shot_zone_range)) {
  accByRange = c(accByRange, mean(train[which(train$shot_zone_range == levels(shot_zone_range)[i]) ,]$shot_made_flag))
  numByRange = c(numByRange, length(which(train$shot_zone_range == levels(shot_zone_range)[i])))
}
names(accByRange) = levels(shot_zone_range)
names(numByRange) = levels(shot_zone_range)
numByRange = numByRange / sum(numByRange)
plot(accByRange, numByRange)
text(accByRange[c(1:3, 5)], numByRange[c(1:3, 5)], labels = names(accByRange)[c(1:3, 5)], xpd=TRUE)
text(accByRange[4], numByRange[4], labels = names(accByRange)[4], adj=0, xpd=TRUE)
accByRange
# unsurprising results - farther away, more likely to miss
# frequencies also displayed

detach(train)

# location analysis
# Credit again to Alexandru
# Visualize Kobe's shots!
train$shot_made_flag <- as.factor(train$shot_made_flag)
ggplot(train, aes(x = loc_x, y = loc_y)) +
  geom_point(aes(color = shot_made_flag), alpha = 0.5, size = 0.5) +
  ylim(c(-50, 400)) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ shot_made_flag) +
  labs(title = "Shots Made(Blue) vs. Shots Missed(Red)")

# General accuracy and numshot plot functions
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))
accuplot <- function(y) {
  y <- substitute(y)
  ggplot(data = train, aes_q(x = y)) +
    geom_bar(aes(fill = shot_made_flag), position = "fill") +
    labs(x = y) + labs(y = "accuracy") + labs(title = paste("Accuracy by",y))
}

numshotplot <- function(y) {
  y <- substitute(y)
  ggplot(data = train, aes_q(x = y)) +
    geom_bar(aes(fill = shot_made_flag), position = "fill") +
    labs(x = y) + labs(y = "# shots") + labs(title = paste("# shots by",y)) +
    geom_bar() + guides(fill=FALSE)
}
# time
accuplot(minutes_remaining)
# consistent, but notably lower when less time remains
numshotplot(minutes_remaining)
# Makes a lot more shots as game approaches end - time to warmup, I suppose?
# Further evidence for pressure/choking at the end - shoots more shots yet still lower accuracy
accuplot(seconds_remaining)
numshotplot(seconds_remaining)
# same story
# time does not seem to vary enough

# By period (over 4 must be overtime?)
accuplot(period)
# very little variation
numshotplot(period)
# yes, overtime

# season
accuplot(season)
# consistent performance from the start - no standout seasons
# declining performance at the end ( :( )
numshotplot(season)
# surprising amount of variation in number of shots made by season!
# still season does not seem useful in classification

# opponent
accuplot(opponent)
# consistent performance
numshotplot(opponent)
# likely the peaks are all West teams
# seems to not be useful classification predictor

accuplot(shot_zone_area)
numshotplot(shot_zone_area)

accuplot(shot_zone_basic)
numshotplot(shot_zone_basic)

accuplot(shot_zone_range)
numshotplot(shot_zone_range)

accuplot(shot_distance)
numshotplot(shot_distance)

accuplot(shot_type)
numshotplot(shot_type)

accuplot(combined_shot_type)
numshotplot(combined_shot_type)

###############################################################################################################
#########################################     write final datasets     ########################################
###############################################################################################################
data = read.csv("data.csv")
test = data[which(is.na(data$shot_made_flag)), ]
train = data[which(!is.na(data$shot_made_flag)), ]

train$shot_made_flag <- as.factor(train$shot_made_flag)
train$game_event_id = NULL
train$game_id = NULL
train$team_id = NULL # was always on the Lakers
train$team_name = NULL
train$matchup = NULL # opponent contains what we want

test$shot_made_flag <- as.factor(test$shot_made_flag)
test$game_event_id = NULL
test$game_id = NULL
test$team_id = NULL 
test$team_name = NULL
test$matchup = NULL 

train$game_date = NULL
test$game_date = NULL

write.csv(train, file = "train.csv")
write.csv(test, file = "test.csv")

traincopy = train
testcopy = test

# Dealing with leakage - not covered. I will attempt to tackle that afterwards.
# For now ignore both to build model

###############################################################################################################
###########################################     random forest     #############################################
###############################################################################################################

library(randomForest)
library(pROC)

train$shot_id = NULL
test$shot_id = NULL

#fit.rf = randomForest(shot_made_flag~., train, mtry=4, ntree=500) 
#plot(fit.rf)
# ntree=300 good enough
#fit.rf.pred=predict(fit.rf, type="prob")  # output the prob of "0" and "1"
#fit.rf.pred.y=predict(fit.rf, type="response")
#mean(train$shot_made_flag != fit.rf.pred.y)

# tune mtry
rf.mce=vector()
for (i in 1:dim(train)[2]) {
  rf.fit=randomForest(shot_made_flag~., train, mtry=i, ntree=250)
  rf.mce[i]=rf.fit$err.rate[rf.fit$ntree, 1]
}
plot(rf.mce)
# lowest MCE at mtry = 1 (!?)
# let's try 1, 2, 3

fit.rf1 = randomForest(shot_made_flag~., train, mtry=1, ntree=250)
fit.rf.pred1 = predict(fit.rf1, type="prob")
roc(train$shot_made_flag, fit.rf.pred1[,2], plot=TRUE)
# fit with mtry = 1 has best AUC but they all differ by very little

# training MCE not good at all
fit.rf1$err.rate[fit.rf1$ntree, 1]

# apply fit.rf1 to test set
predict.rf=predict(fit.rf1, newdata=test)
# predicted accuracy in test set
summary(predict.rf)[2]/5000

# variable importance
varImpPlot(fit.rf1, main="Random Forest Feature Importance")
imp <- importance(fit.rf1, type=2)
as.matrix(imp[order(-imp), ])
# VERY odd results. From above we see that Kobe has consistent performance across opponents!
# All in all, random forests do not seem to be a good classification method here

##############################################################################################################

redtrain = data.frame(train$shot_made_flag, train$action_type, train$shot_zone_area, train$shot_distance)
names(redtrain) = c("shot_made_flag", "action_type", "shot_zone_area", "shot_distance")
redtest = data.frame(test$shot_made_flag, test$action_type, test$shot_zone_area, test$shot_distance)
names(redtest) = c("shot_made_flag", "action_type", "shot_zone_area", "shot_distance")

rf.mce.2=vector()
for (i in 1:dim(redtrain)[2]) {
  rf.fit=randomForest(shot_made_flag~., redtrain, mtry=i, ntree=250)
  rf.mce.2[i]=rf.fit$err.rate[rf.fit$ntree, 1]
}
plot(rf.mce.2)
fit2.rf1 = randomForest(shot_made_flag~., redtrain)
fit2.rf.pred1 = predict(fit2.rf1, type="prob")
roc(train$shot_made_flag, fit2.rf.pred1[,2], plot=TRUE)
fit2.rf1$err.rate[fit2.rf1$ntree, 1]

predict.rf2=predict(fit2.rf1, newdata=redtest)
summary(predict.rf2)[2]/5000

varImpPlot(fit2.rf1, main="Random Forest Feature Importance")

###############################################################################################################
########################################     logistic classifier     ##########################################
###############################################################################################################

library(glmnet)
library(car)
library(leaps)

X = model.matrix(shot_made_flag~., train)[,-12]
Y = train$shot_made_flag
fit.cv.dev=cv.glmnet(X, Y, alpha=1, family="binomial", nfolds = 10, type.measure = "deviance")
plot(fit.cv.dev)
coef.min.dev=coef(fit.cv.dev, s="lambda.1se")
coef.min.dev[which(coef.min.dev!=0), ]
# cv selects action_type, combined_shot_type, minutes_remaining, period, season, seconds_remaining, shot_zone_area, 
# shot_zone_basic, shot_Zone_range, opponent

fit.dev = glm(shot_made_flag ~ action_type + combined_shot_type +
                minutes_remaining + period + season + seconds_remaining + 
                shot_zone_area + shot_zone_basic + shot_zone_range + opponent, train, family="binomial")
Anova(fit.dev)
# need to get rid of combined_shot_type

fit.dev = glm(shot_made_flag ~ action_type +
                minutes_remaining + period + season + seconds_remaining + 
                shot_zone_area + shot_zone_basic + shot_zone_range + opponent, train, family="binomial")
Anova(fit.dev)
# opponent

fit.dev = glm(shot_made_flag ~ action_type +
                minutes_remaining + period + season + seconds_remaining + 
                shot_zone_area + shot_zone_basic + shot_zone_range, train, family="binomial")
Anova(fit.dev)

# all significant
# however, shot_distance and shot_zone_basic are obviously collinear
# same with seconds and minutes!
# remove minutes and basic (higher p value)
fit.dev = glm(shot_made_flag ~ action_type + period + season + seconds_remaining + 
                shot_zone_area + shot_zone_range, train, family="binomial")
Anova(fit.dev)
fit.dev.roc=roc(train$shot_made_flag, fit.dev$fitted, plot=T, col="blue")  
fit.dev.roc$auc
# .701

fit.dev.pred=rep("0", 25697)
fit.dev.pred[fit.dev$fitted > .5]="1" 
fit.dev.pred = as.factor(fit.dev.pred)
cm = table(fit.dev.pred, train$shot_made_flag)
mce=(cm[1,2]+cm[2,1])/length(fit.dev.pred)
mce
# poor performance with naive .5 threshold
# suppose we use Kobe's actual accuracy as the threshold
summary(shot_made_flag)[[2]] / (summary(shot_made_flag)[[1]] + summary(shot_made_flag)[[2]])
# 44.61% accuracy
# cost of misclassifying a made shot as a missed shot is higher than the alternative
a10 = 1-summary(shot_made_flag)[[2]] / (summary(shot_made_flag)[[1]] + summary(shot_made_flag)[[2]])
a01 = summary(shot_made_flag)[[2]] / (summary(shot_made_flag)[[1]] + summary(shot_made_flag)[[2]])
threshold = (a01/a10)/(1 + (a01/a10))
threshold
# threshold is just the accuracy
fit.dev.pred.bayes=rep("0", 25697)
fit.dev.pred.bayes[fit.dev$fitted > threshold] = "1" 
MCE.bayes=(sum(a10*(fit.dev.pred.bayes[train$shot_made_flag == "1"] != "1")) 
           + sum(a01*(fit.dev.pred.bayes[train$shot_made_flag == "0"] != "0")))/length(train$shot_made_flag)
MCE.bayes
# much improved training performance, comparable to random forest! 
# this model also provides much more explanatory power and is more interpretable

# get test predictions
fit.dev.test = predict(fit.dev, test, type="response")
fit.dev.pred.test=rep(0, 5000)
fit.dev.pred.test[fit.dev.test > threshold] = 1
sum(fit.dev.pred.test)/5000
# predicted accuracy in test set is INCREDIBLY different from random forest!

###############################################################################################################
# proposed model

fit.dev2 = glm(shot_made_flag ~ action_type + shot_distance + 
                shot_zone_area, train, family="binomial")
Anova(fit.dev2)
fit.dev2.roc=roc(train$shot_made_flag, fit.dev2$fitted, plot=T, col="blue")  
fit.dev2.roc$auc

fit.dev2.pred=rep("0", 25697)
fit.dev2.pred[fit.dev2$fitted > .5]="1" 
fit.dev2.pred = as.factor(fit.dev2.pred)
cm2 = table(fit.dev2.pred, train$shot_made_flag)
mce2=(cm2[1,2]+cm2[2,1])/length(fit.dev2.pred)
mce2

fit.dev.pred2.bayes=rep("0", 25697)
fit.dev.pred2.bayes[fit.dev2$fitted > threshold] = "1" 
MCE.bayes2=(sum(a10*(fit.dev.pred2.bayes[train$shot_made_flag == "1"] != "1")) 
           + sum(a01*(fit.dev.pred2.bayes[train$shot_made_flag == "0"] != "0")))/length(train$shot_made_flag)
MCE.bayes2

fit.dev.test2 = predict(fit.dev2, test, type="response")
fit.dev.pred.test2=rep(0, 5000)
fit.dev.pred.test2[fit.dev.test2 > threshold] = 1
sum(fit.dev.pred.test2)/5000

# finally, deal with leakage
###############################################################################################################
###############################################     final     #################################################
###############################################################################################################
# we want to avoid leaking of information from the future into the past
# should not be able to use future shots to classify a shot in the past!
# but how can we do this?
# naive algorithm - train 5000 models, each only on shots occurring before it
# will clearly take a massive amount of time!

# random forest method takes too long so it will not be performed

# using our fit.dev model
# no prior information for shot 1; let's just assume it was made
pred.test=rep(0, 5000)
pred.test[1] = 1
for(i in 2:5000) {
  temp = traincopy[which(traincopy$shot_id < testcopy$shot_id[i]), ]
  topredict = testcopy[which(testcopy$shot_id == testcopy$shot_id[i]), ]
  if(testcopy$shot_id[i] > 1511) {
    fit.dev.temp = glm(shot_made_flag ~ action_type + period + season + seconds_remaining + 
                                   shot_zone_area + shot_zone_range, train, family="binomial")
  }
  else {
    fit.dev.temp = glm(shot_made_flag ~ action_type + period + season + seconds_remaining + 
                         shot_zone_area + shot_zone_range, train, family="binomial")
  }
  if (predict(fit.dev.temp, topredict, type="response") > threshold) {
    pred.test[i] = 1
  }
}


