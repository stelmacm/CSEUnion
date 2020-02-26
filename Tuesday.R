library(tidyverse)
library(readr)
library(ggplot2)

## load the data
policies <- readr :: read_csv("https://raw.githubusercontent.com/stelmacm/CSEUnion/master/information/AFC_policies.csv")
claims <- readr :: read_csv("https://raw.githubusercontent.com/stelmacm/CSEUnion/master/information/AFC_claims.csv")

## load the dplyr library that will help us with data wrangling 
## R Packages can be installed with the command install.packages("dplyr")
library(dplyr)

## combine the policies and claims data sets
policies_with_clams <- policies %>% left_join(claims) %>% group_by(BusinessID, BusinessType, Province, Latitude, Longitude, City, AmountOfInsurance, Earnings) %>% summarize(TotalIncurred=sum(dplyr::coalesce(ClaimAmount,0)))
policies_with_clams$lossCost <- policies_with_clams$TotalIncurred

## have a quick look at the result
head(policies_with_clams)
nrow(policies_with_clams) == nrow(policies) ## TRUE : Ok

Toronto <- policies_with_clams[policies_with_clams$City == 'TORONTO',]

## Now we will try to model our loss cost
myFormula <- (lossCost ~ as.factor(BusinessType) + Earnings + AmountOfInsurance) ## let's start with this random model...
myFormula_city <- (lossCost ~ as.factor(BusinessType) + Earnings + Latitude + Longitude + AmountOfInsurance) ## let's start with this random model...



library(statmod) ## the statmod library will allow us to use a tweedie distribution assumption
myModel <- glm(myFormula, data=policies_with_clams, family=tweedie(var.power=1.5, link.power=0))
myModel_city <- glm(myFormula_city, data=policies_with_clams,family=tweedie(var.power=1.5, link.power=0))



summary(myModel)

## We can now apply our model to predict the loss costs
policies_with_clams$prediction <- predict(myModel,policies_with_clams,type="response")
policies_with_clams$prediction_city <- predict(myModel_city,policies_with_clams,type="response")



library(Metrics)
mse(Toronto$prediction, Toronto$lossCost)
mse(Toronto$prediction_city, Toronto$lossCost)
mse(policies_with_clams$prediction_city, policies_with_clams$lossCost)
mse(policies_with_clams$prediction, policies_with_clams$lossCost)

fixedExpensesPerPolicy <- policies_with_clams$prediction*0.1
variableExpensesPerPolicy <- 0.275
LAE <- 0.1 
profitLoading <- 0.05 ## sky is the limit

lossCostTrend <- 1.02

policies_with_clams$premium <- (lossCostTrend*(1+LAE)*policies_with_clams$prediction + fixedExpensesPerPolicy)/(1-variableExpensesPerPolicy-profitLoading)

template <- read.csv('templateForYourPremiums.csv')
dim(template) #250k rows : Ok
template <- template %>% inner_join(policies_with_clams[,c('BusinessID', 'premium')])
template$ProposedPremium <- template$premium
template$premium <- NULL ## I don't need this column anymore
head(template)
dim(template) #250k rows : Ok
sum(is.na(template$premium)) # 0 : Ok
write.csv(template, "templateToUpload.csv", row.names=FALSE)









# Data visualization:
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf(aes(fill = pop_est),color = "black", fill = "gray") + 
  geom_point(data=policies_with_clams,aes(Longitude,Latitude,colour=Province), size = 0.5) +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Longitude") + ylab("Latitude") +
  xlim(-100,-60) +
  ylim(41,58)

ggplot(data = world) + 
  geom_sf(aes(fill = pop_est),color = "black", fill = "gray") + 
  geom_point(data=policies_with_clams,aes(Longitude,Latitude,colour=Province, size=lossCost)) +
  scale_colour_brewer(palette = "Dark2") +
  xlab("Longitude") + ylab("Latitude") +
  xlim(-100,-60) +
  ylim(41,58)









summary(policies_with_clams)

# Some data cleaning:
policies_with_clams$BusinessType[which(policies_with_clams$BusinessType == "chiropractors' offices")] <- "chiropractors offices"
policies_with_clams$BusinessType <- gsub('-', '', policies_with_clams$BusinessType)
policies_with_clams$BusinessType <- gsub(' ', '_', policies_with_clams$BusinessType)

# factoring the categorical variables: 
policies_with_clams$BusinessType <- as.factor(policies_with_clams$BusinessType)
policies_with_clams$City <- as.factor(policies_with_clams$City)
policies_with_clams$Province <- as.factor(policies_with_clams$Province)
levels(policies_with_clams$BusinessType)
nlevels(policies_with_clams$BusinessType)
levels(policies_with_clams$Province)
levels(policies_with_clams$City)
nlevels(policies_with_clams$City)


x <- policies_with_clams[,]
y <- data.frame(policies_with_clams$lossCost)


# We have 6544 levels for cities and 32 levels for bussiness types and 2 levels for province. We need to convert these categorical variables into 
# dummy variables. In dummy coding, one of the categories becomes the reference level and all others will be compared to it. Note that city will
# not be included in analysis because of the high number of levels.
# Making dummy variables: the first level is the reference level.
for (j in 2:nlevels(x$Province)){
  x[,levels(x$Province)[j]] = rep(0,dim(x)[1])
}
for (j in 2:nlevels(x$Province)){
  x[which(x$Province == levels(x$Province)[j]),levels(x$Province)[j]] = 1
}

for (j in 2:nlevels(x$BusinessType)){
  x[,levels(x$BusinessType)[j]] = rep(0,dim(x)[1])
}
for (j in 2:nlevels(x$BusinessType)){
  x[which(x$BusinessType == levels(x$BusinessType)[j]),levels(x$BusinessType)[j]] = 1
}


x <- x[,-c(1,2,3,6,8,9,10)]


# normalizing response and "amountofinsurance":
library("BBmisc")
y <- normalize(y, method = "range", range = c(0, 1))
x$AmountOfInsurance <- normalize(x$AmountOfInsurance, method = "range", range = c(0, 1))


# fitting elasticnet model:
library(glmnet)

n <- dim(x)[1] # num observations
d <- dim(x)[2] # num regressor vars

x <- as.matrix(x)
y <- as.matrix(y)
# alpha=1 is lasso regression.
elnetFit <- glmnet(x,y, family="gaussian", nlambda=100, alpha=1, standardize.response = FALSE, standardize=FALSE)
elnetFit
numLam <- length(elnetFit$lambda)

# Getting coefficients of the model for each lambda value! The kth column is the vector of coeffs for the LASSO fit with lambda = myFit$lambda[k]
dim(elnetFit$beta)

# predicting by this model gives a matrix of predictions for each lamda value
dim(predict.glmnet(elnetFit,newx=x))

# Plotting coefficients behaviour on lasso:
t <- rep(0,numLam)
for (i in 1:numLam)
  t[i] <- sum(abs(elnetFit$beta[,i]))

plot(c(0,max(t)),c(0,0),type="l",col="white",ylim=range(elnetFit$beta),xlab="t",ylab="coeff")
abline(h=0)
myClrs <- sample(rainbow(d))
nonzero <- rep(FALSE,numLam)
for(i in 1:d) {
  nonzero <- c((elnetFit$beta[i,]!=0),TRUE)[-1]
  lines(t[nonzero] , elnetFit$beta[i,nonzero] , col=myClrs[i],lwd=2)
  abline(v=t[match(TRUE,nonzero)], lty="dashed", col="grey",lwd=2)
}


# cross validation for glmnet to choose best lamda value.
elnetFit <- cv.glmnet(x,y, type.measure="mse", nfolds=10)
elnetFit$lambda
elnetFit$cvm # mean cross validated error for each lambda
elnetFit$nzero # number of non-zero coefficients at each lambda
elnetFit$lambda.min # best value of lambda that gives minimum cvm
plot(elnetFit)
coef(elnetFit)
dim(predict(elnetFit,newx=x))


# Performing cross validation for chooseing best value of alpha!
alpha <- seq(0:19)/20
lambda <- rep(0,length(alpha))
mse <- rep(0,length(alpha))
# alpha=1 is lasso and alpha=0 is ridge regression!
for (i in 1:length(alpha)){
  elnetFit <- cv.glmnet(x,y, type.measure="mse", nfolds=10)
  mse[i] <- elnetFit$cvm[which(elnetFit$lambda.min == elnetFit$lambda)]
  lambda[i] <- elnetFit$lambda.min
}
lambda
mse