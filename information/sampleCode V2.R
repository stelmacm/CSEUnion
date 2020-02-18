setwd('C:/Users/.../myfolder')

## load the data
policies <- read.csv('AFC_policies.csv')
claims <- read.csv('AFC_claims.csv')

## load the dplyr library that will help us with data wrangling 
## R Packages can be installed with the command install.packages("dplyr")
library(dplyr)

## combine the policies and claims data sets
policies_with_clams <- policies %>% left_join(claims) %>% group_by(BusinessID, BusinessType, Province, Latitude, Longitude, City, AmountOfInsurance, Earnings) %>% summarize(TotalIncurred=sum(coalesce(ClaimAmount,0)))
policies_with_clams$lossCost <- policies_with_clams$TotalIncurred

## have a quick look at the result
head(policies_with_clams)
nrow(policies_with_clams) == nrow(policies) ## TRUE : Ok

## Now we will try to model our loss cost
myFormula <- lossCost ~ Latitude + Province + I(City=='TORONTO') ## let's start with this random model...

library(statmod) ## the statmod library will allow us to use a tweedie distribution assumption
myModel <- glm(myFormula, data=policies_with_clams,family=tweedie(var.power=1.5, link.power=0))
summary(myModel)

## We can now apply our model to predict the loss costs
policies_with_clams$prediction <- predict(myModel,policies_with_clams,type="response")
fixedExpensesPerPolicy <- 500 ## randomly selected number
variableExpensesPerPolicy <- 0.4 ## randomly selected number
LAE <- 0.2 ## randomly selected number
profitLoading <- 0.25 ## sky is the limit

lossCostTrend <- exp(rnorm(1, 0.06, 0.05)) ## randomly selected trend - not best practice

policies_with_clams$premium <- (policies_with_clams$prediction*lossCostTrend*(1+LAE) + fixedExpensesPerPolicy)/(1-variableExpensesPerPolicy-profitLoading)

template <- read.csv('templateForYourPremiums.csv')
dim(template) #250k rows : Ok
template <- template %>% inner_join(policies_with_clams[,c('BusinessID', 'premium')])
template$ProposedPremium <- template$premium
template$premium <- NULL ## I don't need this column anymore
head(template)
dim(template) #250k rows : Ok
sum(is.na(template$premium)) # 0 : Ok
write.csv(template, "templateToUpload.csv", row.names=FALSE)
