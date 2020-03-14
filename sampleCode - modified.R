setwd('C:/Users/.../myfolder')
setwd('C:/Users/EG95108/workshoptest')
## load the data
marketPolicies <- read.csv('marketPolicies.csv')
policies <- read.csv('companyPolicies.csv')
claims <- read.csv('companyClaims.csv')

## load the dplyr library that will help us with data wrangling 
## R Packages can be installed with the command install.packages("dplyr")
library(dplyr)

## combine the policies and claims data sets
policies_with_claims <- policies %>% left_join(claims) %>% group_by(businessId, BusinessType, Province, Latitude, Longitude, City, AmountOfInsurance, Earnings, CurrentPremiumOffered, CurrentClosingRatio, weight ) %>% summarize(TotalIncurred=sum(coalesce(ClaimAmount,0)))
policies_with_claims$lossCost <- policies_with_claims$TotalIncurred/policies_with_claims$weight

## have a quick look at the result
head(policies_with_claims)
nrow(policies_with_claims) == nrow(policies) ## TRUE : Ok

## our loss ratios per segment :
policies_with_claims %>% group_by(BusinessType) %>% summarize(Policies=n(), EP=sum(CurrentPremiumOffered*weight), Claims=sum(TotalIncurred)) %>% mutate(LR=Claims/EP) %>% arrange(desc(EP))

## Now we will try to model our loss cost 
myFormula <- lossCost ~ Province + Longitude + Latitude +  ## let's go with this Ok loss cost model
  I(log(AmountOfInsurance)) + ## log transform of the amount of insurance
  I(log(Earnings)) + ## log transform of the earnings
  I(BusinessType == 'automobile repair shops and oil change centers') + ## specific parameter for automobile repair shops
  I(BusinessType == 'optometrists offices') + ## parameter for optometrists offices
  I(CurrentClosingRatio^1.5) ## trying something with this ! 

library(statmod) ## the statmod library will allow us to use a tweedie distribution assumption
myModel <- glm(myFormula, data=policies_with_claims, weight=policies_with_claims$weight, family=tweedie(var.power=1.5, link.power=0))
summary(myModel) 
## The result for our I(CurrentClosingRatio^2) parameter is very interesting. 
## The negative Beta coefficient suggests that we should be charging less to people that we already attracting! 
## This seems like a dangerous pricing strategy.

## lets keep going with it because this is just a sample code

## We can now apply our model to predict the loss costs
marketPolicies$prediction <- predict(myModel,marketPolicies,type="response")
fixedExpensesPerPolicy <- 250
variableExpensesPerPolicy <- 0.2
LAE <- 0.1
profitLoading <- 0.05 ## $_$

marketPolicies$premium <- (marketPolicies$prediction*(1+LAE) + fixedExpensesPerPolicy)/(1-variableExpensesPerPolicy-profitLoading)

template <- read.csv('templateForYourPremiums.csv')
dim(template) #500k rows : Ok

template <- template %>% inner_join(marketPolicies[,c('businessId', 'premium')])
template$PremiumOffered <- template$premium
template$premium <- NULL ## I don't need this column anymore

head(template)

if(nrow(template) != 500000){  # need 500k profiles
  stop('missing rows')
}
if(sum(is.na(template$premium)) != 0){ 
  stop('missing premiums')
}
 

write.csv(template, "templateToUpload.csv", row.names=FALSE)
