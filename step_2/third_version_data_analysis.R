library(tidyverse)
library(patchwork)
library(car)
library(GGally)
library(olsrr)
library("bestglm")
library("leaps")
source("https://cipolli.com/students/code/plotResiduals.R")

modelSummary <- function(model){
  print(round(summary(model)$coefficients,10))
  
  print(paste("R-squared:", summary(model)$r.squared))
  print(paste("Adjusted R-Squared:", summary(model)$adj.r.squared))
  print(paste("Sigma:", summary(model)$sigma))
  print(paste("AIC:", AIC(model)))
  print(paste("BIC:", BIC(model)))
  
  plotResiduals(model)
}

r_squared <- function(actual, predicted) {
  cor(actual, predicted)^2
}

predictForSubsets <- function(model, class.attr, ...) {
  x <- list(...)
  i <- 1
  predPlots = list()
  residPlots = list()
  for (v in x) {
    v$predict <- predict(model, v)
    v$resid <- v[[class.attr]] - v$predict
    
    
    print(paste("Subset", i, "R-Squared:", r_squared(v$predict, v[[class.attr]])))
    print(paste("Subset", i, "Mean Abs. Error:", mean(abs(v$resid))))
    
    ggplot(data=v, aes(x=predict, y=resid)) +
      geom_point(size=1,
                 shape=16)+
      theme_bw()+
      xlab("Predicted")+
      ylab("Residuals")+
      ggtitle("Predicted versus Residuals") -> residuals
    ggplot(data=v, aes(x=predict, y=get(class.attr))) +
      geom_point(size=1,
                 shape=16)+
      theme_bw()+
      xlab("Predicted")+
      ylab("Actual")+
      ggtitle("Predicted versus Actual") -> predictions
    print(predictions + residuals)
    i <- i + 1
  }
}

prepData <- function() {
  read_csv("~/GitHub/Mth245Final/dataset/NCbirths.csv") -> births
  
  births <- births %>% mutate(Sex = case_when(Sex == 1 ~ "Male",
                                              Sex == 2 ~ "Female"),
                              Marital = case_when(Marital == 1 ~ "Married",
                                                  Marital == 2 ~ "Unmarried"),
                              RaceMom = case_when(RaceMom == 1 ~ "White",
                                                  RaceMom == 2 ~ "Black",
                                                  RaceMom == 3 ~ "Am. Indian",
                                                  RaceMom == 4 ~ "Chinese",
                                                  RaceMom == 5 ~ "Japanese",
                                                  RaceMom == 6 ~ "Hawaiian",
                                                  RaceMom == 7 ~ "Filipino",
                                                  RaceMom == 8 ~ "Other Asian / PI"),
                              Smoke = case_when(Smoke == 1 ~ "Yes", 
                                                Smoke == 0 ~ "No"),
                              Premie = case_when(Premie == 1 ~ "Yes",
                                                 Premie == 0~ "No"))
  births$Sex <- as.factor(births$Sex)
  births$Marital <- as.factor(births$Marital)
  births$Premie <- as.factor(births$Premie)
  births$Smoke <- as.factor(births$Smoke)
  births$RaceMom <- as.factor(births$RaceMom)
  births$Plural <- as.factor(births$Plural)
  
  births <- births %>% mutate(MomAgeSC = scale(MomAge, center=T, scale=T),
                              MomAgeSq = MomAgeSC^2,
                              WeeksSC = scale(Weeks, center=T, scale=T),
                              WeeksSq = WeeksSC ^2,
                              GainedSC = scale(Gained, center=T, scale=T),
                              GainedSq = I(GainedSC^2))
  
  births <- births %>% filter(!is.na(GainedSC) & !is.na(Smoke))
  
  # Part 1: First-Order Model and Determinations of Necessary Transformations
  
  births <- births %>% mutate(WeightGmLog = log(BirthWeightGm))
  births <- births %>% mutate(WeightGmSqrt = BirthWeightGm^.5)
  births <- births %>% mutate(WeightGmS = BirthWeightGm^2)
  births <- births %>% mutate(WeightGmSLog = log(BirthWeightGm)^2)
  births <- births %>% mutate(WeightGmLogLog = log(log(BirthWeightGm)))
  births <- births %>% mutate(WeightGmLogSqr = log(BirthWeightGm^2))
  births <- births %>% mutate(WeightGmLogQuad = log(BirthWeightGm)^4)
  births <- births %>% mutate(WeightGmSqrtLog = log(BirthWeightGm)^.5)
  births <- births %>% mutate(WeightGmInverse = 1/(BirthWeightGm))
  births <- births %>% mutate(WeightGmSC = scale(BirthWeightGm, center=T, scale=T))
  births <- births %>% mutate(WeightLogSC = scale(WeightGmLog, center=T, scale=T))
  
  births <- births %>% mutate(Twin = (as.character(Plural) == "2"))
  births <- births %>% mutate(Triplet = (as.character(Plural) == "3"))
  births <- births %>% mutate(Filipino = (RaceMom == "Filipino"))
  births <- births %>% mutate(Black = (RaceMom == "Black"))
  
  births$Twin = as.factor(births$Twin)
  births$Triplet = as.factor(births$Triplet)
  births$Fllipino = as.factor(births$Filipino)
  births$Black = as.factor(births$Black)
  
  births
  
}

births <- prepData()

# First-Order Model
lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + RaceMom +
     Marital + Gained + Smoke + Premie, births) -> model.1

modelSummary(model.1)

# Testing Transformations of Weight to Improve Residual Distribution

# Includes non-significant estimators

# Log of Weight
lm(WeightGmLog ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.log

modelSummary(model.log)

# Square Root of Weight
lm(WeightGmSqrt ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.sqrt

modelSummary(model.sqrt)

# Square of Weight
lm(WeightGmS ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.s

modelSummary(model.s)

# Square of the Log of Weight
lm(WeightGmSLog ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.slog

modelSummary(model.slog)

# Log of the Log of Weight
lm(WeightGmLogLog ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.loglog

modelSummary(model.loglog)

# Log of the Square Root of Weight
lm(WeightGmLogSqr ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.logsqr

modelSummary(model.logsqr)

# Fourth Power of the Log of Weight
lm(WeightGmLogQuad ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.logquad

modelSummary(model.logquad)

# Square Root of the Log of Weight
lm(WeightGmSqrtLog ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.sqrtlog

modelSummary(model.sqrtlog)

# Inverse of the Weight
lm(WeightGmInverse ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.inverse

modelSummary(model.sqrtlog)

## At this point, we have two potential "best models": the untransormed weight, which
## better satisfies the assumptions behind linear regression modeling, namely the equal
## distribution of residuals; and we have the log weight model, which has higher r-squared values
## and lower sigma, AIC, and BIC values than most other models, meaning it is more accurate.
## This accuracy is, however, gained only at the expense of making residuals less equally distributed,
## and thus satisfies the assumptions necessary to do things like conduct statistical inference on parameters. 
## Thus, we will be proceeding with two models, the assumptions model and the accuracy model, where one will seek
## to minimize departure from assumptions and the other will seek to maximize accuracy. 
## Assumption models will use the scaled and centered gram weight as their target, while
## accuracy models will use the scaled and centered log of gram weight.

# First-Order Model with All Appropriate Transformations, and Numeric Attributes Scaled & Centered
lm(WeightGmSC ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.2.assu

modelSummary(model.2.assu)
predictForSubsets(model.2.assu, "WeightGmSC", 
                  births, 
                  births %>% filter(Premie=="Yes"),
                  births %>% filter(Premie=="No"))

lm(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
     Marital + GainedSC + Smoke + Premie, births) -> model.2.accur

modelSummary(model.2.accur)
predictForSubsets(model.2.accur, "WeightLogSC", 
                  births,
                  births %>% filter(Premie=="Yes"),
                  births %>% filter(Premie=="No"))
# Part 2: AIC/BIC Iteration 1

## Assumptions Model

x <- model.matrix(model.2.assu)[,-1]

y <- births$WeightGmSC

xy <- as.data.frame(cbind(x,y))
best.subsets.aic <- bestglm(xy, IC="AIC", TopModels = 5)
best.model.aic <- best.subsets.aic$BestModel
modelSummary(best.model.aic)

best.subsets.bic <- bestglm(xy, IC="BIC", TopModels = 5)
best.model.bic <- best.subsets.bic$BestModel
modelSummary(best.model.bic)

regsubsets.out <- regsubsets(WeightGmSC ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
                               Marital + GainedSC + Smoke + Premie,
                             data=births, nbest = 1, nvmax=15)

as.data.frame(summary(regsubsets.out)$outmat)

fit.stats <- data.frame(num.variables=1:15,
                        adjr2 = summary(regsubsets.out)$adjr2,
                        bic=summary(regsubsets.out)$bic)
fit.stats

## Of the models, BIC suggests removing Marital and all races
## except Black. AIC / radj2 would retain Filipino and Marital.
## Since there is only a single Philipino instance, we remove Filipino,
## as well as marital since the significance is so low.

model.3.assu <- lm(WeightGmSC ~ Plural + Sex + MomAgeSC + WeeksSC + Black +
                      GainedSC + Smoke + Premie,
                    births)

modelSummary(model.3.assu)

## Accuracy Model

x <- model.matrix(model.2.accur)[,-1]

y <- births$WeightLogSC

xy <- as.data.frame(cbind(x,y))
best.subsets.aic <- bestglm(xy, IC="AIC", TopModels = 5)
best.model.aic <- best.subsets.aic$BestModel
modelSummary(best.model.aic)

best.subsets.bic <- bestglm(xy, IC="BIC", TopModels = 5)
best.model.bic <- best.subsets.bic$BestModel
modelSummary(best.model.bic)

regsubsets.out <- regsubsets(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + RaceMom +
                               Marital + GainedSC + Smoke + Premie,
                             data=births, nbest = 1, nvmax=15)

as.data.frame(summary(regsubsets.out)$outmat)

fit.stats <- data.frame(num.variables=1:15,
                        adjr2 = summary(regsubsets.out)$adjr2,
                        bic=summary(regsubsets.out)$bic)
fit.stats

model.3.accur <- lm(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + Black +
                      GainedSC + Smoke + Premie, births)

modelSummary(model.3.accur)

# Step 4: Square Terms and Interaction

  # Assumptions model

model.4.assu <- lm(WeightGmSC ~ Plural + Sex + MomAgeSC + MomAgeSq + WeeksSC + Black +
                     GainedSC + GainedSq + Smoke + Premie, births)
modelSummary(model.4.assu)

model.4.interact.assu <- lm(WeightGmSC ~ (Plural + Sex + MomAgeSC + MomAgeSq + WeeksSC + Black +
                                            GainedSC + GainedSq + Smoke + Premie)^2, births)
modelSummary(model.4.interact.assu)

step(model.4.interact.assu, direction="both") 

model.4.assu.rstrct <- lm(formula = WeightGmSC ~ Plural + Sex + MomAgeSC + MomAgeSq + 
                            WeeksSC + Black + GainedSC + GainedSq + Smoke + Premie + 
                            Plural:WeeksSC + MomAgeSC:MomAgeSq + MomAgeSC:WeeksSC + MomAgeSC:GainedSq + 
                            MomAgeSC:Smoke + MomAgeSq:Black + MomAgeSq:GainedSC + WeeksSC:Black + 
                            WeeksSC:GainedSq + WeeksSC:Smoke + WeeksSC:Premie + Black:Smoke + 
                            Black:Premie + GainedSq:Premie, data = births)

modelSummary(model.4.assu.rstrct)

## Removed the highst 3 values of GVIF, which were MomAgeSC*MomAgeSq, Plural*WeeksSC, and WeeksSC*Premie

regsubsets(formula = WeightGmSC ~ Plural + Sex + MomAgeSC + MomAgeSq + 
             WeeksSC + Black + GainedSC + GainedSq + Smoke + Premie + 
             Plural:WeeksSC + MomAgeSC:MomAgeSq + MomAgeSC:WeeksSC + MomAgeSC:GainedSq + 
             MomAgeSC:Smoke + MomAgeSq:Black + MomAgeSq:GainedSC + WeeksSC:Black + 
             WeeksSC:GainedSq + WeeksSC:Smoke + WeeksSC:Premie + Black:Smoke + 
             Black:Premie + GainedSq:Premie, data = births,
           nbest=1, nvmax=15) -> out 

fit.stats <- data.frame(num.variables=1:30,
                        adjr2 = summary(out)$adjr2,
                        bic=summary(out)$bic)
fit.stats
out$

modelSummary(model.4.assu.rstrct)
