library(tidyverse)
library(patchwork)
library(car)
library(GGally)
library(olsrr)
library(gridExtra)
library(caret)
library(car)
library("bestglm")
library("leaps")
source("https://cipolli.com/students/code/plotResiduals.R")
library("qqplotr")

# Mean Absolute Residual Quantile Difference
quantDepart <- function(model) {
  residuals <- sort(scale(model$residuals, scale=T))
  i <- 1:length(residuals)
  fi <- (i - 0.5) / length(residuals)
  x.norm <- qnorm(fi)
  
  mean(abs(residuals - x.norm))
}

modelSummary <- function(model, coef=T, stat=T, plot=T){
  if(coef) {
    print(round(summary(model)$coefficients,10))
  }
  if(stat) {
    print(paste("R-squared:", summary(model)$r.squared))
    print(paste("Adjusted R-Squared:", summary(model)$adj.r.squared))
    print(paste("Sigma:", summary(model)$sigma))
    print(paste("AIC:", AIC(model)))
    print(paste("BIC:", BIC(model)))
    print(paste("Quantile Departure:", quantDepart(model)))
  }
  if(plot) {
    plotResiduals(model)
  }
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
  births <- read_csv("~/GitHub/Mth245Final/dataset/NCbirths.csv")
  
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

interactWith <- function(data, class.attr, interactors, non.interactors){
  for (n in non.interactors) {
    plots <- list() 
    for (i in interactors) {
      newPlot <- ggplot(data=data, aes(x=get(n), y=get(class.attr), color=get(i))) +
        geom_point(size=1,
                   shape=16)+
        geom_smooth(method = "lm", se=F) +
        theme_bw() +
        labs(x="", y=i)

      plots <- append(plots, list(newPlot))
      
    }
    do.call("grid.arrange", append(plots, list()))  
    
  }
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
    predPlots <- append(predPlots, list(predictions))
    residPlots <- append(residPlots, list(residuals))
    print(predictions + residuals)
    i <- i + 1
  }
  do.call("grid.arrange", append(append(predPlots, residPlots), list(ncol=length(residPlots))))
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
modelSummary(best.model.aic, plot=F)

best.subsets.bic <- bestglm(xy, IC="BIC", TopModels = 5)
best.model.bic <- best.subsets.bic$BestModel
modelSummary(best.model.bic, plot=F)

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

interact <- c("Plural", "Premie", "Black")
non.interact <- c("MomAgeSC", "MomAgeSq", "WeeksSC", "GainedSC", "GainedSq")
interactWith(births, "WeightGmSC", interact, non.interact)

model.part.interact.assu <- lm(WeightGmSC ~ (Premie + Plural + Black) * (Plural + Sex + MomAgeSC + MomAgeSq + WeeksSC + Black +
                                               GainedSC + GainedSq + Smoke + Premie), births)
modelSummary(model.part.interact.assu)

model.interact.assu <- lm(WeightGmSC ~ (Plural + Sex + MomAgeSC + MomAgeSq + WeeksSC + Black +
                                            GainedSC + GainedSq + Smoke + Premie)^2, births)
modelSummary(model.interact.assu)

## Uses model.part.interact.assu because it has fewer variables but is nearly as good.

step(model.part.interact.assu, direction="both") 

model.reduce.part.interact <- lm(formula = WeightGmSC ~ Premie + Plural + Black + Sex + MomAgeSC + 
                                   + MomAgeSq + WeeksSC + GainedSC + Smoke + Premie:MomAgeSC + 
                                   + Premie:WeeksSC + Premie:Black + Premie:Smoke + Plural:WeeksSC + 
                                   + Black:MomAgeSq + Black:WeeksSC + Black:Smoke, data = births)

modelSummary(model.reduce.part.interact)

model.assu.rstrct <- lm(formula = WeightGmSC ~ Plural + Sex + MomAgeSC + MomAgeSq + 
                            WeeksSC + Black + GainedSC + GainedSq + Smoke + Premie + 
                            Plural:WeeksSC + MomAgeSC:MomAgeSq + MomAgeSC:WeeksSC + MomAgeSC:GainedSq + 
                            MomAgeSC:Smoke + MomAgeSq:Black + MomAgeSq:GainedSC + WeeksSC:Black + 
                            WeeksSC:GainedSq + WeeksSC:Smoke + WeeksSC:Premie + Black:Smoke + 
                            Black:Premie + GainedSq:Premie, data = births)

modelSummary(model.assu.rstrct)

## Removed the highst 3 values of GVIF, which were MomAgeSC*MomAgeSq, Plural*WeeksSC, and WeeksSC*Premie

cbind(vif(model.assu.rstrct), vif(model.assu.rstrct)[,3]^2)

lm(WeightGmSC ~ Plural + Sex + MomAgeSC + MomAgeSq + 
     WeeksSC + Black + GainedSC + GainedSq + Smoke + Premie + 
     Plural:WeeksSC + MomAgeSC:MomAgeSq + MomAgeSC:WeeksSC + MomAgeSC:GainedSq + 
     MomAgeSC:Smoke + MomAgeSq:Black + MomAgeSq:GainedSC + WeeksSC:Black + 
     WeeksSC:GainedSq + WeeksSC:Smoke + Black:Smoke + 
     Black:Premie + GainedSq:Premie, data = births) -> min.vif.model

cbind(vif(min.vif.model), vif(min.vif.model)[,3]^2)
modelSummary(min.vif.model)


## Accuracy model

model.3.accur <- lm(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + Black +
                      GainedSC + Smoke + Premie, births)
modelSummary(model.3.accur)

model.interact.accur <- lm(WeightLogSC ~ (Plural + Sex + MomAgeSC + WeeksSC + Black +
                             GainedSC + Smoke + Premie)^2, births)
modelSummary(model.interact.accur)

model.part.interact.accur <- lm(WeightLogSC ~ (Plural + Black + Premie) * (Sex + MomAgeSC + WeeksSC +
                                                 GainedSC + Smoke), births)
modelSummary(model.part.interact.accur)

step(model.interact.accur, direction="both")
modelSummary(lm(formula = WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + 
                  Black + GainedSC + Smoke + Premie + Plural:WeeksSC + MomAgeSC:Smoke + 
                  WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                  Black:Smoke + Black:Premie, data = births), coef=F)

mod.accur.prelim.full <- lm(formula = WeightLogSC ~ Twin + Triplet + Sex + MomAgeSC + WeeksSC + 
                               Black + GainedSC + Smoke + Premie + Twin:WeeksSC + Triplet:WeeksSC + MomAgeSC:Smoke + 
                               WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                               Black:Smoke + Black:Premie, data = births)

cbind(vif(mod.accur.prelim.full), vif(mod.accur.prelim.full)[,3]^2)

mod.accur.final <- mod.accur.prelim.full
modelSummary(mod.accur.final)

step(model.part.interact.accur, direction="both")
mod.accur.prelim <- lm(formula = WeightLogSC ~ Plural + Black + Premie + Sex + MomAgeSC + 
                         WeeksSC + GainedSC + Smoke + Plural:WeeksSC + Black:Smoke + 
                         Premie:WeeksSC + Premie:Smoke, data = births)

modelSummary(mod.accur.prelim, coef=F)

cbind(vif(mod.accur.prelim), vif(mod.accur.prelim)[,3]^2)

mod.accur.final.part <- mod.accur.prelim

modelSummary(mod.accur.final.part)
## Cross validation

## How much of our training is just overfitting?

specs <- trainControl(method = "CV",number = 10)
model.cross.accur <- train(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + 
                             Black + GainedSC + Smoke + Premie + Plural:WeeksSC + MomAgeSC:Smoke + 
                             WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                             Black:Smoke + Black:Premie,
                           data = births, method = "lm",trControl = specs,na.action = na.omit)

model.cross.accur

specs <- trainControl(method = "LOOCV",number = 10)
model.cross.loo.accur <- train(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + 
                                 Black + GainedSC + Smoke + Premie + Plural:WeeksSC + MomAgeSC:Smoke + 
                                 WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                                 Black:Smoke + Black:Premie,
                           data = births, method = "lm",trControl = specs,na.action = na.omit)

model.cross.loo.accur 

specs <- trainControl(method = "CV",number = 10)
model.cross.assu <- train(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + 
                             Black + GainedSC + Smoke + Premie + Plural:WeeksSC + MomAgeSC:Smoke + 
                             WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                             Black:Smoke + Black:Premie,
                           data = births, method = "lm",trControl = specs,na.action = na.omit)

model.cross.assu

specs <- trainControl(method = "LOOCV",number = 10)
model.cross.loo.assu <- train(WeightLogSC ~ Plural + Sex + MomAgeSC + WeeksSC + 
                                 Black + GainedSC + Smoke + Premie + Plural:WeeksSC + MomAgeSC:Smoke + 
                                 WeeksSC:Black + WeeksSC:GainedSC + WeeksSC:Smoke + WeeksSC:Premie + 
                                 Black:Smoke + Black:Premie,
                               data = births, method = "lm",trControl = specs,na.action = na.omit)

assessModel <- function(model, p) {
  print(modelSummary(model))
  cbind(vif(model), vif(model)[,3]^2)
  print(summary(model$residual))
  print(confint(model))
  lev <- model$model %>% mutate(h.values = hatvalues(model))
  print(summary(lev$h.values))
  n <- nrow(model$model)
  high.lev <- lev %>% filter(h.values > 2*p/n)
  print(paste("High Lev.:", nrow(high.lev)))
  v.high.lev <- lev %>% filter(h.values > 3*p/n)
  print(paste("Very High Lev.:", nrow(v.high.lev)))
  new.resid <- model$model %>% mutate(stdres = rstandard(model),
                                      stures = rstudent(model))
  print("Standard Residual Quant.:")
  print(summary(new.resid$stdres))
  print("Studentized Residual Quant.:")
  print(summary(new.resid$stures))
  s.outliers.stdres <- new.resid %>% filter(abs(stdres)>3)
  print(paste("Strong Standard Residual Outliers:", nrow(s.outliers.stdres)))
  print(s.outliers.stdres)
  s.outliers.stures <- new.resid %>% filter(abs(stures)>3)
  print(paste("String Studentized Residual Outliers:", nrow(s.outliers.stures)))
  print(s.outliers.stures)
  cooks.values <- model$model %>% mutate(cooks = cooks.distance(model))
  print("Cook's Values:")
  print(summary(cooks.values$cooks))
  cooks.strong <- cooks.values %>% filter(cooks>1)
  print(paste("Strong C. Values:", nrow(cooks.strong)))
}


assessModel(mod.accur.final, 15)
assessModel(mod.accur.final.part, 14)
assessModel(min.vif.model, 25)
modelSummary(mod.accur.final)


