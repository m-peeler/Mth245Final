library(tidyverse)
library(patchwork)
library(car)
library("bestglm")
library("leaps")
source("https://cipolli.com/students/code/plotResiduals.R")

modelSummary <- function(model){
  print(round(summary(model)$coefficients,10))
  
  print(paste("R-squared:", summary(model)$r.squared))
  print(paste("Adjusted R-Squared:", summary(model)$adj.r.squared))
  print(paste("Sigma:", summary(model)$sigma))
  
  plotResiduals(model)
}

read_csv("~/GitHub/Mth245Final/dataset/NCbirths.csv") -> births

births %>% mutate(Sex = case_when(Sex == 1 ~ "Male",
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
                                     Premie == 2~ "No"))
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
                            GainedSq = GainedSC^2)

births <- births %>% mutate(WeightGmLog = log(BirthWeightGm))
births <- births %>% mutate(WeightGmSqrt = BirthWeightGm^.5)
births <- births %>% mutate(WeightGmS = BirthWeightGm^2)
births <- births %>% mutate(WeightGmSLog = log(BirthWeightGm)^2)
births <- births %>% mutate(WeightGmLogLog = log(log(BirthWeightGm)))
births <- births %>% mutate(WeightGmLogSqr = log(BirthWeightGm^2))
births <- births %>% mutate(WeightGmLogQuad = log(BirthWeightGm)^4)
births <- births %>% mutate(WeightGmSqrtLog = log(BirthWeightGm)^.5)
births <- births %>% mutate(WeightGmInverse = 1/(BirthWeightGm))


# 2a - First Order Model
  
lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + RaceMom +
     Marital + Gained + Smoke + Premie, births) -> model.1

modelSummary(model.1)

lm(BirthWeightGm ~ Plural + Sex + 
     MomAgeSC + WeeksSC + RaceMom +
     Marital +
     GainedSC + Smoke + Premie, 
   births) -> model.2

modelSummary(model.2)

# Testing Transformations of Weight to Improve Residual Distribution

# Includes non-significant estimators

lm(WeightGmLog ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.log

modelSummary(model.2)

lm(WeightGmSqrt ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.sqrt

modelSummary(model.sqrt)

lm(WeightGmS ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.s

modelSummary(model.s)

lm(scale(BirthWeightGm, center=T, scale=T) ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.sc

modelSummary(model.sc)

lm(WeightGmSLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.slog

modelSummary(model.slog)

lm(WeightGmLogLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.loglog

modelSummary(model.loglog)

lm(WeightGmLogSqr ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.logsqr

modelSummary(model.logsqr)

lm(WeightGmLogQuad ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.logquad

modelSummary(model.logquad)

lm(WeightGmSqrtLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Premie, 
   births) -> model.sqrtlog

modelSummary(model.sqrtlog)

## Scaled Centered version of model 2

lm(BirthWeightGm ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke, 
   births) -> model.2.new

# Removes non-predictive attributes from model 1

lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + Gained + Smoke, births) -> model.1.new

vif(model.1.new)
vif(model.2.new)

modelSummary(model.2.new)

ggplot(data=model.1.new$model, aes(x=MomAge, y=residuals(model.2.new)))+
  geom_point(size=1,
             shape=16)+
  theme_bw()+
  xlab("Mother's Age")+
  ylab("Residuals")+
  ggtitle("Mother's Age versus Residuals")

ggplot(data=model.1.new$model, aes(x=GainedSC, y=residuals(model.2.new)))+
  geom_point(size=1,
             shape=16)+
  theme_bw()+
  xlab("Weight Gained During Pregnancy")+
  ylab("Residuals")+
  ggtitle("Weight Gained versus Residuals")

ggplot(data=model.1.new$model, aes(x=Weeks, y=residuals(model.2.new)))+
  geom_point(size=1,
             shape=16)+
  theme_bw()+
  xlab("Weeks of Pregnancy")+
  ylab("Residuals")+
  ggtitle("Weeks versus Residuals")

model.with.squares <- lm(BirthWeightGm ~ Plural + Sex + 
                           scale(MomAge, center=T, scale=T) + 
                           I(scale(MomAge, center=T, scale=T)^2) +
                           scale(Weeks, center=T, scale=T) +
                           I(scale(Weeks, center=T, scale=T)^2) +
                           Marital + 
                           scale(Gained, center=T, scale=T) + 
                           I(scale(Weeks, center=T, scale=T)^2) +
                           Smoke, 
                         births)
vif(model.with.squares)

modelSummary(model.with.squares)
## Using log weight

## Best model built

model.quad.log <- lm(WeightGmLog ~ Plural + Sex + 
                       MomAgeSC + MomAgeSq +
                       WeeksSC + WeeksSq +
                       GainedSC + 
                       GainedSq +
                       Smoke, 
                     births)

## Interaction model

model.quad.log.all.interact <- lm(WeightGmLog ~ (Plural + Sex + 
                                    MomAgeSC + MomAgeSq +
                                    WeeksSC + WeeksSq +
                                    GainedSC + 
                                    GainedSq +
                                    Smoke)^2, 
                                  births)



vif(model.quad.log)
modelSummary(model.quad.log)

vif(model.quad.log.all.interact)
modelSummary(model.quad.log.all.interact)

model.select(model.quad.log.all.interact) -> model.refined

modelSummary(model.refined)


births.scrubbed <- births %>% select(WeightGmLog, Plural, Sex, MomAgeSC, MomAgeSq,
                                     WeeksSC, WeeksSq, Marital, GainedSC,
                                     GainedSq, Smoke) %>% 
                              filter(!is.na(GainedSC) & !is.na(Smoke))

x <- model.matrix(model.quad.log)[,-1]

y <- births.scrubbed$WeightGmLog

xy <- as.data.frame(cbind(x,y))
best.subsets.aic <- bestglm(xy, IC="AIC", TopModels = 5)
best.model.aic <- best.subsets.aic$BestModel
modelSummary(best.model.aic)

best.subsets.bic <- bestglm(xy, IC="AIC", TopModels = 5)
best.model.bic <- best.subsets.bic$BestModel
modelSummary(best.model.bic)

regsubsets.out <- regsubsets(WeightGmLog ~ Plural + Sex + 
                                              MomAgeSC + MomAgeSq +
                                              WeeksSC + WeeksSq +
                                              GainedSC + 
                                              GainedSq +
                                              Smoke,
                             data=births, nbest = 1)

as.data.frame(summary(regsubsets.out)$outmat)

fit.stats <- data.frame(num.variables=1:8,
                        adjr2 = summary(regsubsets.out)$adjr2,
                        bic=summary(regsubsets.out)$bic)
fit.stats

## May need to try to fix the high residuals in the middle of the predicted values.

## interact_plot(model.quad.log, pred = GainedSC, modx = Plural, plot.points = TRUE)
interaction.plot(model.quad.log$model$GainedSC, model.quad.log$model$Plural, model.quad.log$model$WeightGmLog)

ggplot(model.quad.log$model, aes(x=GainedSC, y=WeightGmLog, color=Plural)) +
  geom_point(size=1,
             shape=16)+
  geom_smooth(method = "lm", se=F) +
  theme_bw()




births$Plural <- as.factor(births$Plural)
