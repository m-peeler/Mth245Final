library(tidyverse)
library(patchwork)
library(car)
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

births <- births %>% mutate(WeightGmLog = log(BirthWeightGm))
births <- births %>% mutate(WeightGmSqrt = BirthWeightGm^.5)
births <- births %>% mutate(WeightGmS = BirthWeightGm^2)
births <- births %>% mutate(WeightGmSLog = log(BirthWeightGm)^2)
births <- births %>% mutate(WeightGmLogLog = log(log(BirthWeightGm)))
births <- births %>% mutate(WeightGmLogSqr = log(BirthWeightGm^2))
births <- births %>% mutate(WeightGmSLog = log(BirthWeightGm)^4)
births <- births %>% mutate(WeightGmSqrtLog = log(BirthWeightGm)^.5)



lm(BirthWeightGm ~ Gained, births) %>% plotResiduals()

lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + RaceMom +
     Marital + Gained + Smoke + Low + Premie, births) -> model.1

lm(BirthWeightGm ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital +
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.2

lm(WeightGmLog ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.log

lm(WeightGmSqrt ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sqrt

lm(WeightGmS ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.s

lm(scale(BirthWeightGm, center=T, scale=T) ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sc

lm(WeightGmSLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.slog

lm(WeightGmLogLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.loglog

lm(WeightGmLogSqr ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.logsqr

lm(WeightGmLogSqr ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.logqad

lm(WeightGmSqrtLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sqrtlog


round(summary(model.1)$coefficients,10)
round(summary(model.2)$coefficients,10)
round(summary(model.log)$coefficients,10)
round(summary(model.sqrt)$coefficients,10)
round(summary(model.logqad)$coefficients,10)
round(summary(model.sqrtlog)$coefficients,10)



summary(model.1)$r.squared
summary(model.1)$adj.r.squared
summary(model.1)$sigma

summary(model.2)$r.squared
summary(model.2)$adj.r.squared
summary(model.2)$sigma

summary(model.log)$r.squared
summary(model.log)$adj.r.squared
summary(model.log)$sigma

summary(model.slog)$r.squared
summary(model.slog)$adj.r.squared
summary(model.slog)$sigma


summary(model.sqrt)$r.squared
summary(model.sqrt)$adj.r.squared
summary(model.sqrt)$sigma

summary(model.s)$r.squared
summary(model.s)$adj.r.squared
summary(model.s)$sigma

summary(model.sc)$r.squared
summary(model.sc)$adj.r.squared
summary(model.sc)$sigma

summary(model.slog)$r.squared
summary(model.slog)$adj.r.squared
summary(model.slog)$sigma

summary(model.logqad)$r.squared
summary(model.logqad)$adj.r.squared
summary(model.logqad)$sigma

summary(model.sqrtlog)$r.squared
summary(model.sqrtlog)$adj.r.squared
summary(model.sqrtlog)$sigma

source("https://cipolli.com/students/code/plotResiduals.R")
plotResiduals(model.1)
plotResiduals(model.2)
plotResiduals(model.log)
plotResiduals(model.sqrt)
plotResiduals(model.s)
plotResiduals(model.sc)
plotResiduals(model.slog)
plotResiduals(model.loglog)
plotResiduals(model.logsqr)
plotResiduals(model.logqad)
plotResiduals(model.sqrtlog)

model.1$fitted.values

lm(BirthWeightGm ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low, 
   births) -> model.2.new

lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + Marital + Gained + Smoke + Low, births) -> model.1.new

vif(model.1)
vif(model.2.new)

plotResiduals(model.2.new)

summary(model.2.new)$r.squared
summary(model.2.new)$adj.r.squared
summary(model.2.new)$sigma

ggplot(data=model.1.new$model, aes(x=MomAge, y=residuals(model.2.new)))+
  geom_point(size=1,
             shape=16)+
  theme_bw()+
  xlab("Mother's Age")+
  ylab("Residuals")+
  ggtitle("Mother's Age versus Residuals")

ggplot(data=model.1.new$model, aes(x=Gained, y=residuals(model.2.new)))+
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
                           Smoke + Low, 
                         births)
vif(model.with.squares)

plotResiduals(model.with.squares)

summary(model.with.squares)$r.squared
summary(model.with.squares)$adj.r.squared
summary(model.with.squares)$sigma
round(summary(model.with.squares)$coefficients,10)

## Using log weight

model.quad.log <- lm(WeightGmLog ~ Plural + Sex + 
                       scale(MomAge, center=T, scale=T) + 
                       I(scale(MomAge, center=T, scale=T)^2) +
                       scale(Weeks, center=T, scale=T) +
                       I(scale(Weeks, center=T, scale=T)^2) +
                       Marital + 
                       scale(Gained, center=T, scale=T) + 
                       I(scale(Weeks, center=T, scale=T)^2) +
                       Smoke + Low, 
                     births)

vif(model.quad.log)

plotResiduals(model.quad.log)

summary(model.quad.log)$r.squared
summary(model.quad.log)$adj.r.squared
summary(model.quad.log)$sigma
round(summary(model.quad.log)$coefficients,10)

## May need to try to fix the high residuals in the middle of the predicted values.