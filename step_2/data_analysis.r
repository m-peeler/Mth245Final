library(tidyverse)
library(patchwork)
library(car)
library("bestglm")

source("https://cipolli.com/students/code/plotResiduals.R")

fmodelSummary <- function(model){
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


lm(BirthWeightGm ~ Gained, births) %>% plotResiduals()

# 2a - First Order Model
lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + RaceMom +
     Marital + Gained + Smoke + Low + Premie, births) -> model.1

modelSummary(model.1)

lm(BirthWeightGm ~ Plural + Sex + 
     MomAgeSC + WeeksSC + RaceMom +
     Marital +
     GainedSC + Smoke + Low + Premie, 
   births) -> model.2

lm(BirthWeightGm ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.2.comp

modelSummary(model.2)
modelSummary(model.2.comp)



##
## Significant estimators:
## Plural2, Plural3, Sex2, MomAge, Weeks, RaceMom2, RaceMom7,
## Gained, Smoke1, Low
##

# Testing other iterations

#Excludes non-significant estimators

# Uses a logarithmic version of the data to correct for imbapances 

lm(WeightGmLog ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.log

modelSummary(model.2)

lm(WeightGmSqrt ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sqrt

modelSummary(model.sqrt)

lm(WeightGmS ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.s

modelSummary(model.s)

lm(scale(BirthWeightGm, center=T, scale=T) ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sc

modelSummary(model.sc)

lm(WeightGmSLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.slog

modelSummary(model.slog)

lm(WeightGmLogLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.loglog

modelSummary(model.loglog)

lm(WeightGmLogSqr ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.logsqr

modelSummary(model.logsqr)

lm(WeightGmLogQuad ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.logquad

modelSummary(model.logquad)

lm(WeightGmSqrtLog ~
     Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) + RaceMom +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low + Premie, 
   births) -> model.sqrtlog

modelSummary(model.sqrtlog)

## Scaled Centered version of model 2

lm(BirthWeightGm ~ Plural + Sex + 
     scale(MomAge, center=T, scale=T) + 
     scale(Weeks, center=T, scale=T) +
     Marital + 
     scale(Gained, center=T, scale=T) + Smoke + Low, 
   births) -> model.2.new

# Removes non-predictive attributes from model 1

lm(BirthWeightGm ~ Plural + Sex + MomAge + Weeks + Marital + Gained + Smoke + Low, births) -> model.1.new

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

## Best model built

model.quad.log <- lm(WeightGmLog ~ Plural + Sex + 
                       MomAgeSC + MomAgeSq +
                       WeeksSC + WeeksSq +
                       Marital + GainedSC + 
                       GainedSq +
                       Smoke + Low, 
                     births)

## Interaction model

model.quad.log.all.interact <- lm(WeightGmLog ~ (Plural + Sex + 
                                    MomAgeSC + MomAgeSq +
                                    WeeksSC + WeeksSq +
                                    Marital + GainedSC + 
                                    GainedSq +
                                    Smoke + Low)^2, 
                                  births)



vif(model.quad.log)
modelSummary(model.quad.log)

cor(model.quad.log.all.interact)

vif(model.quad.log.all.interact)
modelSummary(model.quad.log.all.interact)

## Methods for stepwise elemination of insignificant p-value.
## Source: Joris Meys, https://stackoverflow.com/questions/3701170/stepwise-regression-using-p-values-to-drop-variables-with-nonsignificant-p-value

# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test="F")
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}              
    } # end while(T) drop var
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}

round(summary(model.quad.log)$coefficients, 4)

model.select(model.quad.log.all.interact) -> model.refined

modelSummary(model.refined)

y <- births$WeightGmLog

x <- model.matrix(model.quad.log)[,-1]
births.scrubbed <- births %>% select(WeightGmLog, Plural, Sex, MomAgeSC, MomAgeSq,
                                     WeeksSC, WeeksSq, Marital, GainedSC,
                                     GainedSq, Smoke, Low)
xy <- as.data.frame(cbind(x,y))

## May need to try to fix the high residuals in the middle of the predicted values.

## interact_plot(model.quad.log, pred = GainedSC, modx = Plural, plot.points = TRUE)
interaction.plot(model.quad.log$model$GainedSC, model.quad.log$model$Plural, model.quad.log$model$WeightGmLog)

ggplot(model.quad.log$model, aes(x=GainedSC, y=WeightGmLog, color=Plural)) +
  geom_point(size=1,
             shape=16)+
  geom_smooth(method = "lm", se=F) +
  theme_bw()

births$Plural <- as.factor(births$Plural)
