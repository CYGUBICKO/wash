#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Effect plots by JD ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 13 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2); theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))
library(lme4)
library(splines)

load("switchSingleModel.rda")
load("eplotsFuns.rda")
load("ordFuns.rda")

## Single var model
y1_df <- singMod_df
y1_df$slumarea <- as.factor(y1_df$slumarea) # All cats have to be factors otherwise error?
y1_model <- y1_model

predNames <- c("slumarea", "years", "xm")
isoList <- lapply(predNames, function(n){
  ordpred(mod = y1_model, n, modAns = y1_df)
})

catNames <- c("slumarea")
print(varPlot(isoList[[1]], ylab=""))

print(varPlot(isoList[[2]], ylab="years"))

print(varPlot(isoList[[3]], ylab="xm"))


## Joint model

joint_df <- jointMod_df
joint_df$slumarea <- as.factor(joint_df$slumarea) # All cats have to be factors otherwise error?
joint_df$services <- as.factor(joint_df$services)
joint_model <- glmer_model

predNames <- c("services", "years", "xm")
isoList2 <- lapply(predNames, function(n){
ordpred(mod = joint_model, n, modAns = joint_df)
})

catNames <- c("services")
print(varPlot(isoList2[[1]], ylab=""))

print(varPlot(isoList2[[2]], ylab=""))

print(varPlot(isoList2[[3]], ylab=""))

### Conditional on services
#srvs <- c("y1", "y2")
#isoList <- list()
#for (s in srvs){
#	iso_df <- (joint_df
#		%>% filter(services==s)
#		%>% data.frame()
#	)
#	isoList[[s]] <- lapply("years", function(n){
#	  ordpred(mod = joint_model, n, modAns = iso_df)
#	})
#}
#print(isoList)




