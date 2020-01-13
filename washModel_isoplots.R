#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Plotting effect plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 10 (Fri) ----

library(splines)
library(gridExtra)
library(ggplot2)
library(lme4)
library(dplyr)
library(emmeans)

source("../funs/plotFuns.R")
source("../funs/ordfuns.R")

load("washModelPvalues.rda")

#### ---- Emmeans ----

mod <- glmer_model_consec_scaledyr

## Sevices
services <- emmeans(mod
	, ~services
)
plot(services, comparisons = TRUE)

## Gender
gender <- emmeans(mod
	, ~gender:services
)
plot(gender, comparisons = TRUE)

## Slum area
slumarea <- emmeans(mod
	, ~slumarea:services
)
plot(slumarea, comparisons = TRUE)

## Age
### Means
age <- emmeans(mod
	, ~age:services
)
plot(age, comparisons = TRUE)

## Trends
emmip(mod, ~age|services, at = list(age = seq(20, 80, 10)))

## year
### Means
year <- emmeans(mod
	, ~year:services
)
plot(year, comparisons = TRUE)

## Trends
emmip(mod, ~year|services, at = list(year = seq(0, 10, 2)))

## HHsize
### Means
hhsize <- emmeans(mod
	, ~hhsize:services
)
plot(hhsize, comparisons = TRUE)

## Trends
emmip(mod, ~hhsize|services, at = list(hhsize = seq(0, 10, 2)))

## ServiceP
### Means
statusP <- emmeans(mod
	, ~statusP:services
)
plot(statusP, comparisons = TRUE)

## Trends
emmip(mod, ~statusP|services, at = list(statusP = seq(0, 1, length.out = 5)))
quit()
#NOTE: A nesting structure was detected in the fitted model:
#    gender %in% services, slumarea %in% services



predNames <- attr(terms(model_form), "term.labels")
predNames <- gsub(".*\\:|.*\\(|\\,.*", "", predNames)
predNames <- predNames[!grepl("\\|", predNames)]

predSummary <- attr(terms(model_form), "term.labels")
predSummary <- gsub(".*\\:", "", predSummary)
predSummary <- predSummary[!grepl("\\|", predSummary)]

d1 <- (consec_long_df
	%>% mutate_at(c("gender", "services", "slumarea"), as.factor)
	%>% data.frame()
)
dd <- model.frame(as.formula(paste0("status~", fixed_effects))
	, data = d1
	, na.action=na.exclude, drop.unused.levels=TRUE 
)
isoList <- lapply(c("services", "gender", "slumarea"), function(n){
  ordpred(mod = glmer_model_consec, n, modAns = dd)
})

isoList2 <- lapply(c("year", "hhsize"), function(n){
  ordpred(mod = glmer_model_consec, n, modAns = dd)
})


