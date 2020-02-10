#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Extract predicted effect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 11 (Sat) ----

library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))
library(ggpubr)

library(dplyr)
library(effects)
library(lme4)
library(splines)

source("../funs/ggplot_theme.R")
load("switchSingleModel.rda")

## Model
mod <- glmer_model
service_preds <- Effect("services", mod = mod)
summary(service_preds)
plot(service_preds)

statusP_preds <- Effect(c("statusP", "services"), mod = mod)
summary(statusP_preds)
plot(statusP_preds)


