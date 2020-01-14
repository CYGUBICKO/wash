#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(splines)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lme4)

load("washModeldata.rda")

## Input files: modData data frame for fitting model and wash original data

## UScaled year
modData_unscaled <- (modData
	%>% select(-year_scaled)
)

## Model formula
fixed_effects <- paste0(c("-1"
		, "(services" 
		, "ns(age, 3)"
		, "gender"
		, "slumarea"
		, "hhsize"
		, "year"
		, "ns(wealthindex, 3)"
		, "statusP):services"
	)
	, collapse = "+"
)
rand_effects <- "(services-1|hhid)"
model_form <- as.formula(paste0("status ~ ", fixed_effects, " + ", rand_effects))

## Fit glmer model
glmer_unscaled <- glmer(model_form
	, data = modData_unscaled
	, family = binomial(link = "logit")
	, control = glmerControl(optimizer="bobyqa"
#		, optCtrl=list(maxfun=2e5)
	)
)

save(file = "washModelfit_glmerU.rda"
	, glmer_unscaled
	, modData_unscaled
	, model_form
)

