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

## Scaled year
modData_scaled <- (modData
	%>% mutate(year = year_scaled)
	%>% select(-year_scaled)
)


## Model formula
fixed_effects <- paste0(c("-1"
		, "(services" 
		, "age"
		, "gender"
		, "slumarea"
		, "hhsize"
		, "year"
		, "wealthindex"
		, "statusP):services"
	)
	, collapse = "+"
)
rand_effects <- "(services-1|hhid)"
model_form <- as.formula(paste0("status ~ ", fixed_effects, " + ", rand_effects))

## Fit glmer model
glmer_linear <- glmer(model_form
	, data = modData_scaled
	, family = binomial(link = "logit")
)

save(file = "washModelfit_glmerL.rda"
	, glmer_linear
	, modData_scaled
	, model_form
)

