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

model_form <- as.formula(
	status ~ -1 
	+ (services + (ns(age, 3) + gender + slumarea + year + statusP) * ns(wealthindex, 3) + hhsize):services
	+ (services-1|hhid)
)

## Fit glmer model
glmer_inter <- glmer(model_form
	, data = modData_scaled
	, family = binomial(link = "logit")
	, control = glmerControl(optimizer="nloptwrap"
#		, optCtrl=list(maxfun=2e5)
	)
)

save(file = "washModelfit_inter_glmerS.rda"
	, glmer_inter
	, modData_scaled
	, model_form
)

