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
pyearmodData_scaled <- (prevyearmodData
	%>% mutate(year = year_scaled)
	%>% select(-year_scaled)
)

model_form <- as.formula(
	status ~ -1 
	+ (services + (ns(age, 3) + gender + slumarea + year + statusP) * ns(wealthindex, 3) + hhsize):services
	+ (services-1|hhid)
)

## Fit glmer model
pglmer_inter <- glmer(model_form
	, data = pyearmodData_scaled
	, family = binomial(link = "logit")
	, control = glmerControl(optimizer="bobyqa")
)

save(file = "washModelfit_inter_pglmerS.rda"
	, pglmer_inter
	, pyearmodData_scaled
	, model_form
)

