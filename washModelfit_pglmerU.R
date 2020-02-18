#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(splines)
library(dplyr)
library(lme4)

load("washModeldata.rda")

## Input files: modData data frame for fitting model and wash original data

## UScaled year
pyearmodData_unscaled <- (prevyearmodData
	%>% mutate(hhsize = hhsize_unscaled)
	%>% select(-c("year_scaled", "hhsize_unscaled"))
)

## Model formula
fixed_effects <- paste0(c("-1"
		, "services" 
		, "(ns(age, 3)"
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
pglmer_unscaled <- glmer(model_form
	, data = pyearmodData_unscaled
	, family = binomial(link = "logit")
	, control = glmerControl(optimizer="bobyqa")
)

save(file = "washModelfit_pglmerU.rda"
	, pglmer_unscaled
	, pyearmodData_unscaled
	, model_form
)

