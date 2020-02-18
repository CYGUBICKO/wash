#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data: Year 1 ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(splines)
library(dplyr)

load("washModeldata.rda")

## Input files: modData data frame for fitting model and wash original data

## Scaled year
year1modData_scaled <- (year1modData
	%>% mutate(year = year_scaled)
	%>% select(-year_scaled)
)

## Model formula
fixed_effects <- paste0(c("-1"
		, "services" 
		, "(ns(age, 3)"
		, "gender"
		, "slumarea"
		, "hhsize"
		, "year"
		, "ns(wealthindex, 3)):services"
	)
	, collapse = "+"
)
model_form <- as.formula(paste0("status ~ ", fixed_effects))

## Fit glmer model
y1glm_scaled <- glm(model_form
	, data = year1modData_scaled
	, family = binomial
)

save(file = "washModelfit_y1glmS.rda"
	, y1glm_scaled
	, year1modData_scaled
	, model_form
)

