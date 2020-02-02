#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(splines)
library(dplyr)
library(tidyr)
library(tibble)
library(brms)

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

get_prior(model_form
	, family = bernoulli(link = "logit")
	, data = modData_scaled
)

## Priors
priors <- c(prior(normal(0, 5), class = b)
	, prior(cauchy(0, 5), class = sd, group = hhid)
	, set_prior("lkj(1)", class = "cor")
)

## Fit brms model
brms_scaled <- brm(model_form
	, data = modData_scaled
	, family = bernoulli(link = "logit")
	, warmup = 1e3
	, iter = 1e4
	, chains = 4
	, cores = parallel::detectCores()
	, control = list(adapt_delta = 0.95)
	, seed = 7777
	, prior = priors
)

save(file = "washModelfit_brmsS.rda"
	, brms_scaled
	, modData_scaled
	, model_form
)
