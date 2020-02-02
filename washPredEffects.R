#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Extract predicted effect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 11 (Sat) ----

library(effects)
library(lme4)
library(splines)

load("washModelfit_glmerS.rda")

mod <- glmer_scaled
mod_effect_df <- predictorEffects(mod)
mod_effect_df <- as.data.frame(mod_effect_df)

save(file = "washPredEffects.rda"
	, mod_effect_df
	, modData_scaled
	, model_form
)
