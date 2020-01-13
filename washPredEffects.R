#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Extract predicted effect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 11 (Sat) ----

library(effects)
library(lme4)
library(splines)

load("washModelfit.rda")

mod <- glmer_model_consec_scaledyr
mod_effect_df <- predictorEffects(mod)
mod_effect_df <- as.data.frame(mod_effect_df)

save(file = "washPredEffects.rda"
	, mod_effect_df
)
