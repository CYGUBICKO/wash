#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Extract predicted effect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 11 (Sat) ----

library(effects)
library(lme4)
library(splines)

# load("washModelfit_pglmerS.rda")
load("washModelfit_y1glmS.rda")

## Year 1 (first year) model

y1mod <- y1glm_scaled

### Service level predictions
y1service_effect_df <- Effect("services", mod = y1mod)
y1service_effect_df <- as.data.frame(y1service_effect_df)

### Conditionaled on all other predictors
y1mod_effect_df <- predictorEffects(y1mod)
y1mod_effect_df <- as.data.frame(y1mod_effect_df)
# d1 <- as.data.frame(allEffects(y1mod)) # The same (predictorEffects gives smooth curves)

## Previous year model
# mod <- pglmer_scaled

save(file = "washPredEffects.rda"
	, y1service_effect_df
	, y1mod_effect_df
)
