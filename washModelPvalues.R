#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Estimating variable level pvalues ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 10 (Fri) ----

library(car)

load("washModelfit.rda")

varlvlglmer <- Anova(glmer_model_consec_scaledyr)
print(varlvlglmer)

save(file = "washModelPvalues.rda"
	, glmer_model_consec_scaledyr
	, consec_long_df_scaledyr
	, varlvlglmer
)
