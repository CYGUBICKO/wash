#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Tidy Model estimates ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(broom.mixed)

library(data.table)
library(lme4)

load("washModelfit.rda")
load("washModelfit_glmmTMB.rda")

unscaled_glmer = glmer_model_consec
scaled_glmer = glmer_model_consec_scaledyr
unscaled_TMB = glmmTMB_model_consec
scaled_TMB = glmmTMB_model_consec_scaledyr
extract_coefs_df <- (map(list(unscaled_glmer = unscaled_glmer
		, scaled_glmer = scaled_glmer
		, unscaled_TMB = unscaled_TMB
		, scaled_TMB = scaled_TMB
	)
		, tidy
		, conf.int = TRUE
	)
	%>% bind_rows(.id = "model")
	%>% mutate(term = factor(term, levels = unique(term))
		, term = gsub("services", "", term)
		, parameter = ifelse(!grepl("\\:|\\.|^cor|^sd", term), "Services", term)
		, parameter = ifelse(grepl("^cor|^sd", parameter)
			, paste0(gsub("\\_.*", "", parameter), "_", group)
			, gsub(".*\\:", "", parameter)
		)
		, term = gsub("\\:.*|\\.hhid|\\.year|.*\\_", "", term)
	)
#	%>% filter(term != "(Intercept)")
	%>% mutate(term = reorder(term, estimate))
)

print(extract_coefs_df, n = Inf, width = Inf)

save(file = "washTidyestimates.rda"
	, extract_coefs_df
	, unscaled_glmer
	, unscaled_TMB
	, scaled_glmer
	, scaled_TMB
)
