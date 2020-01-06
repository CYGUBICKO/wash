#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Tidy Model estimates ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(broom)

library(data.table)
library(lme4)

load("washModelfit.rda")

extract_coefs_df <- (map(list(#lagged = glmer_model_lagged
#		, lagged_randYear0 = glmer_model_lagged_ryr0
#		, lagged_vyears = glmer_model_lagged_accross_years
		consec = glmer_model_consec
#		, consec_randYear0 = glmer_model_consec_ryr0
	)
		, tidy
		, conf.int = TRUE
	)
	%>% bind_rows(.id = "model")
	%>% mutate(term = factor(term, levels = unique(term))
		, term = gsub("services", "", term)
		, parameter = ifelse(!grepl("\\:|\\.", term), "Overall", term)
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
#	, glmer_model_lagged
	, glmer_model_consec
)
