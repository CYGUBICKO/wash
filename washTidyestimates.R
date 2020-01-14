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
library(glmmTMB)
library(lme4)

load("washModelfit_glmerS.rda")
load("washModelfit_glmerU.rda")
load("washModelfit_tmbS.rda")
load("washModelfit_tmbU.rda")

glmerScaled <- glmer_scaled
glmerUnscaled <- glmer_unscaled
tmbScaled <- tmb_scaled
tmbUnscaled <- tmb_unscaled
extract_coefs_df <- (map(list(glmerScaled = glmerScaled
		, glmerUnscaled = glmerUnscaled
		, tmbScaled = tmbScaled
		, tmbUnscaled = tmbUnscaled
	)
		, tidy
		, conf.int = TRUE
	)
	%>% bind_rows(.id = "model")
	%>% mutate(term = factor(term, levels = unique(term))
		, term = gsub("services", "", term)
		, parameter = ifelse(!grepl("\\:|\\.|^cor|^sd", term), "Service gain", term)
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
	, glmerScaled
	, glmerUnscaled
	, tmbScaled
	, tmbUnscaled
)
