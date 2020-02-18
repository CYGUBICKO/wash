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

load("washModelfit_pglmerS.rda")
load("washModelfit_pglmerU.rda")
load("washModelfit_y1glmS.rda")
load("washModelfit_y1glmU.rda")

glmerScaled <- pglmer_scaled
glmerUnscaled <- pglmer_unscaled

glmScaled <- y1glm_scaled
glmUnscaled <- y1glm_unscaled

extract_coefs_df <- (map(list(glmerScaled = glmerScaled
		, glmerUnscaled = glmerUnscaled
		, glmScaled = glmScaled
		, glmUnscaled = glmUnscaled
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
	%>% mutate(term = reorder(term, estimate)
		, parameter = ifelse(grepl("glmSc|glmUn", model)
			, gsub("gain", "level", parameter)
			, parameter
		) 
	)
)

print(extract_coefs_df, n = Inf, width = Inf)

save(file = "washTidyestimates.rda"
	, extract_coefs_df
)
