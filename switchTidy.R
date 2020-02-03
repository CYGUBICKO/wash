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
library(ggplot2)

load("switchSingleModel.rda")

extract_coefs_y1 <- (tidy(y1_model, conf.int = TRUE)
	%>% mutate(parameter = ifelse(grepl("^\\(Intercept", term), "b_gain", term)
		, parameter = ifelse(grepl("y1p1", parameter), "b_add", parameter)
	)
)
print(data.frame(extract_coefs_y1))



extract_coefs_df <- (tidy(glmer_model, conf.int = TRUE)
	%>% mutate(term = factor(term, levels = unique(term))
		, term = gsub("services", "", term)
		, parameter = ifelse(!grepl("\\:|\\.|^cor|^sd", term), "b_gain", term)
		, parameter = ifelse(grepl("^cor|^sd", parameter)
			, paste0(gsub("\\_.*", "", parameter), "_", group)
			, gsub(".*\\:", "", parameter)
		)
		, parameter = ifelse(grepl("statusP1", parameter), "b_add", parameter)
		, term = gsub("\\:.*|\\.hhid|\\.year|.*\\_", "", term)
	)
	%>% mutate(term = reorder(term, estimate))
)

print(extract_coefs_df, n = Inf, width = Inf)


save(file = "switchTidy.rda"
	, extract_coefs_df
	, extract_coefs_y1
)
