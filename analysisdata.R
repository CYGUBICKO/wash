#### ---- Project: APHRC Wash Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 9 (Sat) ----

library(dplyr)

load("globalFunctions.rda")
load("complete.rda")
options(dplyr.width = Inf)

# This script select only variables which will be useful in the subsequent analysis

#### ---- Some additional cleaning ----

wash_vars <- grep("^cat_", colnames(working_df), value = TRUE)

## Convert Improved and Unimproved to 1 and 0, respectively:

patterns <- c("^improved", "unimproved")
replacements <- c(1, 0)
working_df <- (working_df
	%>% mutate(intvwyear = as.numeric(levels(intvwyear))[intvwyear])
	%>% recodeLabs(wash_vars, patterns, replacements, insert = FALSE)
	%>% mutate_at(wash_vars, as.numeric)
	%>% rowsumFunc(wash_vars, "total_wash_indicators")
	%>% mutate(wash_access_rate = total_wash_indicators/(length(wash_vars)))
	%>% mutate(expend_total_USD_per_centered = scale(as.numeric(expend_total_USD_per), scale = FALSE))
	%>% mutate_at("expend_total_USD_per_centered", as.numeric)
)

#### ---- Select variables for analysis ----

cluster_vars <- c("hhid_anon", "intvwyear", "slumarea")
demographic_vars <- c("gender", "ethnicity", "ageyears", "numpeople_total")
social_vars <- c("isbelowpovertyline", "hhdhungerscale")
economic_vars <- c("wealthindex", "expend_total_USD_per_centered", "wealthquintile")
added_wash_vars <- c("total_wash_indicators", "wash_access_rate")

working_df <- (working_df
	%>% select(
		c(cluster_vars
			, demographic_vars
			, social_vars
			, economic_vars
			, wash_vars
			, added_wash_vars
		)
	)
)

summary(working_df)

print(head(working_df))

df_outpath <- paste0("data/", "wash_analysis_df.rds")
saveRDS(working_df, df_outpath)
write.csv(codebook, "data/codebook.csv")

save(file = "analysisdata.rda"
	, working_df
	, codebook
	, wash_vars
)
