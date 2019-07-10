#### ---- Project: APHRC Wash Data ----
#### ---- Task: Descriptives ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jul 09 (Tue) ----

library(DT)
library(tidyr)
library(dplyr)

load("globalFunctions.rda")
load("analysisdata.rda")

#### ---- 1. Water sources ----

tab_vars <- c("hhid_anon", "intvwyear", "cat_hhwatersource")
watersource_tab <- tabsFunc(working_df, tab_vars)

##### ---- 2. Toilet type ----

tab_vars <- c("hhid_anon", "intvwyear", "cat_hhtoilettype")

##### ---- 3. Garbage disposal ----

tab_vars <- c("hhid_anon", "intvwyear", "cat_hhgarbagedisposal")

##### ---- Income ----

tab_vars <- c("hhid_anon", "intvwyear", "wealthindex")
wealthindex_tab <- propFunc(working_df, tab_vars)

## Income only
tab_vars <- c("hhid_anon", "wealthindex")
wealthindex_hh_tab <- propFunc(working_df, tab_vars)

##### ---- Save output ----

save(file = "additional_summaries.rda"
	, watersource_tab
	, wealthindex_tab
	, wealthindex_hh_tab
)

