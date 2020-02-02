#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)

load("washdataInspect.rda")
load("longDFunc.rda")

## Input files:
### 1. wash_lagged_df - Ignore consecutive years. Lags services with years
### 2. wash_consec_df - Assumes all interviews were done consecitvely for all the years in all HH

long_df <- (longDFunc(wash_consec_df)
	%>% mutate(statusP = as.factor(statusP))
)
modData <- model.frame(
	status ~ services
	+ age
	+ gender
#	+ ethnicity
	+ slumarea
	+ hhsize
	+ year
#	+ belowpovertyline
#	+ hungerscale
	+ year_scaled
	+ wealthindex
	+ statusP
	+ hhid
	, data = long_df, na.action = na.exclude, drop.unused.levels = TRUE
)

save(file = "washModeldata.rda"
	, modData
	, wash_df
	, miss_cases_df
)

