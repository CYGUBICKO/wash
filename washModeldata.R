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
### 1. wash_df: Original dataset
### 2. wash_consec_df - Assumes all interviews were done consecitvely for all the years in all HH

## Function to convert data frame to model frame
frameFun <- function(df){
	d <- model.frame(
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
		, data = df, na.action = na.exclude, drop.unused.levels = TRUE
	)
	return(d)
}


## Restructured data
long_df <- longDFunc(wash_consec_df)

## Year 1 data
year1_df <- (long_df[["year1_df"]]
	%>% group_by(hhid)
	%>% filter(year == min(year))
	%>% ungroup()
)

year1modData <- model.frame(
	status ~ services
	+ age
	+ gender
#	+ ethnicity
	+ slumarea
	+ hhsize
	+ hhsize_unscaled
	+ year
#	+ belowpovertyline
#	+ hungerscale
	+ year_scaled
	+ wealthindex
	+ hhid
	, data = year1_df, na.action = na.exclude, drop.unused.levels = TRUE
)

## Previous year based data
prev_df <- (long_df[["prev_df"]]
	%>% group_by(hhid)
	%>% filter(year != min(year))
	%>% ungroup()
)

prevyearmodData <- model.frame(
	status ~ services
	+ age
	+ gender
#	+ ethnicity
	+ slumarea
	+ hhsize_unscaled
	+ hhsize
	+ year
#	+ belowpovertyline
#	+ hungerscale
	+ year_scaled
	+ wealthindex
	+ statusP
	+ hhid
	, data = prev_df, na.action = na.exclude, drop.unused.levels = TRUE
)

save(file = "washModeldata.rda"
	, year1modData
	, prevyearmodData
)

