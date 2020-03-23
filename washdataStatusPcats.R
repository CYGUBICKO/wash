#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Mar 22 (Sun) ----

library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(ggplot2)

load("washdataInspect.rda")

# Recode cases with missing previous to 
## 1. Base year: status of service in the first HH year
## 2. Not observed: immediate preceding year not observed
## 3. Unimproved: Observed but unimproved service
## 4. Improved: Observed improved service

## Input files: wash_consec_df wash_df

statusPfunc <- function(x){
	x <- ifelse(is.na(x), "Not observed"
		, ifelse(x==0, "Unimproved"
			, ifelse(x==1, "Improved", x)
		)
	)
}

statusP_vars <- c("watersourceP", "toilettypeP", "garbagedposalP")

wash_consec_df <- (wash_consec_df
	%>% group_by(hhid)
	%>% mutate(watersourceP = ifelse(year==min(year), "Base year", watersourceP)
			, toilettypeP = ifelse(year==min(year), "Base year", toilettypeP)
			, garbagedposalP = ifelse(year==min(year), "Base year", garbagedposalP)
	)
	%>% ungroup()
	%>% mutate_at(statusP_vars, statusPfunc)
	%>% mutate_at("hhid", as.numeric)
	%>% select(hhid, year, watersource, watersourceP)
)

print(wash_consec_df, n = 50)

## Key variables put in categories

save(file = "washdataStatusPcats.rda"
	, wash_consec_df
	, miss_cases_df
	, wash_df
)
