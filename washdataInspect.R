#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lme4)
library(ggplot2)

load("globalFunctions.rda")
load("analysisdata.rda")

## Input files: cleaned working_df

## Key variables put in categories
response_vars <- c("cat_hhwatersource", "cat_hhtoilettype", "cat_hhgarbagedisposal")
demo_vars <- c("ageyears", "gender", "ethnicity", "slumarea", "numpeople_total")
socio_vars <- c("isbelowpovertyline", "hhdhungerscale", "wealthindex")
other_vars <- c("intvwyear", "hhid_anon")

base_year = 2001
wash_df <- (working_df
	%>% ungroup()
	%>% select(response_vars
		, demo_vars
		, socio_vars
		, other_vars
	)
	%>% setnames(names(.), gsub(".*_hh|.*hhd|_anon|is|intvw|years", "", names(.)))
	%>% setnames("numpeople_total", "hhsize")
	%>% mutate(year = year-base_year
		, year_scaled = drop(scale(year))
		, hhsize_unscaled = hhsize
	)
	%>% mutate_at(c("age", "wealthindex", "hhsize"), function(x){x = drop(scale(x))})
)
str(wash_df)


## Missing cases
miss_cases_df <- (wash_df
	%>% missPropFunc()
	%>% arrange(desc(miss_count))
)

## Restructure the data to have the services in current and previous year in a row per hhid

### Case 1: Adjust for missing consecutive interviews
prevdat <- (wash_df
	%>% transmute(hhid = hhid
		, year = year + 1
		, watersourceP = watersource
		, toilettypeP = toilettype
		, garbagedposalP = garbagedposal
	)
)

wash_consec_df <- (wash_df
	%>% left_join(prevdat, by = c("hhid", "year"))
	%>% group_by(hhid)
	%>% mutate(n = n()
		, nprev_miss1 = sum(is.na(watersourceP))
	)
	%>% ungroup()
)
print(wash_consec_df, n = 50, width = Inf)

save(file = "washdataInspect.rda"
	, wash_consec_df
	, miss_cases_df
	, wash_df
)
