#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Feb 19 (Wed)

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
	%>% select(response_vars
		, other_vars
	)
	%>% setnames(names(.), gsub(".*_hh|.*hhd|_anon|is|intvw|years", "", names(.)))
	%>% mutate(year = year-base_year)
)
str(wash_df)

### Case 1: Look for previous interview
### Try automatic suffix instead of these three manual lines

nrow(wash_df)

wash_first <- (wash_df
	%>% group_by(hhid)
	%>% filter(year == min(year))
	%>% ungroup()
)

wash_later <- (wash_df
	%>% group_by(hhid)
	%>% filter(year != min(year))
	%>% ungroup()
)

nrow(wash_first)
nrow(wash_later)

prevdat <- (wash_df
	%>% transmute(hhid = hhid
		, year = year + 1
		, watersourceP = watersource
		, toilettypeP = toilettype
		, garbagedposalP = garbagedposal
	)
)

service_step <- (wash_later
	%>% left_join(., prevdat, by = c("hhid", "year"))
)

missing_by_household <- (service_step
	%>% group_by(hhid)
	%>% summarize(
		interviews = n() # Should this be based on all hh interviews?
		, no_prev = sum(is.na(watersourceP))
	)
)
print(missing_by_household)
print(missing_by_household
	%>% summarize(
		interviews=mean(interviews)
		, no_prev=mean(no_prev)
	)
)

save(file = "householdSurveys.rda"
	, service_step
	, missing_by_household
)
