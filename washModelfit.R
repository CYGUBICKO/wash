#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(splines)
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lme4)

load("washdataInspect.rda")

## Input files:
### 1. wash_lagged_df - Ignore consecutive years. Lags services with years
### 2. wash_consec_df - Assumes all interviews were done consecitvely for all the years in all HH

# Two step long format restructure

longDFunc <- function(df){
	temp_df1 <- (df
		%>% gather(services, status, c("watersource", "toilettype", "garbagedposal"))
	)

	temp_df2 <- (df
		%>% select(hhid, year, watersourceP, toilettypeP, garbagedposalP)
		%>% gather(serviceP, statusP, c("watersourceP", "toilettypeP", "garbagedposalP"))
		%>% mutate_at("serviceP", function(x)gsub("P", "", x))
	)
	
	long_df <- (temp_df1
		%>% full_join(temp_df2, by = c("hhid", "year", c(services = "serviceP")))	
	)
	
	return(long_df)
}

## Model formula
fixed_effects <- paste0(c("-1"
		, "(services" 
		, "ns(age, 3)"
		, "gender"
#		, "ethnicity"
		, "slumarea"
		, "hhsize"
		, "year"
#		, "belowpovertyline"
#		, "hungerscale"
		, "ns(wealthindex, 3)"
		, "statusP):services"
	)
	, collapse = "+"
)
rand_effects <- paste0("(", "services-1", "|", "hhid", ")")
model_form <- as.formula(paste0("status ~ ", fixed_effects, " + ", rand_effects))

#### Model 1: Case 1 above

lagged_long_df <- longDFunc(wash_lagged_df)
#print(lagged_long_df, n = 50, width = Inf)

#glmer_model_lagged <- glmer(model_form
#	, data = lagged_long_df
#	, family = binomial(link = "logit")
#	, control = glmerControl(optimizer="bobyqa")
#)

#### Model 2: Case 2 above

consec_long_df <- longDFunc(wash_consec_df)

glmer_model_consec <- glmer(model_form
	, data = consec_long_df
	, family = binomial(link = "logit")
	, control = glmerControl(optimizer="bobyqa")
)


save(file = "washModelfit.rda"
#	, glmer_model_lagged
	, glmer_model_consec
)

