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
library(glmmTMB)

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
		%>% data.frame()
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
rand_effects <- paste0("(services-1|hhid)") #, " + ", "(services-1|year)")
model_form <- as.formula(paste0("status ~ ", fixed_effects, " + ", rand_effects))


#### Model 2: Case 2 above

consec_long_df <- longDFunc(wash_consec_df)
head(consec_long_df, n = 50)

glmmTMB_model_consec <- glmmTMB(model_form
	, data = consec_long_df
	, family = list(family="binomial",link="logit")
)


# Model 2b: standardised year
consec_long_df_scaledyr <- (consec_long_df
	%>% mutate(year = year_scaled)
)
glmmTMB_model_consec_scaledyr <- glmmTMB(model_form
	, data = consec_long_df_scaledyr
	, family = list(family="binomial",link="logit")
)

save(file = "washModelfit_glmmTMB.rda"
#	, glmmTMB_model_lagged
	, glmmTMB_model_consec
	, glmmTMB_model_consec_scaledyr
	, consec_long_df
	, consec_long_df_scaledyr
	, model_form
)

