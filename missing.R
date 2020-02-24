
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
load("washdataInspect.rda")

missing <- (wash_consec_df
	%>% group_by(hhid)
	%>% summarise(n=n()
		, all_miss = sum(is.na(watersourceP))
		, real_miss = sum(is.na(watersourceP) & year != min(year))
	)
)

summary(missing)
