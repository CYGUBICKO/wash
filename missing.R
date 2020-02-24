
#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

## 1.374/3.184 ## .432 ## Proportion of points with no previous
## .3741/2.184 ## .171 ## Proportion of followups unusable
## .3741/3.184 ## .118 ## Proportion of data unusable
## git_push/washdataAnalysis_report.html

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

mean_h <- (missing
	%>% select(-hhid)
	%>% summarise_all(funs(mean))
)

print(mean_h
	%>% transmute(
		no_prev = all_miss/n
		, follow_unused = real_miss/(n-1) ## Proportion of things that aren't first that we can't use
		, obs_unused = real_miss/n ## Proportion That we don't use at all
	)
)

