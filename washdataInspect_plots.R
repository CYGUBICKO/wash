#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)

library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))

library(forcats)
library(ggpubr)
library(scales)

source("../funs/ggplot_theme.R")
source("../funs/effectsPlot.R")

load("washdataInspect.rda")

## Input files:
### 1. wash_lagged_df - Ignore consecutive years. Lags services with years
### 2. wash_consec_df - Assumes all interviews were done consecitvely for all the years in all HH

summaryFunc <- function(var){
	summary_df <- (wash_df
		%>% select(var, year)
		%>% rename("temp_var" = var)
		%>% group_by(year)
		%>% summarise(prop = mean(temp_var))
		%>% ungroup()
		%>% mutate(services = var, overall = mean(prop))
	)
	return(summary_df)
}


#### ---- Some summaries ----

## No. of hh
nhhid <- length(unique(wash_df$hhid))
nint_all <- nrow(wash_df)

## Proportion of missingness per variable
miss_cases_df <- miss_cases_df

## No. of interviews for consecutive interviews
nint_consec <- nrow(wash_consec_df)

## HH which had missing interviews in consecutive years
prevcases_df <- (wash_consec_df
	%>% group_by(hhid)
	%>% filter(year != min(year))
	%>% mutate(nprev_miss2 = sum(is.na(watersourceP)))
	%>% ungroup()
	%>% mutate_at("hhid", as.numeric)
	%>% select(hhid, n, nprev_miss1, nprev_miss2)
	%>% distinct()
)

print(prevcases_df, n = 50, width = Inf)

### Excluding first year interviews because no previous year anyway

summary_consec_noyr0_df <- (prevcases_df
	%>% filter(nprev_miss2 > 0)
	%>% group_by(nprev_miss2)
	%>% summarise(nn = sum(nprev_miss2))
)

nint_noyr0 <- (wash_consec_df
	%>% group_by(hhid)
	%>% filter(year != min(year))
	%>% nrow()
)

percent_miss_consec_noyr0 <- percent(sum(summary_consec_noyr0_df$nn/nint_noyr0))

## Percentage of year 1 interviews
nint_yr0 <- (wash_consec_df
	%>% group_by(hhid)
	%>% filter(year == min(year))
	%>% nrow()
)
percent_year0 <- percent(sum(nint_yr0/nrow(wash_consec_df)))
percent_year0

### All missing at least one previous interviews 
summary_consec_df <- (wash_consec_df
	%>% ungroup()
	%>% select(hhid, n, nprev_miss1)
	%>% distinct()
	%>% group_by(nprev_miss1)
	%>% summarise(nn = sum(nprev_miss1))
	%>% ungroup()
)

percent_miss_consec <- percent(sum(summary_consec_df$nn/nint_consec))
percent_miss_consec


## Transition status (JD's Status Quo)

consec_temp_df <- (wash_consec_df
	%>% group_by(hhid)
	%>% filter(year != min(year))
	%>% mutate(waterSQ = case_when(watersource==0 & watersourceP==0 ~ "All -"
			, watersource==1 & watersourceP==1 ~ "All +"
			, watersourceP==1 & watersource==0 ~ "Loss"
			, watersourceP==0 & watersource==1 ~ "Gain"
			, is.na(watersourceP) & (watersource==0 | watersource==1) ~ "No prev."
		)
		, garbageSQ = case_when(garbagedposal==0 & garbagedposalP==0 ~ "All -"
			, garbagedposal==1 & garbagedposalP==1 ~ "All +"
			, garbagedposalP==1 & garbagedposal==0 ~ "Loss"
			, garbagedposalP==0 & garbagedposal==1 ~ "Gain"
			, is.na(garbagedposalP) & (garbagedposal==0 | garbagedposal==1) ~ "No prev."
		)
		, toiletSQ = case_when(toilettype==0 & toilettypeP==0 ~ "All -"
			, toilettype==1 & toilettypeP==1 ~ "All +"
			, toilettypeP==1 & toilettype==0 ~ "Loss"
			, toilettypeP==0 & toilettype==1 ~ "Gain"
			, is.na(toilettypeP) & (toilettype==0 | toilettype==1) ~ "No prev."
		)
	)
	%>% ungroup()
	%>% mutate(hhid = as.numeric(hhid))
	%>% data.frame()
)

## Lagged: Any interview before now is considered previous interview

summary_laged_df <- (wash_df
	%>% group_by(hhid)
	%>% mutate(watersourceP = lag(watersource, order_by = year)
		, toilettypeP = lag(toilettype, order_by = year)
		, garbagedposalP = lag(garbagedposal, order_by = year)
	)
	%>% mutate(n = n()
		, nprev_miss1 = sum(is.na(watersourceP))
	)
	%>% ungroup()
	%>% mutate_at("hhid", as.numeric)
	%>% select(hhid, n, nprev_miss1)
	%>% distinct()
	%>% summarise(nn = sum(nprev_miss1))
)
percent_miss_lagged <- percent(summary_laged_df$nn/nint_consec)

#### ---- Overall service proportion per year ----

service_props <- lapply(c("watersource", "garbagedposal", "toilettype"), summaryFunc)
service_props_df <- do.call(rbind, service_props)

prop_plot <- (ggplot(service_props_df, aes(x = factor(year, levels = 1:14, labels = 2002:2015), y = prop, group = services, colour = services))
	+ geom_point()
	+ geom_line()
	+ geom_hline(aes(yintercept = overall, group = services, colour = services), linetype = "dashed")
	+ geom_text(aes(x = max(year)-1, y = overall, group = services, label = paste0("Overall = ", scales::percent(overall)))
		, vjust = -1.5
		, show.legend = FALSE
	)
	+ labs(x = "Years"
		, y = "Proportions of\nimproved services"
		, colour = "Services"
	)
	+ scale_y_continuous(labels = percent, limits = c(0,1))
	+ scale_colour_discrete(breaks = c("watersource"
			, "garbagedposal"
			, "toilettype"
		)
	)
	+ theme(legend.position = "bottom"
		, plot.title = element_text(hjust = 0.5)
	)
)
print(prop_plot)


#### ---- Number of completed interviews for all the hh ----

n_intv_hh_df <- (wash_df
	%>% group_by(hhid)
	%>% summarise(n = n())
	%>% ungroup()
	%>% group_by(n)
	%>% summarise(total = n())
	%>% mutate(prop = total/sum(total))
)
print(n_intv_hh_df)

n_interviews_plot <- (ggplot(n_intv_hh_df, aes(x = as.factor(n), y = prop))
	+ geom_bar(stat = "identity")
	+ scale_y_continuous(labels = percent)
	+ labs(x = "No. of interviews", y = "Proportion")
)
print(n_interviews_plot)

#### ---- Number of completed interviews per year ----

n_intv_year_df <- (wash_df
	%>% group_by(year)
	%>% summarise(n = n())
	%>% mutate(prop = n/sum(n))
)
year_plot <- (ggplot(n_intv_year_df, aes(x = factor(year, levels = 1:14, labels = 2002:2015), group = 1, y = prop))
	+ geom_point()
	+ geom_line()
	+ scale_y_continuous(labels = percent)
	+ labs(x = "Year", y = "Interviews")
)
print(year_plot)


#### ---- Compare HH size scaled vs unscaled ----

hhsize_df <- (wash_df
	%>% select(hhsize, hhsize_unscaled)
	%>% gather(type, value)
)

hhsize_plot <- (ggplot(hhsize_df, aes(x = value))
	+ geom_histogram()
	+ facet_wrap(~type, scales = "free_x")
)
print(hhsize_plot)

#### ---- Consecutive interviews per year plot ----
consec_all_plot <- (ggplot(summary_consec_df, aes(x = as.factor(nprev_miss1), y = nn))
	+ geom_bar(stat='identity')
	+ geom_text(aes(label=paste0(percent(nn/nint_consec)))
		, stat='identity'
		, position=position_dodge(0.9)
		, vjust=-0.2
	)
	+ labs(x = "Missing consecutive interviews"
		, y = "No. of interviews"
	)
)
print(consec_all_plot)

#### ---- First year ignored consecutive interviews ----

consec_noyr0_plot <- (ggplot(summary_consec_noyr0_df, aes(x = as.factor(nprev_miss2), y = nn))
	+ geom_bar(stat='identity')
	+ geom_text(aes(label=paste0(percent(nn/nint_consec)))
		, stat='identity'
		, position=position_dodge(0.9)
		, vjust=-0.2
	)
	+ labs(x = "Missing consecutive interviews"
		, y = "No. of interviews"
	)
)
print(consec_noyr0_plot)

#### ---- Status Quo based on the previous year ----

consec_temp_df <- (consec_temp_df
	%>% select(c("waterSQ", "garbageSQ", "toiletSQ"))
	%>% gather(services, SQ)
	%>% mutate(services = gsub("SQ", "", services))
	%>% group_by(services, SQ)
	%>% summarise(n = n())
	%>% mutate(prop = n/sum(n))
	%>% ungroup()
)
print(consec_temp_df)

status_quo_plot <- (ggplot(consec_temp_df, aes(x = fct_reorder(SQ, prop), y = prop, group = services))
	+ geom_bar(stat = "identity")
	+ scale_y_continuous(labels = percent)
	+ labs(x = "Status based on previous year", y = "Proportion")
	+ facet_wrap(~services)
	+ coord_flip()
	+ facet_theme
)
print(status_quo_plot)

save(file = "washdataInspect_plots.rda" 
	, nhhid
	, nint_all
	, miss_cases_df
	, nint_consec
	, percent_miss_consec
	, percent_year0
	, percent_miss_consec_noyr0
	, percent_miss_lagged
	, prop_plot
	, status_quo_plot
	, n_interviews_plot
	, year_plot
	, hhsize_plot
	, consec_all_plot
	, consec_noyr0_plot

)
