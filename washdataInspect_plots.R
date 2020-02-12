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
)
n_interviews_plot <- (ggplot(n_intv_hh_df, aes(x = n))
	+ geom_bar(stat = "count")
	+ stat_bin(binwidth = 1, geom="text", size = 2, color='white'
		, aes(label = paste0(percent(..count../sum(..count..), accuracy = 0.1)))
		, position=position_stack(vjust = 0.5)	
	)
	+ labs(x = "No. of interviews")
)
print(n_interviews_plot)

#### ---- Number of completed interviews per year ----

n_intv_year_df <- (wash_df
	%>% group_by(year)
	%>% summarise(n = n())
	%>% mutate(prop = n/sum(n))
)
print(ggplot(n_intv_year_df, aes(x = factor(year, levels = 1:14, labels = 2002:2015), group = 1, y = prop))
	+ geom_point()
	+ geom_line()
	+ scale_y_continuous(labels = percent)
	+ labs(x = "Year", y = "Interviews")
)


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
