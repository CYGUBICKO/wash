#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(ggplot2)

load("analysisdata.rda")

hhsize_df <- (working_df
	%>% transmute(unscaled = numpeople_total, scaled = drop(scale(unscaled)))
	%>% gather(type, value)
)

hhsize_plot <- (ggplot(hhsize_df, aes(x = value))
	+ geom_histogram()
	+ facet_wrap(~type, scales = "free_x")
)

save(file = "hhsizeInspect.rda"
	, hhsize_plot
)
