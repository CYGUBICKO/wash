#### ---- Project: APHRC Wash Data ----
#### ---- Task: Extract just needed ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Sep 06 (Fri) ----

library(ggplot2)
library(rstanarm)

load("rstanarmModelbin.rda")

##### ---- Extract some key summaries ----
rstanmodel <- rstanmodel_list[[1]]

plot_df <- plot(rstanmodel)[["data"]]

##### ---- Save output ----

save(file = "summary_plot_data.rda"
	, plot_df
	, rstanmodel
	, betas_df
	, betas
)

