#### ---- Project: APHRC Wash Data ----
#### ---- Task: Extract just needed ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Sep 06 (Fri) ----

library(DT)
library(data.table)
library(tibble)
library(tidyr)
library(dplyr)

load("simulateHierarchicalmvn.rda")

##### ---- Extract some key summaries ----
nhouseholds <- length(unique(sim_dflist[[1]]$hhid))
nyears <- length(unique(sim_dflist[[1]]$years))
sims_df <- (sim_dflist[[1]]
	%>% filter(hhid %in% sample(hhid, 5))
	%>% mutate_at(c("y1", "y2", "y3", "wealthindex"), function(y){round(y, 3)})
	%>% datatable(caption = "Simulated dataset", rownames = FALSE)
)


##### ---- Save output ----

save(file = "extract_summaries.rda"
	, nhouseholds
	, nyears
	, sims_df
)

