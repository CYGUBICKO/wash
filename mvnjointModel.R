#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit joint model ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 28 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(brms)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("simulatemvn.rda")
set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * x

nsims <- length(sim_dflist)

get_prior(
	mvbind(y1, y2, y3) ~ 0 + intercept + x + (0 + 1|p|id)
		, data = sim_dflist[[1]]
		, family = gaussian 
)

report <- 1 # Index within nsims to save for summary
brmsmodel_list <- list() 
brmscoef_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(
		mvbind(y1, y2, y3) ~ 0 + intercept + x + (0 + 1|p|id) 
			, data = df
			, family = gaussian 
			, cores = 8
			, seed = 7777
	)
	if (s <= report){
		brmsmodel_list[[s]] <- model # Model to store
	}
	brmscoef_list[[s]] <- fixef(model)[, "Estimate"]
}

brmscoef_df <- Reduce(rbind, brmscoef_list) %>% as_tibble()

save(file = "mvnjointModel.rda"
	, brmsmodel_list
	, brmscoef_df
	, betas_df
	, betas
)

