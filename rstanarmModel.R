#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit joint model ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jul 04 (Thu) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(rstanarm)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("simulateHierarchicalmvn.rda")
set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * wealthindex

nsims <- length(sim_dflist)

report <- 1 # Index within nsims to save for summary
rstanmodel_list <- list() 
rstancoef_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- stan_mvmer(
		formula = list(
			y1 ~ wealthindex + (1 | years)
			, y2 ~ wealthindex + (1 | years)
			, y3 ~ wealthindex + (1 | years)
		)
		, data = sim_dflist[[1]]
		, family = list(gaussian, gaussian, gaussian)
		, refresh = 0
		, chains = parallel::detectCores()
		, cores = parallel::detectCores()
		, seed = 7777
		, iter = 1000
	)
	if (s <= report){
		rstanmodel_list[[s]] <- model # Model to store
	}
	rstancoef_list[[s]] <- fixef(model)
}

# Print results
rstancoef_df <- Reduce(rbind, rstancoef_list) %>% as_tibble()
print(betas_df)
print(rstancoef_df)

summary(rstanmodel_list[[1]])


VarCorr(rstanmodel_list[[1]])

save(file = "rstanarmModel.rda"
	, rstanmodel_list
	, rstancoef_df
	, betas_df
	, betas
)

