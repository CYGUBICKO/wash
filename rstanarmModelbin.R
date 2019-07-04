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
# * x

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
			y1bin ~ x + (1 | id)
			, y2bin ~ x + (1 | id)
			, y3bin ~ x + (1 | id)
		)
		, data = sim_dflist[[1]]
		, refresh = 0
		, prior_intercept = normal(0, 10)
		, prior = normal(0, 10)
		, prior_aux = cauchy(0, 2)
		, prior_covariance = lkj(1)
		, family = list(binomial, binomial, binomial)
		, chains = parallel::detectCores()
		, cores = parallel::detectCores()
		, seed = 7777
		, iter = 2000
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

save(file = "rstanarmModelbin.rda"
	, rstanmodel_list
	, rstancoef_df
	, betas_df
	, betas
)

