#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit MCMCglmm to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 02 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(MCMCglmm)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("service1", "service2", "service3")
nsims <- length(sim_dflist)


# Priors
priors <- list(G=list(G1 = list(V = diag(3)*1e-16, n = 1, fix = 1, alpha.mu = rep(0, 3), alpha.V  = diag(3)*1125)
	, G2 = list(V = diag(3)*0.02, n = 0, fix = 1))
)

mcmcglmmcoef_list <- list()
mcmcglmm_list <- list()

for (s in 1:nsims){
   long_df <- (sim_dflist[[s]]
      %>% select(c("hhid_anon", predictors, services))
      %>% gather(service, status, services)
   )
	model <- MCMCglmm(status ~ 0 + wealthindex:service + service
		, random = ~us(service):hhid_anon + us(service):units
		, family = "categorical"
		, data = long_df
 		, prior = priors
 		, verbose = FALSE
   )
	model_summary <- summary(model)
   mcmcglmmcoef_list[[s]] <- model_summary[["solutions"]][,1]
   mcmcglmm_list[[s]] <- model
}

mcmcglmmcoef_df <- Reduce(rbind, mcmcglmmcoef_list) %>% as_tibble()
summary(mcmcglmmcoef_df)
print(mcmcglmmcoef_df)

save(file = "binaryMcmcglmm.rda"
	, mcmcglmm_list
   , mcmcglmmcoef_df
	, predictors
	, betas_df
	, betas
)

