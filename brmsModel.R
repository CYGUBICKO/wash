#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit MCMCglmm to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 02 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(brms)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateMvariate.rda")
set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

nsims <- length(sim_dflist)


get_prior(
	mvbind(service1, service2, service3) ~ 0 + intercept + wealthindex + (0 + 1|p|hhid_anon)
		, data = sim_dflist[[1]]
		, family = list(bernoulli(link = "logit"), bernoulli(link = "logit"), bernoulli(link = "logit")) 
)

report <- 2 # Index within nsims to save for summary
brmsmodel_list <- list() 
brmscoef_list <- list()
prior <- c(
	prior(normal(0, 1), class = b, coef = intercept, resp = service1)
	, prior(normal(0, 1), class = b, coef = intercept, resp = service2)
	, prior(normal(0, 1), class = b, coef = intercept, resp = service3)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = service1)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = service2)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = service3)
	, prior(inv_gamma(3, 1), class = sd, coef = Intercept, group = hhid_anon, resp = service1)
	, prior(inv_gamma(3, 1), class = sd, coef = Intercept, group = hhid_anon, resp = service2)
	, prior(inv_gamma(3, 1), class = sd, coef = Intercept, group = hhid_anon, resp = service3)
)

for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(
		mvbind(service1, service2, service3) ~ 0 + intercept + wealthindex + (0 + 1|p|hhid_anon)
			, data = df
			, cores = 8
			, family = list(bernoulli(link = "logit"), bernoulli(link = "logit"), bernoulli(link = "logit")) 
			, iter = 2e4
			, chains = 4
			, control = list(adapt_delta = 0.95)
			, seed = 7777
			, prior = prior
	)
	if (s <= report){
		brmsmodel_list[[s]] <- model # Model to store
	}
	brmscoef_list[[s]] <- fixef(model)[, "Estimate"]
}

brmscoef_df <- Reduce(rbind, brmscoef_list) %>% as_tibble()

# Align Beta df with the estimates
betas_df <- (betas_df
   %>% mutate(coef = ifelse(grepl("wealth", coef), paste0("b_service", n, "_wealthindex")
         , ifelse(grepl("^services", coef), paste0("b_service", n, "_intercept")
            , ifelse(grepl("_sd", coef), paste0("sd_hhid_anon__service", n, "_Intercept")
            , paste0("cor_hhid_anon__service", substr(n,1,1), "_Intercept__service", substr(n,2,2), "_Intercept"))
         )
      )
   )
)

save(file = "brmsModel.rda"
	, brmsmodel_list
	, brmscoef_df
	, predictors
	, betas_df
	, betas
)

