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

set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("service1", "service2", "service3")
nsims <- 1 #length(sim_dflist)

multimcmcglmmcoef_list <- list()
multimcmcglmm_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )

	# Expanded priors
	sig_services <- (df
		%>% select(services)
		%>% apply(., 2, function(x) var(x, na.rm = TRUE))
	)
	B.prior <- list(V=diag(6)*1e7, mu=rep(0,6))
	priors <- list(
		B = B.prior
		, R = list(V = diag(3)*sig_services*0.8, nu = 2)
		, G = list(
			G1=list(V = diag(3)*sig_services*0.15, nu = 0.001, alpha.mu = c(0,0,0), alpha.V = diag(3)*625)
		)
	)

	tryCatch({
		model <- MCMCglmm(cbind(service1, service2, service3) ~ wealthindex:trait + trait - 1
			, random = ~us(trait):hhid_anon
#			, rcov = ~us(trait):units
			, family = c("categorical", "categorical", "categorical")		
			, data = df
			, prior = priors
			, slice = TRUE
			, trunc = TRUE
			, verbose = FALSE
			, thin   = 20
			, burnin = 5000
			, nitt   = 30000
		)
		model_summary <- summary(model)
		multimcmcglmmcoef_list[[s]] <- model_summary[["solutions"]][,1]
		multimcmcglmm_list[[s]] <- model
	}
	, error = function(e){print(e)}
	)
}

multimcmcglmmcoef_df <- Reduce(rbind, multimcmcglmmcoef_list) %>% as_tibble()
summary(multimcmcglmmcoef_df)
print(multimcmcglmmcoef_df)

save(file = "multiMcmcglmm.rda"
	, multimcmcglmm_list
   , multimcmcglmmcoef_df
	, predictors
	, betas_df
	, betas
)

