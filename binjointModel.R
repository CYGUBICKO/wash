#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit joint model: Binanry response ----
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
	mvbind(y1bin, y2bin, y3bin) ~ 0 + intercept + x + (0 + 1|p|id)
		, data = sim_dflist[[1]]
		, family = bernoulli 
)

report <- 3 # Index within nsims to save for summary

priors <- c(
#	prior(normal(0, 1), class = b, resp = y1bin)
#	, prior(normal(0, 1), class = b, resp = y2bin)
#	, prior(normal(0, 1), class = b, resp = y3bin)
#	prior(normal(0, 1), class = b, coef = intercept, resp = y1bin)
#	, prior(normal(0, 1), class = b, coef = intercept, resp = y2bin)
#	, prior(normal(0, 1), class = b, coef = intercept, resp = y3bin)
#	, prior(normal(0, 1), class = b, coef = x, resp = y1bin)
#	, prior(normal(0, 1), class = b, coef = x, resp = y2bin)
#	, prior(normal(0, 1), class = b, coef = x, resp = y3bin)
#	, prior(normal(0, 1), class = sd, resp = y1bin)
#	, prior(normal(0, 1), class = sd, resp = y2bin)
#	, prior(normal(0, 1), class = sd, resp = y3bin)
#	, prior(normal(0, 1), class = sd, group = id, resp = y1bin)
#	, prior(normal(0, 1), class = sd, group = id, resp = y2bin)
#	, prior(normal(0, 1), class = sd, group = id, resp = y3bin)
	prior(normal(0, 1), class = sd, coef = Intercept, group = id, resp = y1bin)
	, prior(normal(0, 1), class = sd, coef = Intercept, group = id, resp = y2bin)
	, prior(normal(0, 1), class = sd, coef = Intercept, group = id, resp = y3bin)
)
brmsmodel_list <- list() 
brmscoef_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(
		mvbind(y1bin, y2bin, y3bin) ~ 0 + intercept + x + (1|p|id) 
			, data = df
			, family = bernoulli 
			, cores = 8
			, seed = 7777
			, prior = priors
	)
	if (s <= report){
		brmsmodel_list[[s]] <- model # Model to store
	}
	brmscoef_list[[s]] <- fixef(model)[, "Estimate"]
}

brmscoef_df <- Reduce(rbind, brmscoef_list) %>% as_tibble()
summary(brmsmodel_list[[1]])
summary(brmsmodel_list[[2]])

save(file = "binjointModel.rda"
	, brmsmodel_list
	, brmscoef_df
	, betas_df
	, betas
)

