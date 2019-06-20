#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit bivariate models ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 28 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(brms)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

set.seed(7777)

load("simulatemvn.rda")
# * sim_dflist
# * betas_df
# * betas
# * x

nsims <- length(sim_dflist)

report <- 1 # Index within nsims to save for summary
## priors are not used (did we use defaults?)
priors <- c(prior(normal(0, 1), class = b)
	, prior(normal(0, 1), class = b, coef = x)
	, prior(normal(0, 1), class = b, coef = intercept)
	, prior(cauchy(0, 1), class = sigma)
)

y1model_list <- list()
y1coef_list <- list()

# y1
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(y1 ~ 0 + intercept + x
			, data = df
			, family = gaussian 
			, seed = 7777
			, cores = 8
	)
	if (s <= report){
		y1model_list[[s]] <- model # Model to store
	}
	y1coef_list[[s]] <- fixef(model)[, "Estimate"]
}
y1coef_df <- Reduce(rbind, y1coef_list) %>% as_tibble()

#y2
y2model_list <- list()
y2coef_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(y2 ~ 0 + intercept + x
			, data = df
			, family = gaussian 
			, seed = 7777
			, cores = 8
	)
	if (s <= report){
		y2model_list[[s]] <- model # Model to store
	}
	y2coef_list[[s]] <- fixef(model)[, "Estimate"]
}
y2coef_df <- Reduce(rbind, y2coef_list) %>% as_tibble()

#y3
y3model_list <- list()
y3coef_list <- list()
for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
   )
	model <- brm(y3 ~ 0 + intercept + x
			, data = df
			, family = gaussian 
			, seed = 7777
			, cores = 8
	)
	if (s <= report){
		y3model_list[[s]] <- model # Model to store
	}
	y3coef_list[[s]] <- fixef(model)[, "Estimate"]
}
y3coef_df <- Reduce(rbind, y3coef_list) %>% as_tibble()

bivmodel_list <- list(y1model = y1model_list
	, y2model = y2model_list
	, y3model = y3model_list
)
bivcoef_list <- list(y1coef = y1coef_list
	, y2coef = y2coef_list
	, y3coef = y3coef_list
)

save(file = "bivariateModel.rda"
	, bivmodel_list
	, bivcoef_list
	, betas_df
	, betas
)

