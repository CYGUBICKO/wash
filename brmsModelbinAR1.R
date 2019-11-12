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

load("simulateHierarchicalmvnAR1.rda")
set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

nsims <- length(sim_dflist)


get_prior(
	mvbind(y1bin, y2bin, y3bin) ~ 0 + intercept + wealthindex + (0 + 1|g|years) +  (0 + 1|p|hhid)
		, autocor = list(cor_ar(form = ~1, p = 1, cov = TRUE), cor_ar(form = ~1, p = 1, cov = TRUE), cor_ar(form = ~1, p = 1, cov = TRUE))
		, data = sim_dflist[[1]]
		, family = list(bernoulli(link = "logit"), bernoulli(link = "logit"), bernoulli(link = "logit")) 
)

report <- 1 # Index within nsims to save for summary
brmsmodel_list <- list() 
brmscoef_list <- list()
priors <- c(prior(normal(0, 1), class = b, resp = y1bin)
	, prior(normal(0, 1), class = b, resp = y2bin)
	, prior(normal(0, 1), class = b, resp = y3bin)
	, prior(normal(0, 1), class = b, coef = intercept, resp = y1bin)
	, prior(normal(0, 1), class = b, coef = intercept, resp = y2bin)
	, prior(normal(0, 1), class = b, coef = intercept, resp = y3bin)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = y1bin)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = y2bin)
	, prior(normal(0, 1), class = b, coef = wealthindex, resp = y3bin)
	, prior(cauchy(0, 5), class = sd, resp = y1bin)
	, prior(cauchy(0, 5), class = sd, resp = y2bin)
	, prior(cauchy(0, 5), class = sd, resp = y3bin)
	, prior(cauchy(0, 5), class = sd, group = hhid, resp = y1bin)
	, prior(cauchy(0, 5), class = sd, group = hhid, resp = y2bin)
	, prior(cauchy(0, 5), class = sd, group = hhid, resp = y3bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = hhid, resp = y1bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = hhid, resp = y2bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = hhid, resp = y3bin)
	, prior(cauchy(0, 5), class = sd, group = years, resp = y1bin)
	, prior(cauchy(0, 5), class = sd, group = years, resp = y2bin)
	, prior(cauchy(0, 5), class = sd, group = years, resp = y3bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = years, resp = y1bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = years, resp = y2bin)
	, prior(cauchy(0, 5), class = sd, coef = Intercept, group = years, resp = y3bin)
	, set_prior("lkj(1)", class = "cor")
#	, prior(normal(0, 1), class = ar, resp = y1bin)
#	, prior(normal(0, 1), class = ar, resp = y2bin)
#	, prior(normal(0, 1), class = ar, resp = y3bin)
#	, prior(cauchy(0, 5), class = sderr, resp = y1bin)
#	, prior(cauchy(0, 5), class = sderr, resp = y2bin)
#	, prior(cauchy(0, 5), class = sderr, resp = y3bin)
)

for (s in 1:nsims){
   df <- (sim_dflist[[s]]
      %>% data.frame()
		%>% mutate_at(c("hhid", "years"), as.factor)
   )
	model <- brm(
		mvbind(y1, y2, y3) ~ 0 + intercept + wealthindex + (1|g|years) + (1|q|hhid)
			, autocor = list(cor_ar(form = ~years|hhid, p = 1, cov = FALSE)
				, cor_ar(form = ~years|hhid, p = 1, cov = FALSE)
				, cor_ar(form = ~years|hhid, p = 1, cov = FALSE)
			)
			, data = df
#			, family = list(bernoulli(link = "logit")
#				, bernoulli(link = "logit")
#				, bernoulli(link = "logit")
#			) 
			, warmup = 1e3
			, iter = 1e4
			, chains = 4
			, cores = parallel::detectCores()
			, control = list(adapt_delta = 0.95)
			, seed = 7777
#			, prior = priors
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
            , ifelse(grepl("_sd", coef), paste0("sd_hhid__service", n, "_Intercept")
            , paste0("cor_hhid__service", substr(n,1,1), "_Intercept__service", substr(n,2,2), "_Intercept"))
         )
      )
   )
)

save(file = "brmsModelbinAR1.rda"
	, brmsmodel_list
	, brmscoef_df
	, betas_df
	, covmat_df
	, betas
	, covMat
	, corMat
	, ar1_objs
	, ar1Errors_dflist
)

