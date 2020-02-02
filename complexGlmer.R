#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLMER to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 19 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(lme4)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
#load("simulateResponse.rda")
load("simulateMvariate.rda")

set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("service1", "service2", "service3")
nsims <- length(sim_dflist)
model_form <- as.formula(status ~ 0 + wealthindex:service + service + (0 + service|hhid_anon))

complexcoef_list <- list()
complexglmer_list <- list()
complexdf_list <- list()

for (s in 1:nsims){
   long_df <- (sim_dflist[[s]]
      %>% select(c("hhid_anon", predictors, services))
      %>% gather(service, status, services)
   )
	complexdf_list[[s]] <- long_df
	tryCatch({
   	glmer_model <- glmer(model_form
      	, data = long_df
      	, family = binomial(link = "logit")
			, control=glmerControl(optimizer="bobyqa")
   	)
   	complexcoef_list[[s]] <- fixef(glmer_model)
   	complexglmer_list[[s]] <- glmer_model
	}
	, error = function(e){print(e)}
	)
}

complexcoef_df <- Reduce(rbind, complexcoef_list) %>% as_tibble()
summary(complexcoef_df)
print(complexcoef_df)

save(file = "complexGlmer.rda"
	, complexglmer_list
   , complexcoef_df
	, complexdf_list
	, predictors
	, betas_df
	, betas
)

