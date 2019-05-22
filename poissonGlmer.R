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
load("simulatePoisson.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("service1", "service2", "service3")
nsims <- length(sim_dflist)
model_form <- as.formula(status ~ 0 + wealthindex:service + service + (service + 0|hhid_anon))

poissoncoef_list <- list()
poissonglmer_list <- list()

for (s in 1:nsims){
   long_df <- (sim_dflist[[s]]
      %>% select(c("hhid_anon", predictors, services))
      %>% gather(service, status, services)
   )
   glmer_model <- glmer(model_form
      , data = long_df
      , family = poisson
   )
   poissoncoef_list[[s]] <- fixef(glmer_model)
   poissonglmer_list[[s]] <- glmer_model
}

poissoncoef_df <- Reduce(rbind, poissoncoef_list) %>% as_tibble()
summary(poissoncoef_df)
print(poissoncoef_df)

save(file = "poissonGlmer.rda"
	, poissonglmer_list
   , poissoncoef_df
	, predictors
	, betas_df
	, betas
)

