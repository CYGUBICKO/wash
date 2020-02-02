#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLMER to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Oct 14 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(lme4)

options(dplyr.width = Inf)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("simulateHierarchicalmvn.rda")
set.seed(7777)

# Objects in
# * sim_dflist
# * betas_df
# * betas
# * predictors

services <- c("y1bin", "y2bin", "y3bin")
nsims <- length(sim_dflist)
model_form <- as.formula(status ~ 0 + wealthindex:service + service + (0 + service|hhid) + (0 + service|years))

report <- 1 # Index within nsims to save for summary
glmercoef_list <- list()
glmermodel_list <- list()
glmerdf_list <- list()

for (s in 1:nsims){
   long_df <- (sim_dflist[[s]]
      %>% select(c("hhid", "years", "wealthindex", services))
      %>% gather(service, status, services)
   )
	glmerdf_list[[s]] <- long_df
	tryCatch({
   	glmer_model <- glmer(model_form
      	, data = long_df
      	, family = binomial(link = "logit")
			, control=glmerControl(optimizer="bobyqa")
   	)
   	glmercoef_list[[s]] <- fixef(glmer_model)
		if (s <= report){
			glmermodel_list[[s]] <- glmer_model
		}
	}
	, error = function(e){print(e)}
	)
}

glmercoef_df <- Reduce(rbind, glmercoef_list) %>% as_tibble()
print(glmercoef_df)

save(file = "glmerModelbin.rda"
	, glmermodel_list
   , glmercoef_df
	, glmerdf_list
	, betas_df
	, betas
	, covmat_df
	, covMat
	, corMat
)

