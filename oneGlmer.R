#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLMER to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 19 (Tue) ----

library(dplyr)
	options(dplyr.width = Inf)
library(tidyr)
library(tibble)

library(ggplot2)
	theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

library(lme4)
library(blme)

load("globalFunctions.rda")
## load("simulateResponse.rda")
load("analysisdata.rda")

set.seed(7902)

sample_prop <- 0.2 # Prop to sample
year <- 2013
predictors <- "wealthindex"

# Beta values
service1_int <- 0.4
service1_wealth <- 4 
service2_int <- 3
service2_wealth <- 2
service3_int <- 1
service3_wealth <- 3

# Confounder service
serviceU_1 <- 0.1
serviceU_2 <- 0.1
serviceU_3 <- 0.1

sim_df <- (working_df
	%>% filter(intvwyear==year & runif(n())<sample_prop)
	%>% select_("hhid_anon", predictors)
	%>% mutate(U = rnorm(n=n())
		, pred1 = serviceU_1*U + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U + service3_wealth*wealthindex + service3_int
	)
	%>% droplevels()
)

people <- nrow(sim_df)

dat <- (sim_df
	%>% mutate(
		service1 = rbinom(people, 1, plogis(pred1))
		, service2 = rbinom(people, 1, plogis(pred2))
		, service3 = rbinom(people, 1, plogis(pred3))
	)
)

## Analyze
services <- c("service1", "service2", "service3")
model_form <- as.formula(status ~ 0 + wealthindex:service + service + (1|hhid_anon))
sim <- 23

complexcoef_list <- list()
complexglmer_list <- list()

long_df <- (dat
%>% select(c("hhid_anon", predictors, services))
%>% gather(service, status, services)
)

trick_model <- glmer(model_form
, data = long_df
, family = binomial
)

summary(trick_model)

