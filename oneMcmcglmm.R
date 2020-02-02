#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit MCMCglmm to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 03 (Wed) ----

library(dplyr)
	options(dplyr.width = Inf)
library(tidyr)
library(tibble)

library(ggplot2)
	theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

library(MCMCglmm)

load("globalFunctions.rda")
## load("simulateResponse.rda")
load("analysisdata.rda")

set.seed(7902)

sample_prop <- 0.02 # Prop to sample
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

long_df <- (dat
	%>% select(c("hhid_anon", predictors, services))
	%>% gather(service, status, services)
	%>% data.frame()
)

# Priors
IJ <- (1/3) * (diag(3) + matrix(3000, 3, 3))
priors <- list(R = list(V = diag(1)*0.03, nu = 1, fix = 1)
	, G = list(G2 = list(V = diag(3), nu = 1
			, alpha.mu = rep(0, 3), alpha.V = IJ
		)
	)
)

mcmcglmmcoef_list <- list()
mcmcglmm_list <- list()

mcmcfit <- MCMCglmm(status ~ 0 + wealthindex:service + service
	, random = ~corg(service + 0):hhid_anon
	, family = "categorical"
	, data = long_df
	, prior = priors
	, verbose = FALSE
	, slice = TRUE
)

summary(mcmcfit)


