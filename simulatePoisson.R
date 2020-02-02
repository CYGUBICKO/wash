#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7902)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

nsims <- 50 # Number of simulations to run
sample_prop <- 0.08 # Prop of sample per hh
year <- 2013

# Predictor variable to simulate
predictors <- "wealthindex"

# Beta values
service1_int <- 0.4
service1_wealth <- 0.4 
service2_int <- 0.5
service2_wealth <- 0.6
service3_int <- 0.2
service3_wealth <- 0.7

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
print(sim_df)

summary(sim_df)

people <- nrow(sim_df)
sim_dflist <- list()

for (i in 1:nsims){
	dat <- (sim_df
		%>% mutate(
			service1 = rpois(people, exp(pred1))
			, service2 = rpois(people, exp(pred2))
			, service3 = rpois(people, exp(pred3))
		)
	)
	sim_dflist[[i]] <- dat
}

print(sim_dflist)


# sim_df: simulated predicted values
# sim_dflist: simulated predicted response variables per sim

betas <- sapply(grep("service[1-9]", ls(), value = TRUE), get)

# Extract beta values assigned in the simulation
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% mutate(n = extract_numeric(coef)
		, coef = ifelse(grepl("_int$", coef)
			, paste0("serviceservice", n)
			, paste0("wealthindex:serviceservice", n)
		)	
	)
)
print(betas_df)

save(file = "simulatePoisson.rda"
	, sim_df
	, sim_dflist
	, betas_df
	, predictors
	, betas
)

