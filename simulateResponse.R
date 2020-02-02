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

set.seed(7777)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

nsims <- 1 # Number of simulations to run
sample_prop <- 0.05 # Prop of samples
year <- 2013

# Predictor variable to simulate
predictors <- "wealthindex"

# Beta values
service1_int <- 0.1
service1_wealth <- 0.4 
service2_int <- 0.2
service2_wealth <- 0.5
service3_int <- 0.3
service3_wealth <- 0.6

# Confounder service
serviceU_1 <- 0.6
serviceU_2 <- 0.2
serviceU_3 <- 0.6

sim_df <- (working_df
	%>% group_by(intvwyear, hhid_anon)
	%>% filter(intvwyear %in% year & runif(n())<=sample_prop & !is.nan(wealthindex))
	%>% select(hhid_anon, predictors)
	%>% ungroup()
	%>% mutate_at(predictors, scale)
	%>% group_by(hhid_anon)
	%>% mutate(U1 = rnorm(n=1)
		, U2 = rnorm(n=1)
		, U3 = rnorm(n=1)
		, pred1 = serviceU_1*U1 + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U2 + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U3 + service3_wealth*wealthindex + service3_int
	)
	%>% ungroup()
	%>% droplevels()
)
print(data.frame(select(sim_df, hhid_anon, wealthindex, U1, U2, U3)))

summary(sim_df)

# Proportion of 1s per simulation
service_prop <- tibble(sims = 1:nsims
	, service1 = numeric(nsims)
	, service2 = numeric(nsims)
	, service3 = numeric(nsims)
)

people <- nrow(sim_df)
sim_dflist <- list()

for (i in 1:nsims){
	dat <- (sim_df
		%>% mutate(
			service1 = rbinom(people, 1, plogis(pred1))
			, service2 = rbinom(people, 1, plogis(pred2))
			, service3 = rbinom(people, 1, plogis(pred3))
		)
	)
	service_prop[i,2] <- mean(dat[["service1"]])
	service_prop[i,3] <- mean(dat[["service2"]])
	service_prop[i,4] <- mean(dat[["service3"]])
	sim_dflist[[i]] <- dat
}

summary(service_prop)

print(sim_dflist)

prop_plot <- (service_prop
	%>% gather(var, prop, -sims)
	%>% ggplot(aes(x = prop))
		+ geom_histogram()
		+ facet_grid(~var, scales = "free")
		+ labs(x = "Proportion of HHs that has access to services (WASH)"
			, y = "Count"
		)
)

print(prop_plot)
#ggsave("git_push/prop_plot.pdf", prop_plot)

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

save(file = "simulateResponse.rda"
	, sim_df
	, sim_dflist
	, betas_df
	, predictors
	, betas
)

