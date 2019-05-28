#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(MASS)
library(data.table)
library(dplyr)
options(dplyr.width = Inf)

library(tidyr)
library(tibble)
library(ggplot2)


load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7777)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Simulate multivariate response.

nsims <- 1 # Number of simulations to run
sample_prop <- 0.08 # Prop of sample per hh
year <- 2013

# Predictor variable to simulate
predictors <- "wealthindex"

# Beta values
service1_int <- 0.2
service1_wealth <- 0.3 
service2_int <- 0.3
service2_wealth <- 0.8
service3_int <- 0.4
service3_wealth <- 0.5

# Correlation matrix
cor_s1s2 <- 0.20
cor_s1s3 <- 0.30
cor_s2s3 <- 0.50
corMat <- matrix(
	c(1, cor_s1s2, cor_s1s3
		, cor_s1s2, 1, cor_s2s3
		, cor_s1s3, cor_s2s3, 1
	), 3, 3
)

# Sd
service1_sd <- 0.6
service2_sd <- 0.5
service3_sd <- 0.7
sdVec <- c(service1_sd, service2_sd, service3_sd)
varMat <- sdVec %*% t(sdVec)
varMat
corMat
# varcov matrix
covMat <- varMat * corMat
covMat

# Confounder service
serviceU_1 <- 0
serviceU_2 <- 0
serviceU_3 <- 0

# Subset data to sumulate
temp_df <- (working_df
	%>% group_by(intvwyear, hhid_anon)
	%>% filter(intvwyear %in% year & runif(n())<=sample_prop & !is.nan(wealthindex))
	%>% ungroup()
	%>% select(hhid_anon, predictors)
	%>% mutate_at(predictors, scale)
	%>% droplevels()
)
people <- nrow(temp_df)

# Sumulate Betas from mvnorm
betas <- mvrnorm(n = people
	, mu = rep(c(service1_wealth, service2_wealth, service3_wealth), each = 1)
	, Sigma = covMat
	, empirical = TRUE
)
print(var(betas))
# Predictions (XB)
sim_df <- (temp_df
	%>% group_by(hhid_anon)
	%>% mutate(U1 = serviceU_1*rnorm(n = 1)
		, U2 = serviceU_2*rnorm(n = 1)
		, U3 = serviceU_3*rnorm(n = 1)
	)
	%>% ungroup()
	%>% mutate(pred1 = service1_int + betas[,1]*wealthindex + U1
		, pred2 = service2_int + betas[,2]*wealthindex + U2
		, pred3 = service3_int + betas[,3]*wealthindex + U3
	)
	%>% data.frame()
)

sim_dflist <- list()
for (i in 1:nsims){
	dat <- (sim_df
		%>% mutate(
			service1 = rbinom(people, 1, plogis(pred1))
			, service2 = rbinom(people, 1, plogis(pred2))
			, service3 = rbinom(people, 1, plogis(pred3))
		)
	)
	sim_dflist[[i]] <- dat
}

print(people)
print(head(sim_dflist[[1]]))

# Extract beta values assigned in the simulation
betas <- sapply(grep("service[1-9]|cor_s", ls(), value = TRUE), get)
#betas <- betas[!names(betas) %in% grep("_sd", names(betas), value = TRUE)]
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% mutate(n = extract_numeric(coef)
		, coef = ifelse(grepl("_int$", coef)
			, paste0("serviceservice", n)
				, ifelse(grepl("_wealth", coef) 
					, paste0("wealthindex:serviceservice", n)
				, coef
			)
		)
	)
)

print(betas_df)

save(file = "simulateMvariate.rda"
	, sim_df
	, sim_dflist
	, betas_df
	, predictors
	, betas
)

