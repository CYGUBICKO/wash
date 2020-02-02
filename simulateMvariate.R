#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(mvtnorm)
library(data.table)
library(dplyr)
options(dplyr.width = Inf)
library(purrr)
library(tidyr)
library(tibble)

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7777)

# Simulate multivariate response.

nsims <- 1	# Number of simulations to run

minHH <- 3	# Minimum number of interviews per HH before sampling. This is <=14
nHH <- 1500	# Number of HH (primary units) per year

nyears <- 14	# Number of years. This is <=14

# Predictor variable to simulate
predictors <- "wealthindex"

# Temporary data to sample from
temp_df <- (working_df
	%>% mutate_at(predictors, scale)
	%>% filter(!is.nan(wealthindex))
	%>% group_by(hhid_anon)
	%>% filter(n()>=minHH)
	%>% select(hhid_anon, intvwyear, predictors)
	%>% setnames(c("intvwyear", "hhid_anon"), c("years", "hhid"))
	%>% droplevels()
	%>% data.frame()
)

# Sample HHIDs from the data
hhids_sampled <- (temp_df
	%>% pull(hhid)
	%>% unique()
	%>% sample(nHH)
)

# Filter the sampled HHIDs from the temp dataset
temp_df <- (temp_df
	%>% filter(hhid %in% hhids_sampled)
	%>% group_by(hhid)
	%>% data.frame()
)

# Sample years from the data
years_sampled <- (temp_df
	%>% pull(years)
	%>% unique()
	%>% sample(nyears)
)
years_sampled
# Filter the sampled HHIDs from the temp dataset
temp_df <- (temp_df
	%>% filter(years %in% years_sampled)
	%>% group_by(hhid, years)
	%>% droplevels()
	%>% data.frame()
	%>% mutate(hhid = as.numeric(hhid))
)
temp_df

N <- nrow(temp_df)

# Beta values
y1_beta0 <- 0.3
y1_beta1 <- 0.4 
y2_beta0 <- 0.3
y2_beta1 <- 0.8
y3_beta0 <- 0.4
y3_beta1 <- 0.5

# Correlation matrix
cor_y1y2 <- 0.20
cor_y1y3 <- 0.30
cor_y2y3 <- 0.50
corMat <- matrix(
	c(1, cor_y1y2, cor_y1y3
		, cor_y1y2, 1, cor_y2y3
		, cor_y1y3, cor_y2y3, 1
	), 3, 3
)

# Sd
y1_sd <- 0.5
y2_sd <- 0.3
y3_sd <- 0.7
sdVec <- c(y1_sd, y2_sd, y3_sd)
varMat <- sdVec %*% t(sdVec)
varMat
corMat
# varcov matrix
covMat <- varMat * corMat
covMat

# Generate dataset
sim_dflist <- list()
for (i in 1:nsims){
	
	# Simulate B0 for each year and then merge to HH data. Different HH has same B0 for same year
	betas0 <- (MASS::mvrnorm(nyears
			, mu = c(y1_beta0, y2_beta0, y3_beta0)
			, Sigma = covMat
			, empirical = TRUE
		)
   	%>% data.frame()
		%>% mutate(years = years_sampled)
		%>% right_join(temp_df)
		%>% select(c("X1", "X2", "X3"))
	)
	
	# Simulate HH-level random effects (residual error)
	hhRE <- MASS::mvrnorm(nHH
		, mu = c(0, 0, 0)
		, Sigma = covMat
		, empirical = TRUE
	)
	hhRE <- hhRE[temp_df$hhid, ]
	
	dat <- (temp_df
		%>% mutate(y1 = betas0[,1] + y1_beta1*wealthindex + hhRE[,1]
			, y2 = betas0[,2] + y2_beta1*wealthindex + hhRE[,2]
			, y3 = betas0[,3] + y3_beta1*wealthindex + hhRE[,3]
			, y1bin = rbinom(N, 1, plogis(y1))
			, y2bin = rbinom(N, 1, plogis(y2))
			, y3bin = rbinom(N, 1, plogis(y3))
		)
	)
	sim_dflist[[i]] <- dat
}

print((sim_dflist[[1]]))

# Extract beta values assigned in the simulation
betas <- sapply(grep("y[1-9]|cor_y", ls(), value = TRUE), get)
#betas <- betas[!names(betas) %in% grep("_sd", names(betas), value = TRUE)]
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% mutate(n = extract_numeric(coef)
		, coef = ifelse(grepl("_0$", coef)
			, paste0("y", n)
				, ifelse(grepl("_beta", coef) 
					, paste0("wealthindex:y", n)
				, coef
			)
		)
	)
)

print(betas_df)

save(file = "simulateMvariate.rda"
	, sim_dflist
	, betas_df
	, betas
)
