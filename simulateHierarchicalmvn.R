#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Simulate multivariate gausian responses ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 28 (Tue) ----

library(mvtnorm)
library(data.table)
library(dplyr)
options(dplyr.width = Inf)

library(tidyr)
library(tibble)

set.seed(7777)

# Simulation parameters
nsims <- 1		# Number of simulations to run
nHH <- 3000		# Number of HH (primary units) per year

nyrs <- 30	# Number of years to simulate
yrs <- 2000 + c(1:nyrs) # Years to simulate
N <- nyrs * nHH

# Generate dataset template
temp_df <- (data.frame(hhid = rep(c(1:nHH), each = nyrs)
		, years = rep(yrs, nHH)
		, wealthindex = rnorm(n = N)
	)
	%>% group_by(hhid)
#	%>% mutate(wealthindex = mean(wealthindex)) # Average hh wealth index
	%>% ungroup()
)
print(as.data.frame(temp_df))

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
sim_dflist <- list() # Simulated datasets
betas0_dflist <- list() # Simulated random effects estimates for Year 
hhRE_dflist <- list() # Simulated random effects datasets estimates for HH

for (i in 1:nsims){
	
	# Simulate B0 for each year and then merge to HH data. Different HH has same B0 for same year
	betas0 <- (MASS::mvrnorm(nyrs
			, mu = c(y1_beta0, y2_beta0, y3_beta0)
			, Sigma = covMat
			, empirical = TRUE
		)
   	%>% data.frame()
		%>% mutate(years = yrs)
		%>% right_join(temp_df)
		%>% select(c("years", "X1", "X2", "X3"))
	)

	# Save the B0 RE estimates
	betas0_dflist[[i]] <- (betas0
		%>% distinct()
		%>% setnames(c("X1", "X2", "X3"), c("y1", "y2", "y3"))
	)

	# Remove year from betas0
	betas0 <- (betas0
		%>% select(-years)
	)

	# Simulate HH-level random effects (residual error)
	hhRE <- MASS::mvrnorm(nHH
		, mu = c(0, 0, 0)
		, Sigma = covMat
		, empirical = TRUE
	)
	hhRE <- hhRE[temp_df$hhid, ]
	
	# Save HH RE simulation estimates
	hhRE_dflist[[i]] <- (hhRE
		%>% data.frame()
		%>% mutate(hhid = pull(temp_df, hhid))
		%>% distinct()
	)
	
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

# Create a dataframe of covariance matrix
covmat_df <- (
	data.frame(coef = c("y1y1", "y2y2", "y3y3", "y2y1", "y3y1", "y3y2")
		, values = c(diag(covMat), covMat[lower.tri(covMat)])
	)
	%>% mutate(n = extract_numeric(coef)
		, coef_clean = paste0("Sigma[years:y", substr(n, 1, 1), ",y", substr(n, 2, 2), "]")
	) 
)

save(file = "simulateHierarchicalmvn.rda"
	, sim_dflist
	, betas_df
	, covmat_df
	, betas0_dflist
	, hhRE_dflist
	, betas
	, corMat
	, covMat
)

