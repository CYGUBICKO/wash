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
people <- 150	# Number of cases (primary units) per HH
J <- 30			# Number of HH

# Generate dataset template
temp_df <- data.frame(id = sort(rep(c(1:people),J))
	, J = rep(c(1:J),people)
	, x = rnorm(n = J*people)
)
print(temp_df)

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
	betas <- (MASS::mvrnorm(people
			, mu = rep(c(y1_beta1, y2_beta1, y3_beta1), each = 1)
			, Sigma = covMat
			, empirical = TRUE
		)
   	%>% data.frame()
   	%>% setnames(names(.), c("b1", "b2", "b3"))
	)
	betas0 <- (MASS::mvrnorm(people
			, mu = rep(c(y1_beta0, y2_beta0, y3_beta0), each = 1)
			, Sigma = covMat
			, empirical = TRUE
		)
   	%>% data.frame()
	)
	betas0 <- betas0[temp_df$id, ]

	dat <- (betas[temp_df$id, ]
		%>% mutate(id = pull(temp_df, id)
			, x = pull(temp_df, x)
			, y1 = betas0[,1] + b1*x
			, y2 = betas0[,2] + b2*x
			, y3 = betas0[,3] + b3*x
			, y1bin = rbinom(people*J, 1, plogis(y1))
			, y2bin = rbinom(people*J, 1, plogis(y2))
			, y3bin = rbinom(people*J, 1, plogis(y3))
		)
	)
	sim_dflist[[i]] <- dat
}

print(head(sim_dflist[[1]]))

# Extract beta values assigned in the simulation
betas <- sapply(grep("y[1-9]|cor_y", ls(), value = TRUE), get)
#betas <- betas[!names(betas) %in% grep("_sd", names(betas), value = TRUE)]
betas_df <- (data.frame(betas) 
	%>% rownames_to_column("coef")
	%>% mutate(n = extract_numeric(coef)
		, coef = ifelse(grepl("_0$", coef)
			, paste0("y", n)
				, ifelse(grepl("_beta", coef) 
					, paste0("x:y", n)
				, coef
			)
		)
	)
)

print(betas_df)

save(file = "simulateHierarchicalmvn.rda"
	, sim_dflist
	, betas_df
	, betas
)

