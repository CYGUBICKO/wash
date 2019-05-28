#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Simulate multivariate gausian responses ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 28 (Tue) ----

library(MASS)
library(data.table)
library(dplyr)
options(dplyr.width = Inf)

library(tidyr)
library(tibble)
library(ggplot2)


set.seed(7777)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

nsims <- 1
people <- 1000 # Number of simulations to run

# Predictor
x <- rnorm(people)

# Beta values
y1_beta0 <- 0.2
y1_beta1 <- 0.3 
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
y1_sd <- 0.6
y2_sd <- 0.5
y3_sd <- 0.7
sdVec <- c(y1_sd, y2_sd, y3_sd)
varMat <- sdVec %*% t(sdVec)
varMat
corMat
# varcov matrix
covMat <- varMat * corMat
covMat

# Sumulate Betas from mvnorm
sim_dflist <- list()
for (i in 1:nsims){
	betas <- mvrnorm(n = people
		, mu = rep(c(y1_beta1, y2_beta1, y3_beta1), each = 1)
		, Sigma = covMat
		, empirical = TRUE
	)
	# Predictions (XB)
	sim_df <- (tibble(x = x
			, y1n = y1_beta0 + betas[,1]*x
			, y2n = y2_beta0 + betas[,2]*x
			, y3n = y3_beta0 + betas[,3]*x
		)
		%>% data.frame()
	)
	dat <- (sim_df
		%>% mutate(
			y1b = rbinom(people, 1, plogis(y1n))
			, y2b = rbinom(people, 1, plogis(y2n))
			, y3b = rbinom(people, 1, plogis(y3n))
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

save(file = "simulatemvn.rda"
	, sim_dflist
	, betas_df
	, betas
)

