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
library(ggplot2)


set.seed(7777)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

nsims <- 1
people <- 500 # Number of simulations to run

# Predictor
x <- rnorm(people)

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

# Sumulate Betas from mvnorm
sim_dflist <- list()
for (i in 1:nsims){
	XB <- (data.frame(x = x
			, pred1 = y1_beta0 + y1_beta1 * x
			, pred2 = y2_beta0 + y2_beta1 * x
			, pred3 = y3_beta0 + y3_beta1 * x
		)
		%>% select(-x)
		%>% as.matrix()
	)
	sim_df <- (t(apply(XB, 1, function(x){
         	rmvnorm(1, mean = x, sigma = covMat)
      	})
   	)
   	%>% data.frame()
   	%>% setnames(names(.), c("y1", "y2", "y3"))
   	%>% mutate(id = 1:people
      	, x = x
   	)
	)

	dat <- (sim_df
		%>% mutate(
			y1bin = rbinom(people, 1, plogis(y1))
			, y2bin = rbinom(people, 1, plogis(y2))
			, y3bin = rbinom(people, 1, plogis(y3))
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

