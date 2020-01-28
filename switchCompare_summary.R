#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Understanding Mechanistic and Statistic SimSwitch ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 28 (Tue) ----

library(mvtnorm)
library(data.table)
library(dplyr)
options(dplyr.width = Inf)

library(tidyr)
library(tibble)
library(nlme)

library(ggplot2)
theme_set(theme_bw() +
   theme(panel.spacing=grid::unit(0,"lines")))

set.seed(7777)
load("simSwitch_compare.rda")

# Bootstrap function

bootFun <- function(df, stat_vars = "osmech_prob", reps = 100){
	temp_df <- (df
		%>% select(stat_vars)
	)
	df_samples <- lapply(1:reps, function(i) temp_df[sample(nrow(temp_df), replace = TRUE), ])
	est <- sapply(df_samples, mean)
	quants <- as.vector(quantile(est, c(0.025, 0.5, 0.975), na.rm = TRUE))
	return(quants)
}

# Simulation parameters
nHH <- 1			# Number of HH (primary units) per year

nyrs <- 100		# Number of years to simulate
yrs <- 1:nyrs 	# Years to simulate
N <- nyrs * nHH

# AR1 process simulation
phi <- 0.8
sdeps <- 1
phimult <- 0 ## control unmeasured AR for debugging

# True parameter values

## Measured
s1_M <- 0

## Unmeasured
s1_U <- 0

## Switch probabilities
p_gain = 0.2
p_lose = 0.3

sim_df <- switchCompare(p_gain = p_gain
	, p_lose = p_lose
	, phi = phi
	, sdeps = sdeps
	, nyrs = nyrs
	, nHH = nHH
	, s1_M = s1_M
	, s1_U = s1_U
)
print(head(sim_df, 50), width = Inf)

## Simulate for different p_gain and p_lose

