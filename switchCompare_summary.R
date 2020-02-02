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

sim_df <- (sim_df
	%>% select(-xm)
)

print(head(sim_df, 50), width = Inf)

## Just in case I wanted to check for uncertainities
bootFun(sim_df, stat_vars = "add_prob")

## Simulate for different p_gain and p_lose

p_gains <- seq(0.1, 0.9, 0.2)
p_loses <- 1 - p_gains
p_adds <- matrix(0, nrow = length(p_gains), ncol = length(p_loses))

for (g in 1:length(p_gains)){
	for (l in 1:length(p_loses)){
		dd <- switchCompare(p_gain = p_gains[g]
			, p_lose = p_loses[l]
			, phi = phi
			, sdeps = sdeps
			, nyrs = nyrs
			, nHH = nHH
			, s1_M = s1_M
			, s1_U = s1_U
		)
		p_adds[g, l] <- mean(dd[["add_prob"]])
	}
}

p_df <- as.data.frame(p_adds)
colnames(p_df) <- paste0("g", p_gains)
rownames(p_df) <- paste0("l", p_loses)

p_df <- (p_df
	%>% rownames_to_column("p_lose")
	%>% gather(p_gain, p_add, -p_lose)
	%>% mutate_at(c("p_lose", "p_gain"), function(x){as.numeric(gsub("l|g", "", x))})
)

print(p_df)

print(ggplot(p_df, aes(x = p_gain, y = p_add, group = as.factor(p_lose), colour = as.factor(p_lose)))
	+ geom_line(aes(lty = as.factor(p_lose)))
	+ scale_colour_brewer(palette = "Dark2")
)
