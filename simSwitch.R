#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Simulate S1 and S2 based on AR1 covariates and switch prop. ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Nov 09 (Sat) ----

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

# Simulation parameters
nsims <- 1		# Number of simulations to run
nHH <- 3			# Number of HH (primary units) per year

nyrs <- 100		# Number of years to simulate
yrs <- 1:nyrs 	# Years to simulate
N <- nyrs * nHH

# AR1 process simulation
phi <- 0.8
sdeps <- 1

ar1Fun <- function(phi, sdeps, nyrs, nHH){
	
	# Simulate for every HH for all the years
	df_list <- list()
	for (hh in 1:nHH){
		sdAsymp <- sdeps/sqrt(1 - phi^2)
		x0 <- rnorm(1, 0, sdAsymp)
		eps <- rnorm(nyrs, 0, sdeps)
		x <- numeric(nyrs)
		years <- numeric(nyrs)
		for (yr in 1:nyrs){
			years[[yr]] <- yr
			xprev <- ifelse(yr==1, x0, x[[yr-1]])
			x[[yr]] <- phi*xprev + eps[[yr]]
		}
		dat <- data.frame(x = x, years = years, hhid = hh)
		df_list[[hh]] <- dat
	}
	## Rewrite this with bind_rows; you will be happy
	df <- do.call(rbind, df_list) # Merge all the dataset for all the HH
	return(df)
}

## Try the ar1Fun function
x1_df <- ar1Fun(phi = 0.8, sdeps = 1, nyrs = 100, nHH = 3)

print(x1_df)

print(ggplot(x1_df, aes(x = years, y = x, colour = as.factor(hhid), group = as.factor(hhid)))
	+ geom_line()
#	+ scale_colour_viridis_d(name = "HHID")
)

## Simulate Unmeasured (xu) and Measured (xm) covariates
xu <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)
xm <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)

## Create dataframe of the two covariates
temp_df <- (xu
	%>% setnames("x", "xu")
	%>% right_join(xm)
	%>% setnames("x", "xm")
)


# True parameter values

## Intercept (change to gain and lose?)
s1_B0 <- 0.2
s2_B0 <- 0.3

## Measured
s1_M <- 0.4
s2_M <- 0.5

## Unmeasured
s1_U <- 0.6
s2_U <- 0.7

## Covariance
s1_sd <- 0.5
s2_sd <- 0.3
cor_s1s2 <- 0.20

# Construct covariance matrix
## Correlation matrix
corMat <- matrix(
	c(1, cor_s1s2
		, cor_s1s2, 1
	), 2, 2
)

sdVec <- c(s1_sd, s2_sd)
varMat <- sdVec %*% t(sdVec)
varMat
corMat
# matrix
covMat <- varMat * corMat
covMat

# Simulate B0 for each year and then merge to temp_df data. Different HHs have same B0 for same year
betas0 <- (MASS::mvrnorm(nyrs
		, mu = c(s1_B0, s2_B0)
		, Sigma = covMat
		, empirical = TRUE
	)
	%>% data.frame()
	%>% mutate(years = 1:nyrs)
	%>% right_join(temp_df)
	%>% select(c("X1", "X2"))
	%>% setnames(names(.), c("s1_B0", "s2_B0"))
)
print(betas0)

# Question: Should we also simulate HH randef?

## Simulate HH-level random effects (residual error)
hhRE <- MASS::mvrnorm(nHH
	, mu = c(0, 0)
	, Sigma = covMat
	, empirical = TRUE
)
hhRE <- hhRE[temp_df$hhid, ]

dat <- (temp_df
	%>% mutate(s1 = betas0[,1] + s1_M*xm + s1_U*xu + hhRE[,1]
		, s2 = betas0[,2] + s2_M*xm + s2_U*xu + hhRE[,2]  
		, s1bin = rbinom(N, 1, plogis(s1))
		, s2bin = rbinom(N, 1, plogis(s2))
	)
)

print(data.frame(dat))

## Plot simulated services (continuous)

print(ggplot(dat, aes(x = years, y = s1, colour = as.factor(hhid), group = as.factor(hhid)))
	+ geom_line()
#	+ scale_colour_viridis_d(name = "HHID")
)


quit()


save(file = "simSwitch.rda"
	, corMat
	, covMat
)

