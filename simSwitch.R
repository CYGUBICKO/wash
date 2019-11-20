#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Simulate S1 and S2 based on AR1 covariates and switch prop. ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Nov 09 (Sat) ----

library(data.table)
library(dplyr); options(dplyr.width = Inf)
library(tidyr)
library(tibble)

set.seed(7775)

# Simulation parameters
nsims <- 1		# Number of simulations to run
nHH <- 1			# Number of HH (primary units) per year

nyrs <- 1000		# Number of years to simulate
yrs <- 1:nyrs 	# Years to simulate
N <- nyrs * nHH

lines <- 200

# AR1 process simulation
phi <- 0.0
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
		tempdat <- data.frame(x = x, years = years, hhid = hh)
		df_list[[hh]] <- tempdat
	}
	## Rewrite this with bind_rows; you will be happy
	df <- do.call(rbind, df_list) # Merge all the dataset for all the HH
	return(df)
}

## Simulate Unmeasured (xu) and Measured (xm) covariates
xu <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)
xm <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)

## Create dataframe of the two covariates
tempdat <- (xu
	%>% setnames("x", "xu")
	%>% right_join(xm)
	%>% setnames("x", "xm")
)

# True parameter values

## Measured
s1_M <- 0.3
s2_M <- 0.6

## Unmeasured
s1_U <- 0.4
s2_U <- 0.8

## Switch probabilities
b_gain1 = -0.5
b_lose1 = -0.4
b_add1 = -(b_gain1 + b_lose1)

b_gain2 = -0.5
b_lose2 = -0.4
b_add2 = -(b_gain2 + b_lose2)

## b_add1 = b_add2 = 0

# Question: Should we also simulate HH randef?
## Maybe: it would have to be AR1 across years
## Don't do it to correlate services (not mechanistic)

# Question: Should we have year effects?
## Probably

tempdat <- (tempdat
	%>% mutate(lp1 = s1_M*xm + s1_U*xu
		, lp2 = s2_M*xm + s2_U*xu
		, y1 = NA
		, y2 = NA
	) %>% select(-xu)
)

## Simulate the first entry using the random draw but not worrying too much
## We can improve this later, using an average of something

## We should still add a beta here
## Need to be tidy!!

## Steve trying to tidy but not today
#dat <- (tempdat
#	%>% mutate(r = 1:n()
#		, y1 = ifelse(r==1, rbinom(1, 1, plogis(b_gain1 + b_add1/2))
#			, rbinom(1, 1, plogis(lp1 + b_gain1 + b_add1 * lag(y1, default = first(y1))))
#		)
#	)
#)

## Why do we divide by 2??
## Could this also be tidy now?

for (r in 1:nrow(tempdat)){
	if (r == 1){
		os1 <- b_gain1 + b_add1/2
		os2 <- b_gain2 + b_add2/2
		tempdat[r, "y1"] <- rbinom(r, 1, plogis(os1))
		tempdat[r, "y2"] <- rbinom(r, 1, plogis(os2))
	} else{
		os1 <- tempdat[r, "lp1"] + b_gain1 + b_add1*tempdat[r-1, "y1"]
		os2 <- tempdat[r, "lp2"] + b_gain2 + b_add2*tempdat[r-1, "y2"]

		tempdat[r, "y1"] <- rbinom(1, 1, plogis(os1))
		tempdat[r, "y2"] <- rbinom(1, 1, plogis(os2))	
	}
}

print(tempdat, N=lines)

dat <- (tempdat
	%>% select(-c("lp1", "lp2"))
)

with(dat, print(table(y1, y2)))

## Analysis: Stepback trick??
## Remove the last year and first year in the data and merge the two

prevdat <- (dat
	%>% transmute(hhid=hhid
		, years = years+1
		, y1p = y1
		, y2p = y2
	)
)

dat <- (dat
	%>% left_join(prevdat)
)

print(dat)

print(b_add1)
print(b_gain1)
summary(glm(y1 ~ xm+y1p, family=binomial, data = dat))
summary(glm(y2 ~ xm+y2p, family=binomial, data = dat))

