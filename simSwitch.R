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
nsims <- 2			# Number of simulations to run
nHH <- 10000		# Number of HH (primary units) per year

nyrs <- 50		# Number of years to simulate
yrs <- 1:nyrs 	# Years to simulate
N <- nyrs * nHH

printLines <- 200

# True parameter values

## Measured
s1_M <- 0.3
s2_M <- 0.5

## Unmeasured
s1_U <- 0.2
s2_U <- 0.3

## Switch log odds
b_gain1 = 0.8		#~=0.7
b_lose1 = -1.4		#~=0.2
b_add1 = -(b_gain1 + b_lose1)

b_gain2 = -0.8		#~0.3
b_lose2 = 0.4		#~0.6
b_add2 = -(b_gain2 + b_lose2)


# AR1 process simulation
phi <- 0.8
phimult <- 0 ## control unmeasured AR for debugging
sdeps <- 1


## Simulate nsims times

sim_dflist <- list()

for (s in 1:nsims){
	
	# AR1 simulation
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
		df <- bind_rows(df_list)
		return(df)
	}

	## Simulate Unmeasured (xu) and Measured (xm) covariates
	xu <- ar1Fun(phi = phi*phimult, sdeps = sdeps, nyrs = nyrs, nHH = nHH)
	xm <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)

	## Create dataframe of the two covariates
	tempdat <- (xu
		%>% setnames("x", "xu")
		%>% right_join(xm, by = c("hhid", "years"))
		%>% setnames("x", "xm")
	)

	# HH random effects
	y1_hhef <- rnorm(nHH, mean = 0, sd = 1)
	y1_hhef <- y1_hhef[tempdat$hhid]
	y2_hhef <- rnorm(nHH, mean = 0, sd = 1)
	y2_hhef <- y2_hhef[tempdat$hhid]

	## b_add1 = b_add2 = 0

	# Question: Should we also simulate HH randef?
	## Maybe: it would have to be AR1 across years
	## Don't do it to correlate services (not mechanistic)

	# Question: Should we have year effects?
	## Probably

	tempdat <- (tempdat
		%>% mutate(lp1 = s1_M*xm + s1_U*xu + y1_hhef
			, lp2 = s2_M*xm + s2_U*xu + y2_hhef
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
	### Just random starting point

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

	dat <- (tempdat
		%>% select(-c("lp1", "lp2"))
	)

	prevdat <- (dat
		%>% transmute(hhid=hhid
			, years = years+1
			, y1p = y1
			, y2p = y2
		)
	)

	dat <- (dat
		%>% left_join(prevdat, by = c("hhid", "years"))
	)
	sim_dflist[[s]] <- dat
}

save(file = "simSwitch.rda"
	, sim_dflist
	, s1_M
	, s2_M
	, b_gain1
	, b_add1
	, b_gain2
	, b_add2
	, b_lose1
	, b_lose2
)
