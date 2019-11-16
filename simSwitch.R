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

print(names(temp_df))
print(summary(temp_df))

# Question: Should we also simulate HH randef?
## Maybe: it would have to be AR1 across years
## Don't do it to correlate services (not mechanistic)

# Question: Should we have year effects?
## Probably

dat <- (temp_df
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
#df2 <- (dat
#	%>% mutate(r = 1:n()
#		, y1 = ifelse(r==1, rbinom(1, 1, plogis(b_gain1 + b_add1/2))
#			, rbinom(1, 1, plogis(lp1 + b_gain1 + b_add1 * lag(y1, default = first(y1))))
#		)
#	)
#)

for (r in 1:nrow(dat)){
	if (r == 1){
		os1 <- b_gain1 + b_add1/2
		os2 <- b_gain2 + b_add2/2
		dat[r, "y1"] <- rbinom(r, 1, plogis(os1))
		dat[r, "y2"] <- rbinom(r, 1, plogis(os2))
	} else{
		os1 <- dat[r, "lp1"] + b_gain1 + b_add1*dat[r-1, "y1"]
		os2 <- dat[r, "lp2"] + b_gain2 + b_add2*dat[r-1, "y2"]

		dat[r, "y1"] <- rbinom(1, 1, plogis(os1))
		dat[r, "y2"] <- rbinom(1, 1, plogis(os2))	
	}
}

print(dat, N=Inf)

with(dat, print(table(y1, y2)))
