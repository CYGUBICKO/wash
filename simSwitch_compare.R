#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Understanding Mechanistic and Statistic SimSwitch ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 28 (Tue) ----



switchCompare <- function(p_gain, p_lose, phi, sdeps, nyrs, nHH, s1_M, s1_U){
	
	prob2logis <- function(p){
		return(log(p/(1-p)))
	}

	b_gain <- prob2logis(p_gain)
	b_lose <- prob2logis(p_lose)
	b_add <- -(b_gain + b_lose)
	
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
		df <- do.call(rbind, df_list) # Merge all the dataset for all the HH
		return(df)
	}

	xu <- ar1Fun(phi = phi*phimult, sdeps = sdeps, nyrs = nyrs, nHH = nHH)
	xm <- ar1Fun(phi = phi, sdeps = sdeps, nyrs = nyrs, nHH = nHH)

	## Create dataframe of the two covariates
	temp_df <- (xu
		%>% setnames("x", "xu")
		%>% right_join(xm)
		%>% setnames("x", "xm")
	)

	dat <- (temp_df
		%>% mutate(lp1 = s1_M*xm + s1_U*xu
			, y1 = rbinom(N, 1, prob=1/2)
			, osmech = NA
			, osstat = NA
		) %>% select(-xu)
	)

	dat[1, "osmech"] = dat[1, "lp1"]
	dat[1, "osstat"] = dat[1, "lp1"]

	for (r in 2:nrow(dat)){
		 dat[r, "osmech"] <- dat[r, "lp1"] + ifelse(
			dat[r-1, "y1"] == 0
			, b_gain
			, -b_lose
		)
		dat[r, "osstat"] <- dat[r, "lp1"] + b_gain + b_add*dat[r-1, "y1"]
	}

	dat <- (dat
		%>% mutate(osmech_prob = plogis(osmech)
			, osstat_prob = plogis(osstat)
			, add_prob = plogis(b_add)
		)
	)
	return(dat)
}



save.image(file = "simSwitch_compare.rda")

