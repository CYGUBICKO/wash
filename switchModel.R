#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit switch data ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 02 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2); theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))
library(lme4)

load("simSwitch.rda")

# Objects in
# * sim_dflist
# * s1_M
# * s2_M
# * b_gain1
# * b_add1
# * b_gain2
# * b_add2

set.seed(7748)

# True parameters
betas_df <- (data.frame(y1_M = s1_M
		, y2_M = s2_M
		, y1_bgain = b_gain1
		, y2_bgain = b_gain2
		, y1_add = b_add1
		, y2_add = b_add2
	)
	%>% t()
	%>% data.frame()
	%>% rename("betas"=".")
	%>% rownames_to_column("coefs")
	%>% mutate(coefs2 = coefs
		, coefs = ifelse(grepl("gain", coefs), "(Intercept)"
			, ifelse(grepl("_M", coefs), "xm"
				, ifelse(grepl("_add", coefs), paste0("y", extract_numeric(coefs), "p"), coefs)
			)
		)
	)
)

print(betas_df)

nsims <- length(sim_dflist)
y1_coef_list <- list()
y1_model_list <- list()

y2_coef_list <- list()
y2_model_list <- list()

for (s in 1:nsims){
	df <- (sim_dflist[[s]]
		%>% filter(years >= 10) #Throw away first 10 years
	)
	tryCatch({
		# y1 model
   	y1_model <- glmer(y1 ~ y1p + xm + (1|hhid)
      	, data = df
      	, family = binomial
			, control=glmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore")
   	)
   	y1_coef_list[[s]] <- fixef(y1_model)
		y1_model_list[[s]] <- y1_model
		
		# y2 model
   	y2_model <- glmer(y2 ~ y2p + xm + (1|hhid)
      	, data = df
      	, family = binomial
			, control = glmerControl(check.nobs.vs.nlev="ignore" ,check.nobs.vs.nRE="ignore")
   	)
   	y2_coef_list[[s]] <- fixef(y2_model)
		y2_model_list[[s]] <- y2_model
	}
	, error = function(e){print(paste("ERROR caught:", e))}
	)
}

y1_coef_df <- (Reduce(rbind, y1_coef_list)
	%>% as_tibble()
	%>% gather(coefs, values)
)
print(y1_coef_df)

y2_coef_df <- (Reduce(rbind, y2_coef_list)
	%>% as_tibble()
	%>% gather(coefs, values)
)
print(y2_coef_df)


## Histograms

### y1
print(ggplot(y1_coef_df, aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y1", coefs2))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

### y2
print(ggplot(y2_coef_df, aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y2", coefs2))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

warnings( )

## Joint model: Working but slow for now
#f1 <- bf(y1 ~ 0 + intercept + y1p + xm + (1|g|years) + (1|q|hhid))
#f2 <- bf(y2 ~ 0 + intercept + y2p + xm + (1|g|years) + (1|q|hhid))
#
#model3 <- brm(
#	mvbf(f1, f2)
#		, data = dat
#		, family = list(bernoulli(link = "logit")
#			, bernoulli(link = "logit")
#		) 
#		, warmup = 1e3
#		, iter = 2000
#		, chains = 2
#		, cores = parallel::detectCores()
#		, control = list(adapt_delta = 0.95)
#		, seed = 7777
##		, prior = priors
#)


#save(file = "glmerModelbin.rda"
#	, glmermodel_list
#   , glmercoef_df
#	, glmerdf_list
#	, betas_df
#	, betas
#	, covmat_df
#	, covMat
#	, corMat
#)

