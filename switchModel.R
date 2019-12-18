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

set.seed(7775)

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
y1coef_list <- list()
y1model_list <- list()

y2coef_list <- list()
y2model_list <- list()

glmercoef_list <- list()
glmermodel_list <- list()

for (s in 1:nsims){
	df <- (sim_dflist[[s]]
		%>% filter(years >= 20) #Throw away first 10 years
	)
	long_df1 <- (df
		%>% gather(services, status, c("y1", "y2"))
	)
	long_df2 <- (df
		%>% select(-xm)
		%>% gather(serviceP, statusP, c("y1p", "y2p"))
		%>% mutate_at("serviceP", function(x)gsub("p", "", x))
	)
	long_df <- (long_df1
		%>% full_join(long_df2, by = c("hhid", "years", c(services="serviceP")))
	)
	
	tryCatch({
		# y1 model
   	y1_model <- glmer(y1 ~ y1p + xm + (1|hhid)
      	, data = df
      	, family = binomial
   	)
   	y1coef_list[[s]] <- fixef(y1_model)
		y1model_list[[s]] <- y1_model
		
		# y2 model
   	y2_model <- glmer(y2 ~ y2p + xm + (1|hhid)
      	, data = df
      	, family = binomial
   	)
   	y2coef_list[[s]] <- fixef(y2_model)
		y2model_list[[s]] <- y2_model
   	
		# Joint model
		glmer_model <- glmer(status ~ -1 + (services + statusP + xm):services + (0 + services|hhid)
      	, data = long_df
      	, family = binomial(link = "logit")
			, control=glmerControl(optimizer="bobyqa")
   	)
   	glmercoef_list[[s]] <- fixef(glmer_model)
		glmermodel_list[[s]] <- glmer_model
	}
	, error = function(e){print(paste("ERROR caught:", e))}
	)
}

#### ---- Tidy coefficients ----

y1coef_df <- (Reduce(rbind, y1coef_list)
	%>% as_tibble()
	%>% gather(coefs, values)
)
print(data.frame(y1coef_df))

y2coef_df <- (Reduce(rbind, y2coef_list)
	%>% as_tibble()
	%>% gather(coefs, values)
)
print(data.frame(y2coef_df))

glmercoef_df <- (Reduce(rbind, glmercoef_list)
	%>% as_tibble()
	%>% gather(coefs, values)
	%>% mutate(variables = paste0("y", extract_numeric(coefs))
		, coefs = gsub(".*y[1-2]", "(Intercept)", gsub(".*statusP", "yp", gsub(".*\\:", "", coefs)))
	)
)
print(data.frame(glmercoef_df))


save(file = "switchModel.rda"
	, glmercoef_df
	, glmermodel_list
	, y1coef_df
	, y1model_list
	, y2coef_df
	, y2model_list
	, betas_df
	, s1_M
	, s2_M
	, b_gain1
	, b_add1
	, b_gain2
	, b_add2
)

