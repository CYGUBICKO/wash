#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit switch data: One model for effect plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 13 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2); theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))
library(lme4)
library(splines)

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

df <- (sim_dflist[[1]]
	%>% group_by(hhid)
	%>% mutate(slumarea = sample(c("Korogocho", "Viwandani"), 1))
	%>% mutate_at(c("y1p", "y2p"), as.factor)
	%>% ungroup()
	%>% filter(years >= 20) #Throw away first 10 years
)


long_df1 <- (df
	%>% gather(services, status, c("y1", "y2"))
)
long_df2 <- (df
	%>% select(hhid, years, y1p, y2p)
	%>% gather(serviceP, statusP, c("y1p", "y2p"))
	%>% mutate_at("serviceP", function(x)gsub("p", "", x))
)
long_df <- (long_df1
	%>% full_join(long_df2, by = c("hhid", "years", c(services="serviceP")))
	%>% mutate(years = drop(scale(years)))
)

#print(long_df, n = 200, width = Inf)

# y1 model
#dd <- (df
#	%>% mutate(years = drop(scale(years)))
#	%>% data.frame()
#)
#singMod_df <- model.frame(
#	y1 ~ slumarea
#	+ y1p
#	+ xm
#	+ years
#	+ hhid
#	, data = dd
#	, na.action = na.exclude
#	, drop.unused.levels = TRUE
#)
#y1_model <- glmer(y1 ~ slumarea + years + y1p + ns(xm, 3) + (1|hhid)
#	, data = singMod_df
#	, family = binomial
#)

# Joint model
jointMod_df <- model.frame(
	status ~ services
	+ slumarea
	+ statusP
	+ xm
	+ years
	+ hhid
	, data = data.frame(long_df)
	, na.action = na.exclude
	, drop.unused.levels = TRUE
)
glmer_model <- glmer(status ~ -1 + (services + statusP + xm):services + (services-1|hhid)
	, data = jointMod_df
	, family = binomial(link = "logit")
	, control=glmerControl(optimizer="bobyqa")
)


save(file = "switchSingleModel.rda"
	, betas_df
#	, singMod_df
	, jointMod_df
#	, y1_model
	, glmer_model
)

