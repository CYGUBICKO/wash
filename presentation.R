#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Some reports workshop ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 05 (Fri) ----

library(arm)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

load("descriptives.rda")
load("analysisdata.rda")

set.seed(7902)

theme_set(theme_bw() +
            theme(panel.spacing=grid::unit(0,"lines")))

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

sample_prop <- 0.3 # Prop of sample per hh

# Predictor variable to simulate
predictors <- "wealthindex"
years <- c(2003, 2013, 2015)
# Beta values
service1_int <- 0.4
service1_wealth <- 4 
service2_int <- 3
service2_wealth <- 2
service3_int <- 1
service3_wealth <- 3

# Confounder service
serviceU_1 <- 0.1
serviceU_2 <- 0.1
serviceU_3 <- 0.1

sim_df <- (working_df
	%>% filter(intvwyear %in% years & runif(n())<=sample_prop)
	%>% select_("hhid_anon", "intvwyear", "slumarea", predictors)
	%>% mutate(U = rnorm(n=n())
		, pred1 = serviceU_1*U + service1_wealth*wealthindex + service1_int
		, pred2 = serviceU_2*U + service2_wealth*wealthindex + service2_int
		, pred3 = serviceU_3*U + service3_wealth*wealthindex + service3_int
	)
	%>% droplevels()
)

people <- nrow(sim_df)

dat <- (sim_df
	%>% mutate(
		service1 = rbinom(people, 1, plogis(pred1))
		, service2 = rbinom(people, 1, plogis(pred2))
		, service3 = rbinom(people, 1, plogis(pred3))
	)
)

services <- c("service1", "service2", "service3")

long_df <- (dat
%>% select(c("hhid_anon", "intvwyear", "slumarea", predictors, services))
%>% gather(service, status, services)
)

#### ---- 6.9 Wealth index ----

wealthindex_plot <- (ggplot(long_df, aes(x = wealthindex, y = status, colour = slumarea))
	+ stat_sum(alpha = 0.1, na.rm = TRUE)
	+ facet_grid(intvwyear~service)
	+ geom_smooth(method = "gam"
		, method.args = list(family = "binomial")
		, formula = y~s(x, k = 20)
		, alpha = 0.2
		, na.rm = TRUE
	)
	+ labs(title = "WASH and HH Wealth index"
		, x = "Wealth Index"
		, y = "Access to WASH"
	)
#	+ facet_gid(~service, scale = "free")
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)

print(wealthindex_plot)

ggsave("git_push/wealthindex_plot.pdf", wealthindex_plot)

