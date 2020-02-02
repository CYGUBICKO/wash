#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Sub-task: Generate 'fake' response ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 05 (Tue) ----

library(arm)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("analysisdata.rda")

set.seed(7902)

# Aim is to simulate the outcome variable so as to understand the underlying distribution.

nsims <- 1000
df_prop <- 0.05 # Prop of data to use

predictors <- c("intvwyear"
	, "slumarea"
	, "ageyears"
	, "gender"
	, "ethnicity"
	, "numpeople_total"
	, "isbelowpovertyline"
  , "wealthindex"
  , "expend_total_USD_per_centered"
)

#### ---- Water source ----

model_form <- as.formula(paste0("cat_hhwatersource", "~ "
		, paste(predictors, collapse = "+")
	)
)
print(model_form)

# Proportion of data
subset_df <- working_df %>% balPartition("cat_hhwatersource", prop = df_prop)
simulation_df <- subset_df[["train_df"]]

# Model matrix
X <- model.matrix(model_form, data = simulation_df)

betas <- c(b0 = 0
	, intvwyear = 0.2
	, slumareaViwandani = -0.5
	, ageyears = 0.5
	, genderMale = 0
  	, ethnicityKamba = -1
  	, ethnicityKikuyu = 1
  	, ethnicityKisii = 0.5
  	, ethnicityLuhya = -0.9
  	, ethnicityLuo = 0.8
  	, numpeople_total = 0.5
  	, isbelowpovertylineYes = -0.5
	, wealthindex = 1
  	, expend_total_USD_per_centered = 1.2
)
## betas <- unlist(betas)
print(betas)

n <- nrow(X)

ysims <- array(NA, c(nsims, n))
for (s in 1:nsims){
	xb <- X %*% betas
	p <- 1/(1 + exp(-xb))
	ysims[s, ] <- rbinom(n, 1, p)
}

# Props of 1s per simulation
sim_prop <- apply(ysims, 1, function(x){
		p <- sum(x == 1)/length(x)
		return(p)
	}
)
ysims_df <- data.frame(y_prop = sim_prop)
print(ysims_df)

# Prop of 1s observed
yobs_prop <- (simulation_df
	%>% na.omit()
	%>% summarize_at("cat_hhwatersource", mean)
	%>% pull()
)
print(yobs_prop)

sim_plot <- (ggplot(ysims_df, aes(x = y_prop))
	+ geom_density(alpha = 0.3, fill = "lightgreen")
	+ geom_vline(aes(xintercept = yobs_prop, color = "green4")
		, linetype="dashed"
	)
	+ labs(x = "Prop. of fake 1s generated", y = "Desnsity")
	+ ggtitle("Compare proportion of fake 1s generated vs Observed")
	+ guides(colour = FALSE)
	+ theme(plot.title = element_text(hjust = 0.5))
)
print(sim_plot)

