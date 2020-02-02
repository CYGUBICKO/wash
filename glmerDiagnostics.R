#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Complex Diagnostics ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 06 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lattice)
library(dotwhisker)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("complexGlmer.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

lme4_model <- complexglmer_list[[1]]
long_df <- complexdf_list[[1]]

# Parameter estimates
dwplot(lme4_model, effects = "fixed", by_2sd = FALSE)

# Residual plots
plot(lme4_model, service ~ resid(.))
plot(lme4_model)

# Compute profile confidence intervals for comparison
#lme4_CI_prof <- confint(lme4_model)

save(file = "glmerDiagnostics.rda"
	, lme4_model
#	, lme4_CI_prof
)

