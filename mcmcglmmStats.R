#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCMglmr summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 03 (Wed) ----

library(MCMCglmm)

#load("complexGlmer.rda")
load("multiMcmcglmm.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

nsims <- length(multimcmcglmm_list)

for (s in 1:nsims){
	mcmc_mgl <- multimcmcglmm_list[[s]]
	c2 <- ((16 * sqrt(3))/(15 * pi))^2
	mcmc_names <- names(data.frame(mcmc_mgl[["VCV"]]))
	mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)] <- 
	mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)]/(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)])
	mcmc_mgl[["Sol"]][, 4:6] <- mcmc_mgl[["Sol"]][, 4:6]/sqrt(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)[c(1, 5, 9)]])
	mcmc_mgl[["VCV"]][, c(1, 5, 9)] <- sqrt(mcmc_mgl[["VCV"]][, c(1, 5, 9)]) 
	print(summary(mcmc_mgl))
}

