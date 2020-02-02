#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCglmm Diagnostics ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 08 (Wed) ----

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(MCMCglmm)
library(plotMCMC)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("multiMcmcglmm.rda")

# Incoming objects:
# * complexglmer_list - glmer fits per simulation
# * complexcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

### Marginalised loglic as per pg 47 of courseNotes mcmcglmm
mcmc_mgl <- multimcmcglmm_list[[1]]
c2 <- ((16 * sqrt(3))/(15 * pi))^2
mcmc_names <- names(data.frame(mcmc_mgl[["VCV"]]))
mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)] <- 
mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)]/(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)])
mcmc_mgl[["Sol"]][, 4:6] <- mcmc_mgl[["Sol"]][, 4:6]/sqrt(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)[c(1, 5, 9)]])
mcmc_mgl[["VCV"]][, c(1, 5, 9)] <- sqrt(mcmc_mgl[["VCV"]][, c(1, 5, 9)]) 
multimcmc_model <- mcmc_mgl
# Estimate CI

beta_estimates <- (summary(multimcmc_model)[["solutions"]]
	%>% data.frame()
	%>% rownames_to_column("effects")
	%>% setnames(names(.)[3:4], c("lowerCI", "upperCI"))
	%>% mutate(effects = gsub("trait", "", effects))
	%>% mutate_at("effects", function(x){gsub("\\.1", "", x)})
)

mcmc_ciplot <- (ggplot(beta_estimates, aes(x = effects, y = post.mean))
	+ geom_point(color = "black")
	+ geom_linerange(aes(ymin=lowerCI, ymax=upperCI))
	+ geom_hline(yintercept = 0, lty=3)
	+ ylab("Effect sizes")
	+ xlab("")
	+ coord_flip()
)

mcmc_ciplot

# Fixed effect Trace plots
## beta estimates
betaChains <- as.mcmc(multimcmc_model[["Sol"]])
plotTrace(betaChains, axes = TRUE, las = 1)

## Variance covariance estimates
vcvChains <- as.mcmc(multimcmc_model[["VCV"]])
plotTrace(vcvChains, axes = TRUE, las = 1)

## Normal trace plot
plot(multimcmc_model)

# Proportion of variance explained by the random factors
rand <- multimcmc_model[["VCV"]]/apply(multimcmc_model[["VCV"]], 1, sum)

## Get median values (50%) and 95% quantiles
prop_var <- (rand
	%>% apply(., 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))
	%>% data.frame()
	%>% setnames(names(.), gsub("trait", "", names(.)))
	%>% setnames(names(.), gsub("\\.1", "", names(.)))
	%>% setnames(names(.), gsub("service", "svc", names(.)))
)
prop_var

## Get the mean value
mean_values <- (rand
	%>% apply(., 2, mean)
	%>% data.frame()
	%>% setnames(names(.), "mean")
	%>% rownames_to_column("labels")
	%>% mutate(labels = gsub("trait", "", labels))
	%>% mutate(labels = gsub("\\.1", "", labels))
	%>% mutate(labels = gsub("service", "svc", labels))
)
mean_values

save(file = "multimcmcDiagnostics.rda"
	, multimcmc_model
)

