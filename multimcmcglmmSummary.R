#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCglmm summary plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 02 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(MCMCglmm)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("multiMcmcglmm.rda")

# Incoming objects:
# * multimcmcglmm_list - glmer fits per simulation
# * multimcmcglmmcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

multimcmcglmmcoef_list <- list()
for (i in 1:length(multimcmcglmm_list)){
	mcmc_mgl <- multimcmcglmm_list[[i]]
	c2 <- ((16 * sqrt(3))/(15 * pi))^2
	mcmc_names <- names(data.frame(mcmc_mgl[["VCV"]]))
	mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)] <- 
	mcmc_mgl[["VCV"]][, grep("\\.hhid_anon", mcmc_names)]/(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)])
	mcmc_mgl[["Sol"]][, 4:6] <- mcmc_mgl[["Sol"]][, 4:6]/sqrt(1 + c2 * mcmc_mgl[["VCV"]][, grep("\\.units", mcmc_names)[c(1, 5, 9)]])
	mcmc_mgl[["VCV"]][, c(1, 5, 9)] <- sqrt(mcmc_mgl[["VCV"]][, c(1, 5, 9)]) 
	model_summary <- summary(mcmc_mgl)
	multimcmcglmmcoef_list[[i]] <- model_summary[["solutions"]][,1]
}

multimcmcglmmcoef_df <- Reduce(rbind, multimcmcglmmcoef_list) %>% as_tibble()

multimcmcglmm_plot <- (multimcmcglmmcoef_df
   %>% gather(coef, value)
	%>% mutate_at("coef", function(x){gsub("trait", "service", x)})
	%>% mutate_at("coef", function(x){gsub("\\.1", "", x)})
   %>% ggplot(aes(x = value))
   + geom_histogram()
   + geom_vline(data = betas_df, aes(xintercept = betas, color = coef)
      , linetype="dashed"
   )
   + labs(x = "Betas", y = "Count")
   + ggtitle("Fitted vs True betas")
   + guides(colour = FALSE)
   + theme(plot.title = element_text(hjust = 0.5))
   + facet_wrap(~coef, scales = "free")
)
print(multimcmcglmm_plot)

save(file = "multimcmcglmmSummary.rda"
	, multimcmcglmm_list
	, multimcmcglmm_plot
)

