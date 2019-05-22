#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCglmm summary plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 02 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("binaryMcmcglmm.rda")

# Incoming objects:
# * mcmcglmm_list - glmer fits per simulation
# * mcmcglmmcoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

mcmcglmm_plot <- (mcmcglmmcoef_df
   %>% gather(coef, value)
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
print(mcmcglmm_plot)

save(file = "mcmcglmmSummary.rda"
	, mcmcglmm_list
	, mcmcglmm_plot
)

