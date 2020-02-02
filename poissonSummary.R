#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Complex glmer summary plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("poissonGlmer.rda")

# Incoming objects:
# * poissonglmer_list - glmer fits per simulation
# * poissoncoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

poissonglmer_plot <- (poissoncoef_df
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
print(poissonglmer_plot)

save(file = "poissonSummary.rda"
	, poissonglmer_list
	, poissonglmer_plot
)

