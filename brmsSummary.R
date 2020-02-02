#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- brms simulation plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 13 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("brmsModel.rda")

# Incoming objects:
# * brmsmodel_list - glmer fits per simulation
# * brmscoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors 

brmsmodel_plot <- (brmscoef_df
   %>% gather(coef, value)
   %>% ggplot(aes(x = value))
   + geom_histogram()
   + geom_vline(data = (betas_df
			%>% filter(grepl("b_", coef))
			%>% mutate(coef = gsub("b_", "", coef))
		)
		, aes(xintercept = betas, color = coef)
      , linetype="dashed"
   )
   + labs(x = "Betas", y = "Count")
   + ggtitle("Fitted vs True betas")
   + guides(colour = FALSE)
   + theme(plot.title = element_text(hjust = 0.5))
   + facet_wrap(~coef, scales = "free")
)
print(brmsmodel_plot)

save(file = "brmsSummary.rda"
	, brmsmodel_list
	, brmsmodel_plot
)

