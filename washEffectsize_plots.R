#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Plot model efffect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Feb 12 (Wed) ----

library(ggplot2)
library(dplyr)

source("../funs/ggplot_theme.R")
source("../funs/effectsPlot.R")

load("washTidyestimates.rda")

estimates_df <- extract_coefs_df
parameters <- pull(estimates_df, parameter) %>% unique()
estimates_df <- (estimates_df
	%>% mutate(parameter = factor(parameter, levels = parameters, labels = parameters))
)

pos <- ggstance::position_dodgev(height=0.5)

effectsize_plot <- (ggplot(estimates_df, aes(x = estimate, y = term, colour = model))
	+ geom_point(position = pos)
	+ ggstance::geom_linerangeh(aes(xmin = conf.low, xmax = conf.high), position = pos)
	+ scale_colour_brewer(palette="Dark2"
		, guide = guide_legend(reverse = TRUE)
	)
	+ geom_vline(xintercept=0,lty=2)
	+ labs(x = "Estimate"
		, y = ""
		, colour = "Model"
	)
	+ facet_wrap(~parameter, scale = "free", ncol = 2)
	+ facet_theme
	+ theme(legend.position = "bottom")
)
print(effectsize_plot)

save(file = "washEffectsize_plots.rda"
	, effectsize_plot
)
