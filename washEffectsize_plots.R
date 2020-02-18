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

extract_coefs_df

### Function to plot for different model classes i.e., glm and glmer
effectsizeFunc <- function(df){
	estimates_df <- df
	parameters <- pull(estimates_df, parameter) %>% unique()
	estimates_df <- (estimates_df
		%>% mutate(parameter = factor(parameter, levels = parameters, labels = parameters))
	)

	pos <- ggstance::position_dodgev(height=0.5)

	p1 <- (ggplot(estimates_df, aes(x = estimate, y = term, colour = model))
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
	return(p1)
}

## Year 1 model
y1effectsize_plot <- (extract_coefs_df
	%>% filter(grepl("glmSc|glmUn", model))
	%>% effectsizeFunc(.)
)

print(y1effectsize_plot)

save(file = "washEffectsize_plots.rda"
	, y1effectsize_plot
)
