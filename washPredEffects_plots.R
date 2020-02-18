#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Extract predicted effect sizes ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2020 Jan 11 (Sat) ----

library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))
library(ggpubr)

library(dplyr)
library(effects)
library(lme4)
library(splines)

source("../funs/ggplot_theme.R")
load("washPredEffects.rda")


### Plot effects
plotEffects <- function(df, var, xlabs){
	pos <- position_dodge(0.2)
	p1 <- (ggplot(df, aes_string(x = var, y = "fit", group = "services"))
		+ scale_color_discrete(breaks = c("toilettype", "garbagedposal", "watersource"))
		+ labs(x = xlabs
			, y = "Probability of\nimproved service"
			, colour = "Services"
		)
		+ theme(legend.position = "bottom")
	)
	if (grepl("numeric|integer", class(df[[var]]))){
		p2 <- (p1 + geom_smooth(aes(ymin = lower, ymax = upper, fill = services, colour = services)
				, stat = "identity"
				, size = 0.5
			)
			+ guides(fill = FALSE)
		)
	} else {
		p2 <- (p1 + geom_point(size = 0.6)
			+ geom_line()
			+ geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)
			+ facet_wrap(~services)
			+ facet_theme
		)
	}
	return(p2)
}


## Year 1 model
### Service level

y1service_plot <- (ggplot(y1service_effect_df, aes(x = services, y = fit))
	+ geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, colour = "steelblue3")
	+ geom_point(colour = "blue")
	+ scale_x_discrete(limits = c("toilettype", "garbagedposal", "watersource"))
	+ scale_y_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, 0.1))
	+ labs(x = "Service"
		, y = "Probability of\nimproved service"
	)
)
print(y1service_plot)

### Other remaining predictors
pred_vars <- names(y1mod_effect_df)[!names(y1mod_effect_df) %in% "services"]

y1pred_effect_plots <- lapply(pred_vars, function(x){
	plotEffects(y1mod_effect_df[[x]], x, grep(x, pred_vars, value = TRUE))
})

print(y1pred_effect_plots)

## Previous year model
### Service level

pyrservice_plot <- (ggplot(pyrservice_effect_df, aes(x = services, y = fit))
	+ geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, colour = "steelblue3")
	+ geom_point(colour = "blue")
	+ scale_x_discrete(limits = c("toilettype", "garbagedposal", "watersource"))
	+ scale_y_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, 0.1))
	+ labs(x = "Service"
		, y = "Probability of\nimproved service"
	)
)
print(pyrservice_plot)

### Other remaining predictors
pred_vars <- names(pyrmod_effect_df)[!names(pyrmod_effect_df) %in% "services"]

pyrpred_effect_plots <- lapply(pred_vars, function(x){
	plotEffects(pyrmod_effect_df[[x]], x, grep(x, pred_vars, value = TRUE))
})

print(pyrpred_effect_plots)

save(file = "washPredEffects_plots.rda"
	, y1service_plot
	, y1pred_effect_plots
	, pyrservice_plot
	, pyrpred_effect_plots
)

