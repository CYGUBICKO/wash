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
load("switchSingleModel.rda")

## Plot funs
#source("ggplot_theme.R")

plotEffects <- function(df, var, xlabs){
	pos <- position_dodge(0.2)
	p1 <- (ggplot(df, aes_string(x = var, y = "fit", group = "services"))
		+ scale_color_discrete(breaks = c("y1", "y2"))
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

## Predict effect sizes

mod <- glmer_model
mod_effect_df <- predictorEffects(mod)
mod_effect_df <- as.data.frame(mod_effect_df)

### Service level
service_df <- (data.frame(mod_effect_df[["services"]])[, c("services", "fit", "lower", "upper")]
	%>% group_by(services) 
	%>% summarise_at(vars(fit:upper), mean, na.rm = TRUE)
)
service_plot <- (ggplot(service_df, aes(x = services, y = fit))
	+ geom_point(size = 0.6)
	+ geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)
	+ scale_x_discrete(limits = c("y1", "y2"))
	+ labs(x = "Services gain"
		, y = "Probability of\nimproved service"
	)
)
print(service_plot)

cat_vars <- c("slumarea")
cat_plots <- lapply(cat_vars, function(x){plotEffects(mod_effect_df[[x]], x, x)})
p1 <- (cat_plots[[1]]
   + theme(plot.margin = margin(0, 1, 0.1, 1, "cm")
      , panel.spacing.x = unit(1, "lines")
   )
)
print(p1)

num_vars <- c("years"
	, "xm"
	, "statusP"
)
num_plots <- lapply(num_vars, function(x){plotEffects(mod_effect_df[[x]], x, x)})
print(num_plots)

