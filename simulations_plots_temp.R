library(DT)
library(data.table)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(broom)

theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))

load("simulateHierarchicalmvn.rda")
load("rstanarmModelbin.rda")


rstanmodel <- rstanmodel_list[[1]]

plot_df <- plot(rstanmodel)[["data"]]
summary_plot <- (plot_df
	%>% filter(grepl("^y[1-3]", parameter))
	%>% ggplot(aes(x = reorder(parameter, m), y = m))
		+ geom_hline(yintercept = 0, size = 1/2, color = "firebrick4", alpha = 1/10)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 2/5, color = "deepskyblue4"
		)
		+ geom_segment(aes(y = ll
			, yend = hh
			, x = parameter
			, xend = parameter
		) 
			, size = 0.5, color = "azure3"
		)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 0.7, color = ""
		)
		+ geom_point(color = "azure3", size = 3)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme_bw()
)
summary_plot

plot(rstanmodel, regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex")




quit()
summary_plot <- (plot_df
	%>% filter(grepl("^y[1-3]", parameter))
	%>% ggplot(aes(x = reorder(parameter, m), y = m))
#		+ geom_point(color = "deepskyblue", size = 3, alpha = 1/10)
		+ geom_hline(yintercept = 0, size = 1/2, color = "firebrick4", alpha = 1/10)
		+ geom_segment(aes(y = ll
			, yend = hh
			, x = parameter
			, xend = parameter
		) 
			, size = 2/5, color = "deepskyblue"
		)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 1, color = "deepskyblue4", alpha = 1/10
		)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme_bw()
)
summary_plot
summary_plot <- (rstanmodel
	%>% summary()
	%>% data.frame()
	%>% rownames_to_column("terms")
	%>% filter(!terms %in% c("log-posterior", grep("_PPD", terms, value = TRUE)))
	%>% select(c("terms", "mean", "sd", "X2.5.", "X50.", "X97.5.", "n_eff"))
	%>% setnames(names(.), c("terms", "mean", "sd", "lb", "median", "lu", "n_eff"))
	%>% ggplot(aes(x = reorder(terms, mean), y = mean))
		+ geom_hline(yintercept = 0, size = 1/2, color = "firebrick4", alpha = 1/10)
		+ geom_pointrange(aes(ymin = lb, ymax = lu), size = 2/5, shape = 20, color = "firebrick4")
		+ geom_segment(aes(y = lb
			, yend = lu
			, x = terms
			, xend = terms
		) 
			, size = 3, color = "firebrick4", alpha = 1/10
		)
		+ coord_flip()
)
summary_plot


posterior_df <- as.matrix(rstanmodel)

summary(rstanmodel)

quit()
posterior_estimates <- (t(apply(posterior_df, 2, quantile, probs = c(0.1, 0.5, 0.9)))
	%>% data.frame()
	%>% rownames_to_column("terms")
	%>% mutate(Variable = ifelse(grepl("y1|", terms), "y1"
   	, ifelse(grepl("y2|", terms), "y2", "y3")
   	)
	)
)

posterior_estimates
