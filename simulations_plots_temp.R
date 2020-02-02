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

# Population level estimate plot
population_est_plot <- (plot_df
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
			, size = 0.8, color = "lightblue"
		)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 1, color = "deepskyblue4"
		)
		+ geom_point(color = "lightblue", size = 3.5)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme_bw()
)
population_est_plot

# Sigma
sigma_est_plot <- (plot_df
	%>% filter(grepl("^Sigma", parameter))
	%>% mutate(ny = ifelse(lengths(regmatches(parameter, gregexpr("y1", parameter))) == 2
			, 2 
			, ifelse(lengths(regmatches(parameter, gregexpr("y2", parameter))) == 2
				, 2
				, ifelse(lengths(regmatches(parameter, gregexpr("y3", parameter))) == 2
					, 2
					, lengths(regmatches(parameter, gregexpr("y1|y2|y2", parameter)))
				) 
			)
		)
	)
	%>% filter(ny == 2)
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
			, size = 0.8, color = "lightblue"
		)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 1, color = "deepskyblue4"
		)
		+ geom_point(color = "lightblue", size = 3.5)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme_bw()
)
sigma_est_plot

# Group level estimates - Year
year_est_plot <- (plot_df
	%>% filter(grepl("^b\\[", parameter) & grepl("years", parameter))
	%>% mutate(variables = ifelse(grepl("y1", parameter), "y1"
			, ifelse(grepl("y2", parameter), "y2"
				, "y3"
			)
		)
		, parameter = gsub("y[1-3]", "", parameter)
	)
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
			, size = 0.8, color = "lightblue"
		)
		+ geom_pointrange(aes(ymin = l
			, ymax = h
		)
			, size = 1, color = "deepskyblue4"
		)
		+ geom_point(color = "lightblue", size = 3.5)
		+ facet_grid(.~ variables, scales = "free")
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme_bw()
)
year_est_plot

quit()
patterns <- c("^b\\[y1", "^b\\[y2", "^b\\[y3")

year_est_plots <- list()
for (i in 1:length(patterns)){
	year_est_plot <- (plot_df
		%>% filter(grepl(patterns[i], parameter) & grepl("years", parameter))
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
				, size = 0.8, color = "lightblue"
			)
			+ geom_pointrange(aes(ymin = l
				, ymax = h
			)
				, size = 1, color = "deepskyblue4"
			)
			+ geom_point(color = "lightblue", size = 3.5)
			+ coord_flip()
			+ labs(x = NULL, y = NULL)
			+ theme_bw()
	)
	year_est_plots[[i]] <- year_est_plot
}

bayesplot_grid(year_est_plots[[1]]
	, year_est_plots[[2]]
	, year_est_plots[[3]]
	, grid_args = list(ncol = 1)
)

