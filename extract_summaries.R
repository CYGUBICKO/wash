#### ---- Project: APHRC Wash Data ----
#### ---- Task: Extract just needed ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Sep 06 (Fri) ----

library(DT)
library(data.table)
library(tibble)
library(tidyr)
library(dplyr)
options(dplyr.width = Inf)

library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))

library(rstanarm)
library(bayesplot)
library(broom)


load("simulateHierarchicalmvn.rda")
load("summary_plot_data.rda")


#### ---- Setup for Viz ----
nhhid <- 30	# Number of hhid to vizualize

##### ---- Extract some key summaries ----
nhouseholds <- length(unique(sim_dflist[[1]]$hhid))
nyears <- length(unique(sim_dflist[[1]]$years))
sims_df <- (sim_dflist[[1]]
	%>% filter(hhid %in% sample(hhid, 5))
	%>% mutate_at(c("y1", "y2", "y3", "wealthindex"), function(y){round(y, 3)})
	%>% datatable(caption = "Simulated dataset", rownames = FALSE)
)

# Tidy true betas and sigma
betas_df <- (betas_df
   %>% mutate(coef_clean = coef
		, coef_clean = ifelse(grepl("0$", coef_clean), paste0("y", substr(n, 1, 1), "|(Intercept)")
         , ifelse(grepl("x:y[1-9]", coef_clean), paste0("y", substr(n, 1, 1), "|wealthindex")
            , ifelse(grepl("_sd", coef_clean), paste0("sd_y", n, "|(Intercept).years")
            	, ifelse(grepl("^cor_", coef_clean), paste0("cor_y", substr(n, 1, 1), "|(Intercept).y", substr(n, 2, 2), "|(Intercept).years")
						, coef_clean
					)
				)
         )
      )
   )
)

# Tidy the random intercept estimates
betas0_df <- (betas0_dflist[[1]]
	%>% mutate_at("years", as.factor)
	# I thing these estimates include B0 (so substract to compare)
	%>% mutate(y1 = y1 - betas[["y1_beta0"]]
		, y2 = y2 - betas[["y2_beta0"]]
		, y3 = y3 - betas[["y3_beta0"]]
	)
)
hhRE_df <- (hhRE_dflist[[1]]
	%>% setnames(c("X1", "X2", "X3"), c("y1", "y2", "y3"))
	%>% mutate_at("hhid", as.factor)
	%>% distinct()
)

# Fixed effects

fixed_effects <- (summary(rstanmodel, regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex")
	%>% round(., digits = 4)
	%>% datatable()
)
fixed_effects

## Intercept and slope

#plot(rstanmodel, regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex")

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
		+ geom_point(data = betas_df
			%>% mutate(coef_clean2 = coef_clean)
			%>% filter(grepl("^y[1-3]\\|\\(Intercept\\)|wealthindex", coef_clean2))
			, aes(x = coef_clean, y = betas), colour = "red"
		)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme(text = element_text(size=20))
)
population_est_plot


## Year and hhid variance

# Sigma - Years
sigma_est_plot_years <- (plot_df
	%>% filter(grepl("^Sigma\\[years", parameter))
	%>% mutate(parameter = gsub("\\|\\(Intercept)", "", parameter))
#	%>% mutate(ny = ifelse(lengths(regmatches(parameter, gregexpr("y1", parameter))) == 2
#			, 2 
#			, ifelse(lengths(regmatches(parameter, gregexpr("y2", parameter))) == 2
#				, 2
#				, ifelse(lengths(regmatches(parameter, gregexpr("y3", parameter))) == 2
#					, 2
#					, lengths(regmatches(parameter, gregexpr("y1|y2|y2", parameter)))
#				) 
#			)
#		)
#	)
#	%>% filter(ny == 2)
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
		+ geom_point(data = covmat_df
			, aes(x = coef_clean, y = values), colour = "red"
		)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme(text = element_text(size=20))
)
sigma_est_plot_years

# Sigma - hhid
sigma_est_plot_hhids <- (plot_df
	%>% filter(grepl("^Sigma\\[hhid", parameter))
	%>% mutate(parameter = gsub("\\|\\(Intercept)", "", parameter))
#	%>% mutate(ny = ifelse(lengths(regmatches(parameter, gregexpr("y1", parameter))) == 2
#			, 2 
#			, ifelse(lengths(regmatches(parameter, gregexpr("y2", parameter))) == 2
#				, 2
#				, ifelse(lengths(regmatches(parameter, gregexpr("y3", parameter))) == 2
#					, 2
#					, lengths(regmatches(parameter, gregexpr("y1|y2|y2", parameter)))
#				) 
#			)
#		)
#	)
#	%>% filter(ny == 2)
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
		+ geom_point(data = covmat_df 
			%>% mutate(coef_clean = gsub("years", "hhid", coef_clean))
			, aes(x = coef_clean, y = values), colour = "red"
		)
		+ coord_flip()
		+ labs(x = NULL, y = NULL)
		+ theme(text = element_text(size=20))
)
sigma_est_plot_hhids

#print(plot(rstanmodel, regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex")
#	+ geom_point(data = betas_df %>% filter(grepl("^y[1-3]\\|\\(Intercept\\)|wealthindex", coef_clean))
#		, aes(x = betas, y = coef_clean), colour = "red"
#	)
#)
betas_dens_plot <- (plot(rstanmodel, "dens", regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex") 
	+ geom_vline(data = betas_df %>% filter(grepl("^y[1-3]\\|\\(Intercept\\)|wealthindex", coef_clean))
		%>% setnames(c("coef_clean", "betas"), c("Parameter", "Value"))
		, aes(xintercept = Value)
		, linetype = "dashed"
		, colour = "red"
	)
   + facet_wrap(~Parameter, scales = "free", ncol = 3)
	+ theme(strip.text.x = element_text(size = 6))
)
betas_dens_plot

## Year-specific estimates

#plot(rstanmodel, "mcmc_areas", pars = "varying")
# plot(rstanmodel, regex_pars = "^b\\[y[1-3]\\|\\(Intercept\\) years:")

# Group level estimates - Year
patterns <- c("^b\\[y1", "^b\\[y2", "^b\\[y3")

year_est_plots <- list()
for (i in 1:length(patterns)){
	year_est_plot <- (plot_df
		%>% filter(grepl(patterns[i], parameter) & grepl("years", parameter))
#		%>% mutate(parameter = gsub("\\|\\(Intercept)", "", parameter))
		%>% mutate(parameter = gsub("b\\[y[1-3]\\|\\(Intercept\\) years:|\\]", "", parameter))
		%>% ggplot(aes(x = as.factor(reorder(parameter, m)), y = m))
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
			+ geom_point(data = betas0_df
				, aes_string(x = "years", y = gsub(".*\\[", "", patterns[i])), colour = "red"
			)
			+ coord_flip()
			+ labs(x = "Years", y = "b (Intercept)")
			+ ggtitle(paste0("Service", " ", gsub(".*\\[", "", patterns[i])))
			+ theme(plot.title = element_text(hjust = 0.5)
				, text = element_text(size=15)
			)
	)
	year_est_plots[[i]] <- year_est_plot
}

#plot_grid(year_est_plots[[1]]
#	, year_est_plots[[2]]
#	, year_est_plots[[3]]
#	, nrow = 1
#	, scale = c(1, 1, 1)
#)

year_est_plots[[1]]
year_est_plots[[2]]
year_est_plots[[3]]

## HH-specific estimates

# plot(rstanmodel, regex_pars = "^b\\[y[1-3]\\|\\(Intercept\\) hhid:")
sampledHH_df <- sample_n(hhRE_df, nhhid)
sampledHHid <- pull(sampledHH_df, hhid)

patterns <- c("^b\\[y1", "^b\\[y2", "^b\\[y3")


hhid_est_plots <- list()
for (i in 1:length(patterns)){
	hhid_est_plot <- (plot_df
		%>% filter(grepl(patterns[i], parameter) & grepl("hhid", parameter))
#		%>% mutate(parameter = gsub("\\|\\(Intercept)", "", parameter))
		%>% mutate(parameter = gsub("b\\[y[1-3]\\|\\(Intercept\\) hhid:|\\]", "", parameter))
#		%>% sample_n(nhhid)
		%>% filter(parameter %in% sampledHHid)
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
			+ geom_point(data = sampledHH_df
				, aes_string(x = "hhid", y = gsub(".*\\[", "", patterns[i])), colour = "red"
			)
			+ coord_flip()
			+ labs(x = "Households", y = "b (Intercept)")
			+ ggtitle(paste0("Service", " ", gsub(".*\\[", "", patterns[i])))
			+ theme(plot.title = element_text(hjust = 0.5)
				, text = element_text(size=15)
			)
	)
	hhid_est_plots[[i]] <- hhid_est_plot
}

hhid_est_plots[[1]]
hhid_est_plots[[2]]
hhid_est_plots[[3]]

## Covariance and Sigma

#plot(rstanmodel, "mcmc_areas", pars = "varying")
summaryTwoLevelModelVar<- tidy(rstanmodel, intervals=TRUE, prob=.95)
datatable(print(summaryTwoLevelModelVar, digits = 2))
quit()
true_cor_df <- (betas_df
	%>% filter(grepl("cor_", coef))
	%>% mutate(Parameter = gsub("years", "hhid", Parameter))
	%>% rbind(., filter(betas_df, grepl("cor_", coef)))
	%>% select(c("Parameter", "Value"))
	%>% setnames(names(.), c("term", "true_value"))
)
cor_tabs <- (tidy(rstanmodel, parameters = "hierarchical")
	%>% right_join(true_cor_df)
	%>% select(c("term", "group", "true_value", "estimate"))
	%>% mutate(diff = round(abs(estimate - true_value), 2))
	%>% mutate(term = gsub("\\|\\(Intercept)", "", term))
	%>% datatable(options = list(pageLength = 20), rownames = FALSE)
	%>% formatRound(columns=c("estimate"), digits = 4)
	%>% formatStyle("diff"
		, backgroundColor = styleInterval(0.1, c("lightgreen", "red"))
	)
)
cor_tabs

##### ---- Save output ----

save(file = "extract_summaries.rda"
	, nhouseholds
	, nyears
	, sims_df
	, fixed_effects
	, population_est_plot
	, sigma_est_plot_years
	, sigma_est_plot_hhids
	, betas_dens_plot
	, year_est_plots
	, hhid_est_plots
	, cor_tabs
)

