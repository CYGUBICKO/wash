#### ---- Project: APHRC Wash Data ----
#### ---- Task: Extract just needed ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Sep 06 (Fri) ----

library(DT)
library(data.table)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstanarm)
library(bayesplot)
library(broom)


load("simulateHierarchicalmvn.rda")
load("summary_plot_data.rda")

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


## ----------------------------------------------------------------------
## 
# Lazy to rerun the whole code to include covMat in the .rda because it takes days. But should work on this in the next rerun

# Correlation matrix
cor_y1y2 <- 0.20
cor_y1y3 <- 0.30
cor_y2y3 <- 0.50
corMat <- matrix(
	c(1, cor_y1y2, cor_y1y3
		, cor_y1y2, 1, cor_y2y3
		, cor_y1y3, cor_y2y3, 1
	), 3, 3
)

# Sd
y1_sd <- 0.5
y2_sd <- 0.3
y3_sd <- 0.7
sdVec <- c(y1_sd, y2_sd, y3_sd)
varMat <- sdVec %*% t(sdVec)
varMat
corMat
# varcov matrix
covMat <- varMat * corMat
covMat

## Create cov-varaince dataframe
covmat_df <- (
	data.frame(coef = c("y1y1", "y2y2", "y3y3", "y2y1", "y3y1", "y3y2")
		, values = c(diag(covMat), covMat[lower.tri(covMat)])
	)
	%>% mutate(n = extract_numeric(coef)
		, coef_clean = paste0("Sigma[years:y", substr(n, 1, 1), ",y", substr(n, 2, 2), "]")
	) 
)

## ----------------------------------------------------------------------


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
			+ ggtitle(paste0("Service", " ", gsub(".*\\[", "", patterns[i])))
			+ theme(plot.title = element_text(hjust = 0.5))
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
nhhid <- 100	# Number of hhid to vizualize
patterns <- c("^b\\[y1", "^b\\[y2", "^b\\[y3")

hhid_est_plots <- list()
for (i in 1:length(patterns)){
	hhid_est_plot <- (plot_df
		%>% filter(grepl(patterns[i], parameter) & grepl("hhid", parameter))
		%>% sample_n(nhhid)
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
			+ ggtitle(paste0("Service", " ", gsub(".*\\[", "", patterns[i])))
			+ theme(plot.title = element_text(hjust = 0.5))
	)
	hhid_est_plots[[i]] <- hhid_est_plot
}

hhid_est_plots[[1]]
hhid_est_plots[[2]]
hhid_est_plots[[3]]

## Covariance and Sigma

#plot(rstanmodel, "mcmc_areas", pars = "varying")
#summaryTwoLevelModelVar<- tidy(rstanmodel, intervals=TRUE, prob=.95, parameters = "hierarchical")
#datatable(print(summaryTwoLevelModelVar, digits = 2))

true_cor_df <- (betas_df
	%>% filter(grepl("cor_|_sd", coef))
	%>% mutate(Parameter = gsub("years", "hhid", Parameter))
	%>% rbind(., filter(betas_df, grepl("cor_|_sd", coef)))
	%>% select(c("Parameter", "Value"))
	%>% setnames(names(.), c("term", "true_value"))
)
cor_tabs <- (tidy(rstanmodel, parameters = "hierarchical")
	%>% right_join(true_cor_df)
	%>% select(c("term", "group", "true_value", "estimate"))
	%>% mutate(diff = round(abs(estimate - true_value), 2))
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

