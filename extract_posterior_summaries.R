#### ---- Project: APHRC Wash Data ----
#### ---- Task: Extract Posterior summaries ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Oct 13 (Sun) ----

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
library(brms)
library(broom)
library(ggstance)

load("rstanarmModelbin.rda")
load("brmsModelbin.rda")
load("simulateHierarchicalmvn.rda")


#### ---- Setup for Viz ----
nhhid <- 30	# Number of hhid to vizualize
ci_probs <- c(0.025, 0.975) # CI to extact

# Plots
pos <- ggstance::position_dodgev(height=0.5)
plot_alpha <- 0.3
plot_text_size <- 13

# Models
rstanmodel <- rstanmodel_list[[1]]
brmsmodel <- brmsmodel_list[[1]]

# Covariance matrix
covmat_df <- (covmat_df
	%>% mutate(coef_clean = gsub("years:", "", coef_clean)
		, model = "true value"
	)
)

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
		, coef_clean = ifelse(grepl("0$", coef_clean), paste0("y", substr(n, 1, 1), "_intercept")
         , ifelse(grepl("x:y[1-9]", coef_clean), paste0("y", substr(n, 1, 1), "_wealthindex")
            , ifelse(grepl("_sd", coef_clean), paste0("sd_y", n, "|(Intercept).years")
					, ifelse(grepl("^cor_", coef_clean), paste0("Cor[y", substr(n, 1, 1), ",y", substr(n, 2, 2), "]")
						, coef_clean
					)
				)
         )
      )
		, model = "true value"
   )
)

print(betas_df)

# Tidy the random intercept estimates
betas0_df <- (betas0_dflist[[1]]
	%>% mutate_at("years", as.factor)
	# I thing these estimates include B0 (so substract to compare)
	%>% mutate(y1 = y1 - betas[["y1_beta0"]]
		, y2 = y2 - betas[["y2_beta0"]]
		, y3 = y3 - betas[["y3_beta0"]]
	)
	%>% gather(variable, values, -years)
	%>% mutate(model = "true value")
)
hhRE_df <- (hhRE_dflist[[1]]
	%>% setnames(c("X1", "X2", "X3"), c("y1", "y2", "y3"))
	%>% mutate_at("hhid", as.factor)
	%>% distinct()
)

# Sample HH to plot
sampledHH_df <- sample_n(hhRE_df, nhhid)
sampledHHid <- pull(sampledHH_df, hhid)
sampledHH_df <- (sampledHH_df
	%>% gather(variable, values, -hhid)
	%>% mutate(model = "true value")
)

#### ---- Extract the posteriors ----
rstanarm_cor_df <- (tidy(rstanmodel, parameters = "hierarchical")
	%>% filter(grepl("^cor_", term))
   %>% mutate(term = gsub("\\|\\(Intercept)", "", term)
		, term_labels = ifelse(grepl("^cor_", term), paste0("Cor[", gsub("\\.", ",", gsub(".*_|\\.hh.*|\\.ye.*", "", term)), "]"), term)
		, model = "rstanarm"
		, variable = "Corr"
		, effect = "Hierarchical"
		, lower = NA
		, upper = NA
		, std.error = NA
	)
	%>% setnames("group", "grouping")
)


# rstanarm

## SES estimates
rstanarm_ses_df <- (rstanmodel[["ses"]]
	%>% do.call(cbind, .)
	%>% data.frame()
	%>% setnames(names(.), c("y1", "y2", "y3"))
	%>% rownames_to_column("term")
	%>% gather(coef, std.error, -term)
	%>% mutate(term = ifelse(coef == "y2", gsub("y1", "y2", term)
		, ifelse(coef == "y3", gsub("y1", "y3", term), term))
	)
	%>% select(-coef)
)

## Point estimates and CI
old_coefnames <- c("mean", "X2.5.", "X97.5.")
new_coefnames <- c("estimate", "lower", "upper")
rstanarm_summaries_df <- (summary(rstanmodel, probs = ci_probs, regex_pars = "y[1-3]\\|w|y[1-3]\\|\\(Int|^Sig|^b\\[")
	%>% data.frame()
	%>% rownames_to_column("term")
	%>% select(-c("sd", "mcse", "n_eff", "Rhat"))
	%>% setnames(old_coefnames, new_coefnames)
	%>% left_join(rstanarm_ses_df, by = "term")
	%>% mutate(effect = ifelse(grepl("^y[1-3]\\|", term), "Fixef"
		  , ifelse(grepl("^b\\[", term), "Randef", "Hierarchical")
		)
	)
	%>% mutate(new_term = ifelse(grepl("^y[1-3]\\|", term), tolower(gsub("\\|", "_", term))
		, ifelse(grepl("hhid:", term), paste0("hhid_", gsub(".*b\\[|\\|.*", "", term), "[", gsub(".*\\:|\\].*", "", term), ",Intercept]")
			, ifelse(grepl("years:", term), paste0("years_", gsub(".*b\\[|\\|.*", "", term), "[", gsub(".*\\:|\\].*", "", term), ",Intercept]"), term)
		)
	)
		, new_term = ifelse(grepl("^Sigma", term), term, new_term)
		, new_term = ifelse(effect == "Fixef", gsub("\\(|\\)", "", new_term), new_term)
		, term = new_term
		, model = "rstanarm"
	)
	%>% select(-new_term)
)


# brms
## varcov
### HHID
varcov_est_brms <- VarCorr(brmsmodel)
hhid_varcov_est_brms <- (varcov_est_brms[["hhid"]][["cov"]]
	%>% data.frame()
	%>% t()
	%>% data.frame()
	%>% rownames_to_column("term")
	%>% gather(variable, estimate, -term)
	%>% mutate(type = gsub("\\.y[1-3].*", "", term))
	%>% mutate(term = paste0("Sigma[hhid:", gsub("bin_.*|.*\\.", "", term), "|(Intercept),", gsub("bin_.*", "", variable), "|(Intercept)]")
	)
	%>% select(-variable)
	%>% spread(type, -estimate)
	%>% mutate(combs = paste0(gsub(".*\\:|\\|\\(.*", "", term), gsub(".*\\,|\\|\\(.*", "", term)))
	%>% filter(!combs %in% c("y1y2", "y1y3", "y2y3"))
	%>% select(-combs)
	%>% setnames(names(.), c("term", "std.error", "estimate", "lower", "upper"), skip_absent = TRUE)
)

### YEARS
years_varcov_est_brms <- (varcov_est_brms[["years"]][["cov"]]
	%>% data.frame()
	%>% t()
	%>% data.frame()
	%>% rownames_to_column("term")
	%>% gather(variable, estimate, -term)
	%>% mutate(type = gsub("\\.y[1-3].*", "", term))
	%>% mutate(term = paste0("Sigma[years:", gsub("bin_.*|.*\\.", "", term), "|(Intercept),", gsub("bin_.*", "", variable), "|(Intercept)]")
	)
	%>% select(-variable)
	%>% spread(type, -estimate)
	%>% mutate(combs = paste0(gsub(".*\\:|\\|\\(.*", "", term), gsub(".*\\,|\\|\\(.*", "", term)))
	%>% filter(!combs %in% c("y1y2", "y1y3", "y2y3"))
	%>% select(-combs)
	%>% setnames(names(.), c("term", "std.error", "estimate", "lower", "upper"), skip_absent = TRUE)
)

## Put all varcov together 
varcov_est_brms_df <- (rbind(hhid_varcov_est_brms, years_varcov_est_brms)
	%>% mutate(effect = "Hierarchical")
)

brms_summaries <- posterior_summary(brmsmodel, probs = ci_probs)
brms_summaries_df <- (brms_summaries
	%>% data.frame()
	%>% setnames(names(.), c("estimate", "std.error", "lower", "upper"))
	%>% rownames_to_column("term")
	%>% mutate(effect = ifelse(grepl("^b_", term), "Fixef"
		, ifelse(grepl("^r_", term), "Randef", "Hierarchical")
	)
	 , term = gsub("^b_|^r_|bin", "", term)
	 , term = gsub("__", "_", term)
	)
	%>% filter(!grepl("^lp_|^sd_", term))
	%>% rbind(., varcov_est_brms_df)
	%>% mutate(model = "brms")
)

# Put all the model estimates together
posterior_estimates_df <- (rbind(rstanarm_summaries_df, brms_summaries_df)
	%>% mutate(variable = ifelse(!grepl("^cor_", term), gsub(".*_|\\[.*", "", term), "Corr")
		, grouping = ifelse(grepl("^year|^hhid", term), gsub("_.*", "", term)
			, ifelse(grepl("^Sigma|^cor", term), gsub(".*\\[|\\:.*|.*cor_|_y[1-3].*", "", term)
				, ifelse(grepl("^y[1-3]_i", term), "Intercept", "Slope")
			)
		)
		, term_labels = ifelse(grepl("^year|^hhid", term), gsub(".*\\[|\\,.*", "", term)
			, ifelse(grepl("^cor_", term), paste0("Cor[", gsub(".*rs_|.*id_|_Int.*", "", term), ",", gsub(".*pt_|_Int.*", "", term), "]")
				, ifelse(grepl("^Sigma", term), paste0("Sigma[", gsub(".*\\:|\\|\\(.*", "", term), ",", gsub(".*\\,|\\|\\(.*", "", term), "]"), term)
			)
		)
	)
	%>% rbind(., rstanarm_cor_df)
)

#### ---- Plot model estimates ----

## Base plot for all otherst
base_plot <- (ggplot(posterior_estimates_df
	%>% filter(effect == "Fixef")
	%>% mutate(term_labels = reorder(term_labels, estimate))
		, aes(x = estimate, y = term_labels, colour = model))
	+ geom_point(position = pos, alpha = plot_alpha, size = 1)
	+ ggstance::geom_linerangeh(aes(xmin = lower, xmax = upper), position = pos, alpha = plot_alpha)
	+ scale_colour_brewer(palette="Dark2"
		, guide = guide_legend(reverse = TRUE)
	)
	+ geom_vline(xintercept=0,lty=2)
	+ labs(x = "Estimate"
		, y = ""
		, colour = "Model"
	)
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
		, text = element_text(size = plot_text_size)
	)
)


## Population level plot
population_est_plot <- (base_plot
	+ geom_point(data = betas_df
		%>% filter(grepl("^y[1-3]_", coef_clean))
			, aes(x = betas, y = coef_clean, colour = model)
		, position = pos, size = 1
	)
	+ ggtitle("Fixed effect estimates")
)
print(population_est_plot)

## Year random effects plot
year_est_plots <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^years", grouping) & !grepl("^Hierarchical", effect))
		%>% mutate(term_labels = reorder(term_labels, as.numeric(term_labels)))
	)
	+ geom_point(data = betas0_df, aes(x = values, y = years)
		, position = pos, size = 0.5
	)
	+ ggtitle("Year random intercept estimates")
	+ facet_wrap(~variable)
)
print(year_est_plots)

## HH random effects plot
hhid_est_plots <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^hhid", grouping) & !grepl("^Hierarchical", effect))
		%>% filter(term_labels %in% sampledHHid)
		%>% mutate(term_labels = reorder(term_labels, as.numeric(term_labels)))
	)
	+ geom_point(data = sampledHH_df, aes(x = values, y = hhid)
		, position = pos, size = 0.5
	)
	+ ggtitle("HH random intercept estimates")
	+ facet_wrap(~variable)
)
print(hhid_est_plots)

## Year Covariance matrix plot
sigma_est_plot_years <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^year", grouping) & grepl("^Sigma", variable))
		%>% mutate(term_labels = reorder(term_labels, estimate))
	)
	+ geom_point(data = covmat_df, aes(x = values, y = coef_clean, colour = model)
		, position = pos, size = 1
	)
	+ ggtitle("Year variance-covariance estimates")
)
print(sigma_est_plot_years)

## hhid Covariance matrix plot
sigma_est_plot_hhids <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^hhid", grouping) & grepl("^Sigma", variable))
		%>% mutate(term_labels = reorder(term_labels, estimate))
	)
	+ geom_point(data = covmat_df, aes(x = values, y = coef_clean, colour = model)
		, position = pos, size = 1
	)
	+ ggtitle("HH variance-covariance estimates")
)
print(sigma_est_plot_hhids)

## Year correlation plot
cor_est_plot_years <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^year", grouping) & grepl("^Cor", variable))
		%>% mutate(term_labels = reorder(term_labels, estimate))
	)
	+ geom_point(data = betas_df
		%>% filter(grepl("^Cor", coef_clean))
			, aes(x = betas, y = coef_clean, colour = model)
		, position = pos, size = 1
	)
	+ ggtitle("Year correlation estimates")
)
print(cor_est_plot_years)

## hhid correlation matrix plot
cor_est_plot_hhids <- (base_plot 
	%+% (posterior_estimates_df
		%>% filter(grepl("^hhid", grouping) & grepl("^Cor", variable))
		%>% mutate(term_labels = reorder(term_labels, estimate))
	)
	+ geom_point(data = betas_df
		%>% filter(grepl("^Cor", coef_clean))
			, aes(x = betas, y = coef_clean, colour = model)
		, position = pos, size = 1
	)
	+ ggtitle("HH correlation estimates")
)
print(cor_est_plot_hhids)
##### ---- Save output ----

save(file = "extract_posterior_summaries.rda"
	, nhouseholds
	, nyears
	, sims_df
#	, fixed_effects
#	, population_est_plot
#	, sigma_est_plot_years
#	, sigma_est_plot_hhids
#	, betas_dens_plot
#	, year_est_plots
#	, hhid_est_plots
#	, cor_tabs
)

