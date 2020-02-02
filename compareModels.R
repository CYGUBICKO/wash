#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Compare model estimates ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 06 (Mon) ----

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(lme4)
library(MCMCglmm)
library(lattice)
library(dotwhisker)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("glmerDiagnostics.rda")
load("multimcmcDiagnostics.rda")

# Input files
## lme4_model
## multimcmc_model
## long_df

# Compare parameter estimates

compareFunc <- function(mcmc_model, lme4_model, whichvar = 1:3, use.units = FALSE){
	mcmc_fixed <- (mcmc_model[["Sol"]]
		%>% as.data.frame()
		%>% gather(variable)
		%>% mutate(type = "fixed")
	)
	mcmc_random <- (mcmc_model[["VCV"]]
		%>% as.data.frame()
		%>% gather(variable)
		%>% mutate(type = "random")
	)
	mcmc_est <- (rbind(mcmc_fixed, mcmc_random)
		%>% mutate(variable = gsub("trait", "", variable))
		%>% mutate(variable = gsub("\\.1", "", variable))
		%>% mutate(variable = gsub("service", "svc", variable))
		%>% filter(!grepl("svc3:svc2|svc3:svc1|svc2:svc3|svc2:svc1|svc1:svc3|svc1:svc2", variable))
	)

	lme4_fixed <- (fixef(lme4_model)
		%>% as.data.frame()
		%>% setnames(names(.), "value")
		%>% rownames_to_column("variable")
		%>% mutate(type = "fixed")
		%>% mutate(variable = gsub("serviceservice", "svc", variable))
	)
	lme4_vcv <- (VarCorr(lme4_model)
		%>% as.data.frame()
		%>% filter(is.na(var2))
		%>% mutate(grp = paste0("serviceservice", extract_numeric(var1), ":", var1, ".", grp))
		%>% select(c("grp", "sdcor"))
		%>% setnames(c("grp", "sdcor"), c("variable", "value"))
		%>% mutate(type = "random")
		%>% mutate(variable = gsub("serviceservice", "svc", variable))
		%>% slice(whichvar)
	)
	lme4res <- rbind(lme4_fixed, lme4_vcv)

	mcmc_summary <- summary(mcmc_model)
	mcmc_fixres <- (mcmc_summary[["solutions"]]
		%>% as.data.frame()
		%>% rownames_to_column("variable")
		%>% mutate(type = "fixed")
		%>% setnames("post.mean", "value")
		%>% select(c("type", "variable", "value"))
		%>% mutate(variable = gsub("trait", "", variable))
		%>% mutate(variable = gsub("\\.1", "", variable))
		%>% mutate(variable = gsub("service", "svc", variable))
	)
	mcmc_vcvres <- (data.frame(type = "random"
			, variable = colnames(mcmc_model[["VCV"]])
			, value = c(mcmc_summary[["Gcovariances"]][, "post.mean"]
				, mcmc_summary[["Rcovariances"]][, "post.mean"]
			)
		)
		%>% mutate(variable = gsub("trait", "", variable))
		%>% mutate(variable = gsub("\\.1", "", variable))
		%>% mutate(variable = gsub("service", "svc", variable))
		%>% filter(variable %in% c(paste0(paste0(rep("svc", 3), 1:3), ":", paste0(rep("svc", 3), 1:3), ".hhid_anon")))
	)
	MGres <- (rbind(mcmc_fixres, mcmc_vcvres[whichvar,])
	)
	allres <- rbind(data.frame(sum = "MCMCglmm mean", MGres)
		, data.frame(sum = "lme4 MLE",lme4res)
	)
	if (!use.units){
		mcmc_est <- (mcmc_est
			%>% filter(!grepl("\\.units", variable))
		)
	}
	list(mcmc_est = mcmc_est, allres = allres)
}

## Comparison plot

compare_est <- compareFunc(multimcmc_model, lme4_model, whichvar = 1:3)
compare_est_plot <- (ggplot(compare_est[["mcmc_est"]], aes(x = variable, y = value))
	+ geom_violin(fill = "gray")
	+ geom_point(data = compare_est[["allres"]], aes(colour = sum), alpha = 0.7)
	+ facet_wrap(~type, scales = "free")
	+ labs(x = "Beta", y = "Estimate", colour = "Model")
	+ theme(legend.position = "bottom")
	+ coord_flip()
)
compare_est_plot
save(file = "compareModels.rda"
)

