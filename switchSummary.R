#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Checkplots and coef summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 09 (Mon) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2); theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))
library(lme4)

load("switchModel.rda")

set.seed(7748)

## check plots: Extract pvalue estimates to do checkplots
### parms = list(parm est. as per the glmer object = true value)

modelCheck <- function(glmerobj, parms = list()){
	
	psummary <- coefficients(summary(glmerobj))	
	if (nrow(psummary) != length(parms)) stop("Number of parms must be equal to ", nrow(psummary))
	
	pnames <- names(parms)
	pv <- list()

	for (parm in pnames){
		
		real <- parms[[parm]]
		est <- psummary[parm, "Estimate"]
		se <- psummary[parm, "Std. Error"]
	
		zv <- abs(real - est)/se
		pv[[parm]] <- 2*pnorm(zv, lower.tail = FALSE)
	}
	pvalues <- do.call("rbind", pv)
	return(data.frame(pvalues))
}

## Model objects

pp <- list("servicesy1" = b_gain1
	, "servicesy2" = b_gain2
	, "servicesy1:statusP" = b_add1
	, "servicesy2:statusP" = b_add2
	, "servicesy1:xm" = s1_M
	, "servicesy2:xm" = s2_M
)

nsims <- length(glmermodel_list)
checkvals <- list()
for (s in 1:nsims){
	checkvals[[s]] <- (modelCheck(glmerobj = glmermodel_list[[s]], parms = pp)
		%>% rownames_to_column("parms")
	)
}

pv_df <- bind_rows(checkvals)

for(p in names(pp)){
	df <- (pv_df
		%>% filter(parms == p)
	)
	pvalues <- pull(df, pvalues)
#	print(ggplot(df, aes(x = pvalues))
#		+ geom_histogram()
#		+ labs(title = p
#			, x = "P-values"
#		)
#	)
	hist(pvalues, main = p)
}


## True vs estimates Histograms


### Joint models

# y1
print(ggplot(glmercoef_df %>% filter(variables=="y1"), aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y1", coefs2))
		%>% mutate(coefs = gsub("[1-2]", "", coefs))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

# y2
print(ggplot(glmercoef_df %>% filter(variables=="y2"), aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y2", coefs2))
		%>% mutate(coefs = gsub("[1-2]", "", coefs))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

quit()
### Separate models

### y1
print(ggplot(y1coef_df, aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y1", coefs2))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

### y2
print(ggplot(y2coef_df, aes(x = values))
	+ geom_histogram()
   + geom_vline(data = betas_df
		%>% filter(grepl("^y2", coefs2))
		, aes(xintercept = betas, color = coefs2)
      , linetype="dashed"
   )
	+ facet_wrap(~coefs, scales = "free")
	+ guides(colour = FALSE)
)

