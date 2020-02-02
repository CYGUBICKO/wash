#### ---- Project: APHRC Wash Data ----
#### ---- Task: Logistic PCA ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 9 (Sat) ----

library(dplyr)
library(logisticPCA)

load("globalFunctions.rda")
load("analysisdata.rda")

## Conduct logistic PCA to create a single variable for the three wash variables (wash_vars):
# * cat_hhwatersource
# * cat_hhtoilettype
# * cat_hhgarbagedisposal

#### ---- 1. Cross-validation -----
# Determine the optimal number of Bernoulli saturated models?? (https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html)

wash_vars_df <- select(working_df, wash_vars)
#logistic_cv <- cv.lpca(wash_vars_df, ks = 2, ms = seq(2, 14, 2))
#logistic_cv_plot <- plot(logistic_cv)
#logistic_cv_plot

### ---- 2. Logistic moldel ----

logistic_pca <- logisticPCA(wash_vars_df, k = 2, m = 0, main_effects = TRUE)

# Create the two categories (Unimproved, Improved)

score <- logistic_pca[["PCs"]][,1]
cats <- quantile(score, probs=seq(0, 1, by = 0.5), na.rm = TRUE)
working_df <- (working_df
	%>% mutate(wash_score = score
		, cat_wash = cut(wash_score
			, breaks = cats
			, include.lowest = TRUE
			, labels = c(0, 1)
#			, ordered_result = TRUE
		)
	)
)

## Update codebook
codebook <- updateCodebook("wash_score", "Composite PC score for WASH variables - New")
codebook <- updateCodebook("cat_wash", "Catogorized composite WASH variable - New")

# Some cleaning

factors <- function(x){
	factor(x, levels = c(1, 0), labels = c( "Improved", "Unimproved"))
}

working_df <- (working_df
	%>% mutate_at(c(wash_vars, "cat_wash"), funs(factors))
	%>% mutate(cat_wash_num = ifelse(cat_wash=="Improved", 1, 0))
)

codebook <- updateCodebook("expend_total_USD_per_centered", "Centered total HH expenditure - New")
codebook <- updateCodebook("cat_wash_num", "0/1 Categorized composite WASH variable - New")

save(file = "logisticpca.rda"
	, working_df
	, codebook
	, logistic_pca
	, wash_vars
)

