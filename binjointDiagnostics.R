#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Continous outcome simulation plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 May 30 (Thu) ----

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(brms)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("binjointModel.rda")

# Incoming objects:
# * brmsmodel_list - glmer fits per simulation
# * brmscoef_df - fixed effect coef per simulation
# * betas_df & betas - initial beta values for simulations
# * predictors  

# Clean betas_df
# Align Beta df with the estimates
betas_df <- (betas_df
   %>% mutate(coef_clean = coef
		, coef_clean = ifelse(grepl("0$", coef_clean), paste0("b_y", substr(n, 1, 1), "bin_intercept")
         , ifelse(grepl("x:y[1-9]", coef_clean), paste0("b_y", substr(n, 1, 1), "bin_x")
            , ifelse(grepl("_sd", coef_clean), paste0("sd_id__y", n, "bin_Intercept")
            	, ifelse(grepl("^cor_", coef_clean), paste0("cor_id__y", substr(n, 1, 1), "bin_Intercept__y", substr(n, 2, 2), "bin_Intercept")
						, coef_clean
					)
				)
         )
      )
   )
)
brmsmodel <- brmsmodel_list[[1]]

summary(brmsmodel)

print(betas_df)

print(vcov(brmsmodel))

# Coefficient plots
print(stanplot(brmsmodel) 
	+ geom_point(data = betas_df, aes(x = betas, y = coef_clean), colour = "red")
)

# Zoom in
print(stanplot(brmsmodel, type = "dens") 
	+ geom_vline(data = betas_df 
		%>% setnames(c("coef_clean", "betas"), c("Parameter", "Value"))
		, aes(xintercept = Value)
		, linetype = "dashed"
		, colour = "red"
	)
   + facet_wrap(~Parameter, scales = "free", ncol = 3)
	+ theme(strip.text.x = element_text(size = 6))
)

# Trace plots
plot(brmsmodel)

# Marginal effect of predictors
plot(marginal_effects(brmsmodel, "x", resp = "y1bin"), points = TRUE, rug = FALSE)
plot(marginal_effects(brmsmodel, "x", resp = "y2bin"), points = TRUE, rug = FALSE)
plot(marginal_effects(brmsmodel, "x", resp = "y3bin"), points = TRUE, rug = FALSE)
