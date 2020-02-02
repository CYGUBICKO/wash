#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Joint binary response diagnostic plots ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Jul 16 (Tue) ----

library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(rstanarm)

theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("rstanarmModelbin.rda")

# Incoming objects:
# * rstanmodel_list - stan fits per simulation
# * rstancoef_df - fixed effect coef per simulation
# * betas_df & betas - true beta values for simulations
# * predictors  

# Clean betas_df
# Align Beta df with the estimates


betas_df <- (betas_df
   %>% mutate(coef_clean = coef
		, coef_clean = ifelse(grepl("0$", coef_clean), paste0("y", substr(n, 1, 1), "|(Intercept)")
         , ifelse(grepl("x:y[1-9]", coef_clean), paste0("y", substr(n, 1, 1), "|wealthindex")
            , ifelse(grepl("_sd", coef_clean), paste0("sd_id__y", n, "bin_Intercept")
            	, ifelse(grepl("^cor_", coef_clean), paste0("cor_id__y", substr(n, 1, 1), "bin_Intercept__y", substr(n, 2, 2), "bin_Intercept")
						, coef_clean
					)
				)
         )
      )
   )
)

print(betas_df)
rstanmodel <- rstanmodel_list[[1]]

print(plot(rstanmodel, regex_pars = "^y[1-3]\\|\\(Intercept\\)|wealthindex")
	+ geom_point(data = betas_df %>% filter(grepl("^y[1-3]\\|\\(Intercept\\)|wealthindex", coef_clean))
		, aes(x = betas, y = coef_clean), colour = "red"
	)
)
#plot(rstanmodel, "trace", regex_pars = "^y")
plot(rstanmodel, regex_pars = "^y")

quit()

summary(rstanmodel)

print(betas_df)

print(vcov(rstanmodel))

# Coefficient plots
print(stanplot(rstanmodel) 
	+ geom_point(data = betas_df, aes(x = betas, y = coef_clean), colour = "red")
)

# Zoom in
print(stanplot(rstanmodel, type = "dens") 
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
plot(rstanmodel)

# Marginal effect of predictors
plot(marginal_effects(rstanmodel, "x", resp = "y1bin"), points = TRUE, rug = FALSE)
plot(marginal_effects(rstanmodel, "x", resp = "y2bin"), points = TRUE, rug = FALSE)
plot(marginal_effects(rstanmodel, "x", resp = "y3bin"), points = TRUE, rug = FALSE)
