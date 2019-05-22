#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- Fit GLM to recapture betas ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Mar 16 (Sat) ----

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

load("globalFunctions.rda")
load("simulateResponse.rda")

set.seed(7902)

# Objects in
# * sim_dflist
# & betas_df
# * betas
# * predictors

#response <- "service1"
services <- c("service1", "service2", "service3")
nsims <- length(sim_dflist)
model_form <- as.formula(status ~ 0 + wealthindex:service + service)
print(model_form)

coef_list <- list()
glm_list <- list()

for (s in 1:nsims){
   long_df <- (sim_dflist[[s]]
      %>% select(c("hhid_anon", predictors, services))
      %>% gather(service, status, services)
   )
	glm_model <- glm(model_form, data = long_df, family = "binomial")
	coef_list[[s]] <- coef(glm_model)
	glm_list[[s]] <- glm_model
}

coef_df <- Reduce(rbind, coef_list) %>% as_tibble()
summary(coef_df)
print(coef_df)

glm_beta_plot <- (coef_df
	%>% gather(coef, value)
	%>% ggplot(aes(x = value))
	+ geom_histogram()
  	+ geom_vline(data = betas_df, aes(xintercept = betas, color = coef)
     	, linetype="dashed"
  	)
  	+ labs(x = "Betas", y = "Count")
  	+ ggtitle("Fitted vs True betas")
  	+ guides(colour = FALSE)
  	+ theme(plot.title = element_text(hjust = 0.5))
	+ facet_wrap(~coef, scales = "free")
)
print(glm_beta_plot)

save(file = "simpleGlm.rda"
	, glm_beta_plot
	, betas
)
