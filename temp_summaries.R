library(brms)
load("brmsModelbinAR1.rda")


ar1_model <- brmsmodel_list[[1]]

summary(ar1_model)

save(file = "temp_summaries.rda")


