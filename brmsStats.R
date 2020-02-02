#### ---- Project: APHRC Wash Data ----
#### ---- Task: Simulation ----
#### ---- MCMCMglmr summary ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Apr 03 (Wed) ----

library(brms)

load("brmsModel.rda")

# Incoming objects:
# * brms model objects

nsims <- length(brmsmodel_list)

for (s in 1:nsims){
	print(summary(brmsmodel_list[[s]]))
}

