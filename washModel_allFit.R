#### ---- Project: APHRC Wash Data ----
#### ---- Task: Modeling real data ----
#### ---- Tidy Model estimates ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Dec 24 (Tue) ----

library(dplyr)
library(tidyr)
library(tibble)

library(splines)
library(lme4)

load("washModelfit_pglmerS.rda")

mod <- pglmer_scaled
glmer_allfit <- allFit(mod, parallel = "multicore", ncpus = parallel::detectCores())
summary(glmer_allfit)

save(file = "washModel_allFit.rda"
	, glmer_allfit
)
