#### ---- Project: APHRC Wash Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 9 (Sat) ----

library(dplyr)

load("cleaning.rda")
load("globalFunctions.rda")

#### ---- 1. Complete cases ------
# Cases were droped based on indicator variable (dropcase) which was generated from. 
# * gender - No/Missing gender?
# * ageyears - Missing age?

complete_df <- (working_df
	%>% filter(dropcase == 0)
)
incomplete_df <- (working_df
	%>% filter(dropcase == 1)
)

codebook <- updateCodebook("dropcase", "Cases to drop (TRUE - Drop) - New")

## Don't have two names for the same thing!
working_df <- complete_df

summary(working_df)

## Change this to save
save.image("complete.rda")

# Save codebook to csv
write.csv(codebook, "wash_codebook.csv")
