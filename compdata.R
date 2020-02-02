library(dplyr)

load("analysisdata.rda")

df_prop <- 0.02 # Prop of data to use
set.seed(7902)

n <- nrow(working_df)
complex <- (working_df
	%>% select(wealthindex)
	%>% filter(runif(n)<df_prop)
)

summary(complex)

save(complex, file="compdata.rda")
