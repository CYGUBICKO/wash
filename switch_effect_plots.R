library(ggplot2)
theme_set(theme_bw() +
	theme(panel.spacing=grid::unit(0,"lines")))

library(ggpubr)
library(dplyr)
library(tidyr)
library(scales)
library(DT)

source("../funs/ggplot_theme.R")

load("switchSummary.rda")
load("switchPredEffects.rda")
load("switchTidy.rda")

### Reformat betas_df 
dd <- (betas_df 
	%>% mutate(parameter = ifelse(grepl("\\_M", coefs2), "Income" 
		, ifelse(grepl("gain", coefs2), "b_gain (Baseline)",  
			ifelse(grepl("add", coefs2), "b_add (Advantage)", "b_lose")) 
		) 
		, term = gsub("\\_.*", "", coefs2) 
	) 
	%>% filter(term=="y1" & !grepl("lose", parameter)) 
) 
estimates_df <- (extract_coefs_y1 
	%>% filter(effect == "fixed") 
) 
parameters <- pull(estimates_df, parameter) %>% unique() 
estimates_df <- (estimates_df 
%>% mutate(parameter = factor(parameter, levels = parameters, labels = parameters))) 

pos <- ggstance::position_dodgev(height=0.5) 

# print(ggplot(estimates_df, aes(x = estimate, y = parameter)) 
# 	+ geom_point(position = pos) 
# 	+ ggstance::geom_linerangeh(aes(xmin = conf.low, xmax = conf.high), position = pos) 
# 	+ geom_point(data = dd, aes(x=betas, y = parameter), colour = "red") 
# 	+ labs(x = "Estimate" 
# 		, y = "" 
# 	) 
# 	#	+ facet_wrap(~parameter, scale = "free", ncol = 1) 
# 	#	+ facet_theme 
# 	+ theme(legend.position = "bottom") 
# ) 

### Reformat betas_df 
betas_df2 <- (betas_df 
	%>% mutate(parameter = ifelse(grepl("\\_M", coefs2), "Income" 
		, ifelse(grepl("gain", coefs2), "b_gain (Baseline)",  
			ifelse(grepl("add", coefs2), "b_add (Advantage)", "b_lose")) 
		) 
		, term = gsub("\\_.*", "", coefs2) 
	) 
	%>% filter(!grepl("lose", parameter)) 
) 


estimates_df <- (extract_coefs_df 
	%>% filter(effect == "fixed")
	%>% mutate(parameter = ifelse(grepl("xm", parameter), "Income" 
		, ifelse(grepl("gain", parameter), "b_gain (Baseline)",  
			ifelse(grepl("add", parameter), "b_add (Advantage)", "b_lose")) 
		) 
	) 
	%>% data.frame()
)

parameters <- pull(estimates_df, parameter) %>% unique() 
estimates_df <- (estimates_df 
	%>% mutate(parameter = factor(parameter, levels = parameters, labels = parameters)) 
) 

pos <- ggstance::position_dodgev(height=0.5) 

print(ggplot(estimates_df, aes(x = estimate, y = term, colour = "Fitted")) 
	+ geom_point(position = pos) 
	+ ggstance::geom_linerangeh(aes(xmin = conf.low, xmax = conf.high), position = pos) 
	+ geom_point(data = betas_df2, aes(x=betas, y = term, colour = "True")) 
	+ labs(x = "" , y = "")
	+ scale_colour_manual(name = ""
		, values =c("#808080", "#E41A1C")
		, guide = guide_legend(reverse = TRUE)
   )
	+ facet_wrap(~parameter, scale = "free", ncol = 1) 
	+ facet_theme 
	+ theme(legend.position = "bottom") 
) 
