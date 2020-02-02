#### ---- Project: APHRC Wash Data ----
#### ---- Task: Descriptives ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 10 (Sun) ----

library(tidyr)
library(dplyr)
library(expss)
library(ggplot2)
library(scales)

load("globalFunctions.rda")
load("logisticpca.rda")

# Functions to generate basic plots for descriptive stats (counts and proportions)
source("../funs/descriptivePlots.R")

theme_set(theme_bw()+
theme(panel.spacing=grid::unit(0,"lines")))

#### ---- 1. Water sources ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhwatersource")
legend_title <- "Slum area"
color_var <- "slumarea"
filter_rule <- quo(cat_hhwatersource == "Improved")
y_limits <- c(0, 1)

water_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
water_plot <- (water_plot[["prop_plot"]] 
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Water"
	)
	+ theme(legend.position = "right")
)
water_plot
ggsave("water_plot.pdf", water_plot)

# Check with JD
propFunc(working_df, tab_vars)

tab_vars2 <- c("intvwyear", "cat_hhwatersource", "slumarea")
propFunc(working_df, tab_vars2)

##### ---- 2. Toilet type ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhtoilettype")
filter_rule <- quo(cat_hhtoilettype == "Improved")
y_limits <- c(0, 0.3)

toilet_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
toilet_plot <- (toilet_plot[["prop_plot"]] 
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Toilet types"
	)
	+ theme(legend.position = "right")
)
toilet_plot
ggsave("toilet_plot.pdf", toilet_plot)

##### ---- 3. Garbage disposal ----

tab_vars <- c("intvwyear", "slumarea", "cat_hhgarbagedisposal")
filter_rule <- quo(cat_hhgarbagedisposal == "Improved")
y_limits <- c(0, 1)

garbage_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
garbage_plot <- (garbage_plot[["prop_plot"]] 
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Garbage disposal"
	)
	+ theme(legend.position = "right")
)
garbage_plot
ggsave("garbage_plot.pdf", garbage_plot)

#### ---- 4. Logistic PCA Wash Variable ----

tab_vars <- c("intvwyear", "slumarea", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")

cat_wash_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
cat_wash_plot <- (cat_wash_plot[["prop_plot"]] 
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "Overall WASH indicator from Logistic PCA"
	)
	+ theme(legend.position = "right")
)
cat_wash_plot


#### ---- 5. Access rate (improved/all items) ----
# This is the proportion of improved WASH indicators per case

prop_wash_plot <- (ggplot(working_df, aes(wash_access_rate, colour = slumarea))
	+ geom_density()
	+ scale_colour_brewer(palette = "Dark2")
	+ facet_wrap(~intvwyear)
	+ labs(title = "Proportion of WASH accessed"
		, x = "Proportion of WASH"
		, color = "Slum"
	)
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
prop_wash_plot

#### ---- 6. Overall WASH indicator and socio-demo ----


#### ---- 6.1 Gender ----

tab_vars <- c("intvwyear", "slumarea", "gender", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")
color_var <- "gender"
legend_title <- "Gender"
y_limits <- c(0.1, 0.85)

wash_gender_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
wash_gender_plot <- (wash_gender_plot[["prop_plot"]] 
	+ facet_wrap(~slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and gender"
	)
	+ theme(legend.position = "bottom")
)
wash_gender_plot

#### ---- 6.2 Age ----

xvar <- "intvwyear"
yvar <- "ageyears"
colvar <- "slumarea"
wash_age_plot <- (working_df
	%>% filter(cat_wash=="Improved")
	%>% meanPlot(xvar, yvar, color_var)
)
wash_age_plot <- (wash_age_plot[["mean_plot"]]
	+ labs(x = "Year"
		, title = "WASH and age"
		, color = "Slum area"
	)
	+ theme(legend.position = "bottom")
)
wash_age_plot

#### ---- 6.3 Ethnicity ----

tab_vars <- c("intvwyear", "slumarea", "ethnicity", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")
color_var <- "ethnicity"
legend_title <- "Ethnicity"
y_limits <- c(0, 1)

wash_ethnicity_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
wash_ethnicity_plot <- (wash_ethnicity_plot[["prop_plot"]] 
	+ facet_grid(~slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and ethnicity"
	)
	+ theme(legend.position = "bottom")
)
wash_ethnicity_plot

##### ---- 6.4 Total number of people in the HH ----

numpeople_totalplot <- (ggplot(working_df %>% filter(cat_wash=="Improved"), aes(numpeople_total, colour = slumarea))
	+ geom_density()
	+ scale_colour_brewer(palette = "Dark2")
	+ facet_wrap(~intvwyear)
	+ labs(title = "WASH and HH size"
		, x = "HH size"
		, color = "Slum area"
	)
	+ theme(axis.text.x=element_text(angle=90))
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
numpeople_totalplot


#### ---- 6.5 Wealth index ----

tab_vars <- c("intvwyear", "slumarea", "wealthquintile", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")
color_var <- "wealthquintile"
legend_title <- "Wealth quintile"
y_limits <- c(0, 1)

wash_wealthquintile_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)

cols <- c("Lowest" = "red4"
	, "Second" = "tomato1"
	, "Middle" = "grey"
  	, "Fourth" = "springgreen1"
   , "Highest" = "springgreen4"
)
wash_wealthquintile_plot <- (wash_wealthquintile_plot[["prop_plot"]] 
	+ facet_grid(~slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_manual(values = cols
		, breaks = c("Lowest", "Second", "Middle", "Fourth", "Highest")
		, labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
	)
	+ labs(x = "Years"
		, title = "WASH and wealth"
	)
	+ theme(legend.position = "bottom")
)
wash_wealthquintile_plot


#### ---- 6.6 Poverty line ----

tab_vars <- c("intvwyear", "slumarea", "isbelowpovertyline", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")
color_var <- "isbelowpovertyline"
legend_title <- "Below poverty line"
y_limits <- c(0, 1)
wash_isbelowpovertyline_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
wash_isbelowpovertyline_plot <- (wash_isbelowpovertyline_plot[["prop_plot"]] 
	+ facet_grid( ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and Poverty line"
	)
	+ theme(legend.position = "bottom")
)
wash_isbelowpovertyline_plot

#### ---- 6.7 Hunger scale ----

tab_vars <- c("intvwyear", "slumarea", "hhdhungerscale", "cat_wash")
filter_rule <- quo(cat_wash == "Improved")
color_var <- "hhdhungerscale"
legend_title <- "Hunger scale"
y_limits <- c(0, 1)
wash_hhdhungerscale_plot <- (working_df
	%>% propPlot(tab_vars
		, color_var
		, legend_title
		, y_limits
		, filter_rule
	)
)
wash_hhdhungerscale_plot <- (wash_hhdhungerscale_plot[["prop_plot"]] 
	+ facet_grid( ~ slumarea)
	+ theme(axis.text.x=element_text(angle=90))
	+ scale_colour_brewer(palette="Dark2")
	+ labs(x = "Years"
		, title = "WASH and hunger scale"
	)
	+ theme(legend.position = "bottom")
)
wash_hhdhungerscale_plot

#### ---- 6.8 Household Expenditure ----

expend_total_USD_per_centered_plot <- (ggplot(working_df, aes(x = expend_total_USD_per_centered, y = cat_wash_num, colour = slumarea))
	+ stat_sum(alpha = 0.25, na.rm = TRUE)
	+ facet_wrap(~intvwyear)
	+ geom_smooth(aes(lty = slumarea)
		, method = "gam"
		, method.args = list(family = "binomial")
		, formula = y~s(x, k = 20)
		, alpha = 0.1
		, na.rm = TRUE
	)
	+ labs(title = "WASH and HH Expenditure"
		, x = "Centered Expenditure (USSD)"
		, y = "WASH Indicator"
	)
	+ theme(plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
	)
)
print(expend_total_USD_per_centered_plot)

descriptive_saved_plots <- sapply(grep("_plot$", ls(), value = TRUE), get)

save(file = "descriptives.rda"
#	, working_df
	, codebook
	, descriptive_saved_plots
)
