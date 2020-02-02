#### ---- Project: APHRC Wash Data ----
#### ---- Task: Data cleaning and preparation ----
#### ---- By: Steve and Jonathan ----
#### ---- Date: 2019 Feb 9 (Sat) ----

library(dplyr)
library(scales)
library(expss)
library(DT)
library(tibble)
library(tidyr)

load("loadData.rda")
load("globalFunctions.rda")

# In this script, we check each and every variable. Then, recode, re-label.
## Create an indicator(varname_keepcase) variable for cases to drop

#### ---- 1. Slum area -----

slumarea_tab <- (working_df
	%>% propFunc("slumarea", coltotal = TRUE)
	%>% datatable(caption = extractLabs("slumarea"), rownames = FALSE)
)

# Drop imputed cases
patterns <- c("^korogo", "^viwanda", "NIU|miss|don")
replacements <- c("Korogocho", "Viwandani", NA)
working_df <- (working_df
	%>% mutate(slumarea_keepcase = ifelse(grepl("korogocho|viwandani", slumarea), 1, 0))
	%>% recodeLabs("slumarea", patterns, replacements, insert = FALSE)
	%>% mutate_at("slumarea", factor)
	%>% mutate_at("hhid_anon", factor)
)

#### ---- 2. Gender -----

gender_tab <- (working_df
	%>% propFunc("gender", coltotal = TRUE)
	%>% datatable(caption = extractLabs("gender"), rownames = FALSE)
)

# Drop imputed cases
working_df <- (working_df
	%>% mutate(gender_keepcase = ifelse(grepl("female|male", gender), 1, 0))
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("^female", "^male", "NIU|miss|don")
replacements <- c("Female", "Male", NA)
working_df <- (working_df
	%>% recodeLabs("gender", patterns, replacements, insert = FALSE)
	%>% mutate_at("gender", factor)
)


#### ---- 3. Age -----

ageyears_tab <- (working_df
	%>% propFunc("ageyears", coltotal = TRUE)
	%>% datatable(caption = extractLabs("ageyears"), rownames = FALSE)
)

# Drop imputed cases
working_df <- (working_df
	%>% mutate(ageyears_keepcase = ifelse(grepl("[0-9]", ageyears), 1, 0))
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("ageyears", patterns, replacements, insert = FALSE)
	%>% mutate_at("ageyears", as.numeric)
)


#### ---- 4. Total expenditure (USSD) -----

expend_total_USD_per_tab <- (working_df
	%>% propFunc("expend_total_USD_per", coltotal = TRUE)
	%>% datatable(caption = extractLabs("expend_total_USD_per"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("expend_total_USD_per", patterns, replacements, insert = FALSE)
	%>% mutate_at("expend_total_USD_per", as.numeric)
)


#### ---- 5. Below poverty line -----

isbelowpovertyline_tab <- (working_df
	%>% propFunc("isbelowpovertyline", coltotal = TRUE)
	%>% datatable(caption = extractLabs("isbelowpovertyline"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("^no", "^yes", "NIU|miss|don")
replacements <- c("No", "Yes", NA)
working_df <- (working_df
	%>% recodeLabs("isbelowpovertyline", patterns, replacements, insert = FALSE)
	%>% mutate_at("isbelowpovertyline", factor)
	%>% mutate(isbelowpovertyline = relevel(isbelowpovertyline, ref = "No"))
)


#### ---- 6. Hunger scale -----

hhdhungerscale_tab <- (working_df
	%>% propFunc("hhdhungerscale", coltotal = TRUE)
	%>% datatable(caption = extractLabs("hhdhungerscale"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("severely", "mildly|moderately", "food secure", "NIU|miss|don")
replacements <- c("Severely insecure", "Mildly/Moderately insecure", "Secure" , NA)
working_df <- (working_df
	%>% recodeLabs("hhdhungerscale", patterns, replacements, insert = FALSE)
	%>% mutate_at("hhdhungerscale", factor)
	%>% mutate(hhdhungerscale = relevel(hhdhungerscale, ref = "Severely insecure"))
)

#### ---- 7. Ethnicity -----

ethnicity_tab <- (working_df
	%>% propFunc("ethnicity", coltotal = TRUE)
	%>% datatable(caption = extractLabs("ethnicity"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("kikuyu|meru|embu", "luhya", "luo", "kamba", "kisii", "other|^bora|^kale|^garr|^masa|^miji|^non-k|^swahi|^taita|^tavet|somali", "NIU|miss|don")
replacements <- c("Kikuyu", "Luhya", "Luo", "Kamba", "Kisii", "Other", NA)
working_df <- (working_df
	%>% recodeLabs("ethnicity", patterns, replacements, insert = FALSE)
	%>% mutate_at("ethnicity", factor)
	%>% mutate(ethnicity = relevel(ethnicity, ref = "Other"))
)

#### ---- 8. Number of people living in the structure -----

numpeople_thisstructure_tab <- (working_df
	%>% propFunc("numpeople_thisstructure", coltotal = TRUE)
	%>% datatable(caption = extractLabs("numpeople_thisstructure"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("numpeople_thisstructure", patterns, replacements, insert = FALSE)
	%>% mutate_at("numpeople_thisstructure", as.numeric)
)

#### ---- 9. Total number of people -----

numpeople_total_tab <- (working_df
	%>% propFunc("numpeople_total", coltotal = TRUE)
	%>% datatable(caption = extractLabs("numpeople_total,"), rownames = FALSE)
)

## Convert yes and no to 1 and 0, and NA otherwise

patterns <- c("NIU|miss|don")
replacements <- c(NA)
working_df <- (working_df
	%>% recodeLabs("numpeople_total", patterns, replacements, insert = FALSE)
	%>% mutate_at("numpeople_total", as.numeric)
)


#### ---- Wealth quintile ----

patterns <- c("^lowe", "^seco", "^midd", "^four", "^highe", "NIU|miss|don")
replacements <- c("Lowest", "Second", "Middle", "Fourth", "Highest", NA)
working_df <- (working_df
	%>% recodeLabs("wealthquintile", patterns, replacements, insert = FALSE)
	%>% mutate_at("wealthquintile", factor)
	%>% mutate(wealthquintile = relevel(wealthquintile, ref = "Lowest"))
)
#### ---- Missingness -----

## Proportion per variable merged with codebook
miss_prop_df <- (working_df
   %>% missPropFunc()
   %>% left_join(codebook, by = "variable")
   %>% select(variable, description, miss_count, miss_prop)
   %>% arrange(desc(miss_prop))
)

## Formated output
miss_prop_df_tab <- datatable(miss_prop_df, caption = "Missingness per variable")

## Drop variables with no data
miss_vars <- (miss_prop_df
  %>% filter(miss_prop==100)
  %>% select(variable)
)

vars_droped <- pull(miss_vars, variable)
no_vars_droped <- length(vars_droped)

working_df <- (working_df
  %>% select(-c(vars_droped))
)

#### ---- Cases to completely drop ----

indicators <- grep("_keepcase", colnames(working_df), value = TRUE)
working_df <- (working_df
	%>% mutate(dropcase = apply(working_df[, indicators], 1, function(x){any(x==0)}))
	%>% select(-c(indicators))
)

#### ---- Save ------

grouped_vars <- sapply(grep("_var$", ls(), value = TRUE), get)

save(file = "cleaning.rda"
	, working_df
	, codebook
	, grouped_vars
	, miss_prop_df_tab
	, no_vars_droped
)

