library(dplyr)

load("globalFunctions.rda")
load("complete.rda")

## summary(working_df)

## Do people report the same services in the same household at the same time?

## First, how many reports do we get from same time and place?
print(working_df
	%>% group_by(intvwyear, hhid_anon)
	%>% summarise(count = n())
	%>% filter(count>1)
)

## Only 1!

## Does code work? What if we group by household only?
print(working_df
	%>% group_by(hhid_anon)
	%>% summarise(years = n())
	%>% group_by(years)
	%>% summarise(hhs = n())
)

## K., so it looks like they interview one person per household/intvwyear. Who is it?

print(working_df
	%>% group_by(gender)
	%>% summarise(count = n())
)

## Usually a male. Probably reflecting HoH bias
