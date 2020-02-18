# Two step long format restructure
longDFunc <- function(df){
	temp_df1 <- (df
		%>% gather(services, status, c("watersource", "toilettype", "garbagedposal"))
	)

	temp_df2 <- (df
		%>% select(hhid, year, watersourceP, toilettypeP, garbagedposalP)
		%>% gather(serviceP, statusP, c("watersourceP", "toilettypeP", "garbagedposalP"))
		%>% mutate_at("serviceP", function(x)gsub("P", "", x))
	)
	
	long_df <- (temp_df1
		%>% full_join(temp_df2, by = c("hhid", "year", c(services = "serviceP")))	
		%>% data.frame()
	)
	return(list(year1_df = temp_df1, prev_df = long_df))
}

save.image("longDFunc.rda")


