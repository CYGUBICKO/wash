library(splines)

library(dplyr)
library(tidyr)

library(ggplot2)
theme_set(theme_bw() + theme(panel.spacing=grid::unit(0,"lines")))

n <- 1e5
p_0 <- 0.5
beta_x <- 1
beta_z <- 1e-0
seed <- 403

set.seed(seed)
beta_0 <- qlogis(p_0)
print(beta_0)

x <- rnorm(n)
z <- rnorm(n)

ran <- seq(-3, 3, length.out=201)
pfun <- function(beta_0, beta_x, beta_z, type = "link"){
	o <- beta_0 + beta_x*x + beta_z*z
	res <- rbinom(n, size=1, prob=plogis(o))
	smod <- glm(res ~ ns(x, 4), family="binomial")
	return(predict(smod
		, newdat=data.frame(x=ran)
		, type = type
	))
}

beta_z <- seq(1,5)
plst <- list()
for (b in beta_z){
	name <- paste0("beta_z", b)
	plst[[name]] <- pfun(beta_0, beta_x, b)
}

pplot_logit <- (data.frame(ran, plst)
	%>% gather(Beta_z, Value, -ran)
	%>% mutate(Beta_z = gsub("\\D", "", Beta_z))
	%>% ggplot(aes(x = ran, y = Value, group = Beta_z, colour = Beta_z))
		+ geom_line()
		+ scale_color_manual(values = beta_z)
		+ labs(x = "x"
			, y = paste0("Predicted probabilities (Logit scale)")
		)
	
)


plst <- list()
for (b in beta_z){
	name <- paste0("beta_z", b)
	plst[[name]] <- pfun(beta_0, beta_x, b, type = "response")
}

pplot_prob <- (data.frame(ran, plst)
	%>% gather(Beta_z, Value, -ran)
	%>% mutate(Beta_z = gsub("\\D", "", Beta_z))
	%>% ggplot(aes(x = ran, y = Value, group = Beta_z, colour = Beta_z))
		+ geom_line()
		+ scale_color_manual(values = beta_z)
		+ labs(x = "x"
			, y = paste0("Predicted probabilities")
		)
)

print(pplot_logit)
print(pplot_prob)
save(file = "binary_random.rda"
	, pplot_logit
	, pplot_prob
)
