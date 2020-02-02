library(lme4)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(101)

n <- 10000
b0 <- 2
b1 <- 4
b2 <- 10

b1_sd <- 1
b2_sd <- 5
cor_ab <- 0.5

cormat <- matrix(c(1,cor_ab,cor_ab,1),2,2)
sdvec <- c(b1_sd, b2_sd)
varmat <- sdvec %*% t(sdvec)
covmat <- varmat * cormat 

print(covmat)

x <- rnorm(n)

b0vec <- rnorm(2*n,mean=b0,sd=2)

betas <- MASS::mvrnorm(n=n
	, mu = rep(c(b1, b2), each=1)
	, Sigma = covmat
)

y1 <- head(b0vec,n) + betas[,1]*x
y2 <- tail(b0vec,n) + betas[,2]*x 

dat <- data.frame(y1, y2, X=x, id=1:n)

mdat <- (dat
	%>% gather(key="type", value="Y", -X, -id)
	%>% mutate( p = plogis(Y)
		, service = rbinom(2*n, 1, p)
		)
)

print(ggplot(mdat, aes(y=p, x=type, color=type))
	+ geom_boxplot()
)

norm_mod <- lmer(Y~ type:X + type + (0+type|id)
	, data=mdat
	, control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore")
)

print(summary(norm_mod))

print(VarCorr(norm_mod))

print(sd(x)/sqrt(n))

quit()

mod <- glm(service ~ X 
	, data=(mdat %>% filter(type=="y1"))
	, family = binomial
)

print(summary(mod))

quit()

binom_mod <- glmer(service ~ type:X + (0+type|id)
	, data = mdat
	, family = binomial
	, control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore")
)

print(summary(binom_mod))

