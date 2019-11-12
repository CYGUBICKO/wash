## We want to simulate an ar(1) random effect
## This means that the mean is zero, and we have some autocorrelation ρ

## My only question (JD) is do we need to worry about starting point,
## or does the asymptotic distribution already match the error distribution?
## The only issue is in fact the sd, and the sd did need to be adjusted

numYears <- 1e3
phi <- 0.9
sdSim <- 1
sdAsymp <- sdSim/sqrt(1-phi^2)

x0 <- rnorm(1, 0, sdAsymp)

eps <- rnorm(numYears, 0, sdSim)
x <- numeric(numYears)

for (y in 1:numYears){
	xprev <- ifelse(y==1, x0, x[[y-1]])
	x[[y]] <- phi*xprev + eps[[y]]
}

plot(1:numYears, x, type="l")

print(sd(eps))
print(sd(x))
print(sdAsymp)
cor.test(x[-1], x[-numYears])
