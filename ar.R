## We want to simulate an ar(1) random effect
## This means that the mean is zero, and we have some autocorrelation ρ
## My only question (JD) is do we need to worry about starting point,
## or does the asymptotic distribution already match the error distribution?

numYears <- 100000
phi <- 0.9
sd <- 1
sdAsymp <- sd/sqrt(1-phi^2)

x0 <- rnorm(1, 0, sdAsymp)

eps <- rnorm(numYears, 0, sd)
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
