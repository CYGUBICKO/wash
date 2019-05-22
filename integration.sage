# Understanding how shift in variance changes the betas

from sage.symbolic.integration.integral import definite_integral
x, sigma, mu = var('x, sigma, mu')
assume(sigma>0)
assume(mu, 'real')

# f(theta) = N(mean(theta), sigma(theta)). Take theta = x
f = 1/(sqrt(2*pi*sigma)) * exp(-(x - mu)^2/(2*sigma^2));
sol1 = integrate(f, x, -infinity, infinity);
print(sol1.simplify())

# Likelihood function, L(theta) = exp(x)/(1 + exp(-x))
l = 1/(1 + exp(-x));
sol2 = integrate(l, x, -infinity, 1);
print(sol2.simplify())

# L(theta)*f(theda)
p = integrate(l*f, x, -infinity, infinity, algorithm = "maxima");
print(p.simplify())

