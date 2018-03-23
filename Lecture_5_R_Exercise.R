# Hypothesis testing part

# t-Test
# Continuation of Week 4
# Import data and see they are OK.
dt <- read.csv("http://nb.vse.cz/~zouharj/econ/labour_market.csv")
head(dt)

# Data eyeballing.
# install.packages("GGally")
library(GGally)
ggpairs(dt)  # Wait, 0s and 1s are categories, not numbers!
dt$fulltime <- factor(dt$fulltime)

hist(dt$wage)  # Basic histogram.
m1 <- lm(wage ~ educ, data=dt)  # Regress wage on education, save results to m1.
plot(wage ~ educ, data=dt)  # Basic scatterplot.
abline(m1)  # Add a regression line to the existing plot.

m2 <- lm(log(wage) ~ educ, data=dt)
plot(log(wage) ~ educ, data=dt)
abline(m2)

# Fantastic plot using the ggplot2 package.
library(ggplot2)
ggplot(data=dt, aes(x=educ, y=log(wage), colour=factor(female))) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap( ~ fulltime)

# OK, let's do some serious regression.
m3 <- lm(log(wage) ~ . , data=dt)  # "." means "all other variables"
exp(m3$coefficients)  # Exponentiate coefficients to obtain EXACT % changes.
100*(exp(m3$coefficients) - 1)  # Multiplicative factors are turned % changes.
summary(m3)  # Detailed results
confint(m3)  # Obtain 95% confidence intervals for regression paramaters.
# Manual calculation for the education coefficient
0.091001 + 1.96*0.005965  # Bounds of 95% CI: Estimate +- 2 x Std. Error
0.091001 - 1.96*0.005965

install.packages("stargazer")
library(stargazer)
stargazer(m1, m2, m3, type="text")
# This generates two graphs containing both genders in two separated graphs regarding part-time and full-time

# Week 5's part
# Hypothesis testing
# Does education affect wage at all?
# H0: Beta-edu = 0 ( In the population, there's no effect)
# H1: Beta-edu != 0 OR (one-sided hypothesis such as Beta-edu > 0 or Beta-edu <0)
?pt
qt(0.975, 992) # 97.5%th percentile of t(992)
# Since it is symetric, both tails are -1.96 and 1.96
# We want to know if this number is within the rejection region
# t = 15.26, which is way in the rejection region
# We reject the null hypothesis at a 5% level

# Confidence intervals for Hypothesis testing (ONLY FOR TWO-TAILED TESTS)
# Rule to remember is: reject if the hypothesized value is outside the 
# 95% confidence interval, for the given parameter.
# 95% confidence interval for the effect of education no wages: Beta-edu
# has the following endpoints:
# Estimated +- C*Std Error
# c is 97.5th percentile of t-(n-k-1)
confint(m3)
# So the confidence interval is shown below:
# 95% Confidence interval is[0.079, 0.103] does not contain 0, which is the hypothesized value
# So we reject H0

# The last approach is the p-value appraoch
# p < alpha -> reject H0
