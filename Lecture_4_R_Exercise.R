library(GGally)

# Read in the csv file
laborm <- read.csv('http://nb.vse.cz/~zouharj/econ/labour_market.csv')

# Check the data types
head(laborm)

# Make the ggpairs plot
ggpairs(laborm)

# ggpairs doesn't recognize the dummy variables such as fulltime, married, female, metro and black
# This is not nice because 0 and 1 are categories instead of numeric values

# We have to manually make ggpairs understand category variables
# The factor() function does this
laborm$fulltime <- factor(laborm$fulltime)

# Plot a histogram
hist(laborm$wage, main = 'Labor Wage Histogram', xlab = 'Wage', ylab = 'Frequency')

# Regress wage on education and store in model1 
model1 <- lm(wage ~ educ, data = laborm)
summary(model1)
# Scatterplot of wage against education
plot(wage ~ educ, data = laborm, main = 'Scatter Plot', xlab = 'Education', ylab = 'Wage')
# abline(model1) gives the regression line to the existing plot
abline(model1)

# We wanna run the log regression
model2 <- lm(log(wage) ~ educ, data=laborm)
plot(log(wage) ~ educ, data = laborm, main = 'Log Regression Point Plot')
abline(model2)
# It is a lot easier to plot it with the following codes by simply copying and adjust
ggplot(data=laborm, aes(x=educ, y=log(wage), colour=factor(female))) + 
  geom_point() + 
  geom_smooth(method=lm) +
  facet_wrap( ~ fulltime)

# Let's get back to regression seriously
# If you use a period sign in regression, it means to
# regress everything against wage, so does it work for
# log(wage) ~ .
model3 <- lm(log(wage) ~ . , data = laborm)
model3
# Description: with all other variables held constant, with 
# 1 year more education, the wage will increase by approximately
# 9%

# Description: Fulltime workers earn approximately 26% more than the partime worker
# R creates reference variable name for category variable fulltime, so fulltime1 means
# those people who are fulltime

# If you write down this way, by applying exponential function to all coefficients
# to obtain the "exact" % changes:
exp(model3$coefficients)
# So one unit increase in exper will increase wage by 1.095. 1 more year of education
# will increase wage by roughly 9.5%

# Or we subtract by 1 and multiply by 100
100*(exp(model3$coefficients)-1)
# This takes everything in percentage without the % sign

# summary( ) gives the summary of the regression analysis
summary(model3)

# Standard error and confidence intervals, we want to obtain confidence intervals
# Obtain 95% confidence interval for regression parameters
confint(model3)

# Coefficient + 2* Standard Error -> Upper Bound of 95%
# Coefficient - 2* Standard Error -> Lower Bound of 95%
# Manually calculation with educ coefficient and educ standard error
0.091001 + 2*0.005965
0.091001 - 2*0.005965
# The result roughly matches the result of the confidence interval

# If we wanna generate a "professonal look" table for a research paper
# We can install the "stargazer" package  and use the stargazer()
# function to compare the results produced by different models
# But we have to emphasize that type="text" to generate a nice table

install.packages('stargazer')
library(stargazer)
stargazer(model1, model2, model3, type="text")

# We can also use the ggplot library, which is fantastic
library(ggplot2)
# aes stands for aesthetics. Use geom_point() plot points
ggplot(data=laborm, aes(x=educ, y=wage)) + geom_point()

# If you have over 80 points, it is too long, so we wanna short it
# geom_smooth() gives a smooth line, shaded area is the confidence interval
ggplot(data=laborm, aes(x=educ, y=wage)) + 
  geom_point() + 
  geom_smooth()

# Or we use the fitted line, we use method=lm in smooth()
ggplot(data=laborm, aes(x=educ, y=wage)) + 
  geom_point() + 
  geom_smooth(method=lm)

# We can also ask for different colors for variable fulltime
ggplot(data=laborm, aes(x=educ, y=wage, colour=fulltime)) + 
  geom_point() + 
  geom_smooth(method=lm)

# If we want to show colors for female, we haven't factor female variable to recognize it
# as category variable. So we need to factorize it either like we did for fulltime, or directly
# gg plot like the following code
ggplot(data=laborm, aes(x=educ, y=wage, colour=factor(female))) + 
  geom_point() + 
  geom_smooth(method=lm)

# If we need multiple variables for multiple plots, we use facet_wrap in ggplot
# but here only with ~fulltime
ggplot(data=laborm, aes(x=educ, y=wage, colour=factor(female))) + 
  geom_point() + 
  geom_smooth(method=lm) +
  facet_wrap( ~ fulltime)
# This generates two graphs containing both genders in two separated graphs regarding part-time and full-time



