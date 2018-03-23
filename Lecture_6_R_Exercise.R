skoda <- read.csv("http://nb.vse.cz/~zouharj/econ/skoda.csv")

# To fix the ugly numbers in the summary, do it once for all for this session
# . Scientific notation scipen
options(scipen = 5)

# change unit under km, but if we do so, we cannot use the priod to regress on everything else
skoda$km <- skoda$km/10000

# Run a regression of log(price) on everything else and test whether 
# fuel type affects the price. Regress on everything else use period . to do so

# First run the regression
model1 <- lm(log(price) ~ ., data = skoda)
summary(model1)
# Omitted variable fuel: autogas
# If we want to understand does Fuel matter?
# We pick the H0: Beta-fuel|diesel| = 0, Beta-fuel|petrto| = 0
# So we need to test joint significance of Fuel Dummies (F test)

# Use base R without loading package to do F test
# Next, run the "restrictted" regression - without fuel
model1restricted <- lm(log(price) ~ km + year + combi + model, data = skoda)
# We run anova of model1 and model1restricted
# ANOVA implements an F-test
anova(model1, model1restricted)
# Neither of the two fuel variables are significant in model1 by usual standards on its own right
# In ANOVA, p < 0.05, so we reject the null hypothesis and conclude that "Fuel Matters"

# A different way to carry out the test
# Same test for joint significance
# We have to upload a new package called "car"
# install.packages("car")
library("car")

# This package has the following command
# linearyHypothesis(model, c("variables you want to omit-the dummies"))
linearHypothesis(model1, c("fueldiesel", "fuelpetrol"))
# The result states the null hypothesis

# In addition, let us test another type of a linear hypothesis:
# H0: Beta-year = Beta-combi
linearHypothesis(model1, "year = combi")
# NOTE: we cannot write c("year", "combi") here because this is testing:
# H0: Beta-year = 0, Beta-combi = 0. This is a totally different null hypothesis

# In addition, we can also do the followings with the linearHypothesis(which actually makes no sense)
# H0: Beta-year = Beta-combi, 2Beta-year + Beta-km = 5
linearHypothesis(model1, c("year = combi", "2*year + km = 5"))
# Again, this is only a demonstration of the powefulness of this function, but it makes no sense


# Change the reference fuel category from "autogas" to "petrol"
skoda$fuel <- relevel(skoda$fuel, ref="petrol")
# it becomes the first category to be eliminated if there are dummy variables
model2 <- lm(log(price) ~ ., data = skoda)
summary(model2)
# In the last two rows, fueldiesel is highly significant
# In the first model, model1, we would like to conlcude fuel is not important
# But here, we see one significant result from fuel, so we might conclude important
# So we see, that we could manupilate the categorical variables to get different result
# This is something we don't like
linearHypothesis(model2, c("fueldiesel", "fuelautogas")) 
# run the test for joint significance
# p value exactly as in the linearHypothese test in model1
# Change of category, you can manipulate the results of the regression
# But the change of category doesn't influence the result of test for joint significance
# This can be proved using the Fisher's Model

# Now we use the stargazer package to produce a table for both of these regressions
library(stargazer)
stargazer(model1, model2, type ="text")

# To deal with Heteroskedasticity, we need two more packages
# install.packages("lmtest") # Tests of Homoskedasticity
# install.packages("sandwich") # Robust std. error
library(lmtest)
library(sandwich)
# Test of Homoskedasticity in model1 (caution: the test is weak!)
# Breusch-Pagan test
bptest(model1)
# Remember the null hypothesis is Homoskedasticity: H0: Homoskedasticity
# The result p-value is small, so we reject Homoskedasticity
# All the tests we did were not well, so all the previous hypothesis tests are invalid

# So what should we do?
# Robust standard errors will be based on: Heteroskedasticity consistent
vcov_robust <- vcovHC(model1)
stdErr_robust <- sqrt(diag(vcov_robust))
stdErr_robust
