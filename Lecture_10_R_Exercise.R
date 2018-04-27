# download the data set and check the data
dur <- read.csv("http://nb.vse.cz/~zouharj/econ/durgoods.csv")
head(dur) # Period: 1978Q1 - 1985Q3 (not given in the data file)

# With time series, the first thing is to plot them
# type b is both points and lines
plot(dur$dish, type = "b")
# check fridges are seasonal
plot(dur$frig, type = "b") # there is seasonality in the fridges

# Let's create the year variable
dur$year <- seq(from=1978, to=1985.75, by=0.25)
plot(dur$frig ~ dur$year, type = "b") # plot and check

# Now for seasonality
dur$quarter <- 1:4 # 1:4 repeats this sequence in the data frame

# Are dishwasher sales seasonal?
model1 <- lm(dish ~ quarter, data = dur)
summary(model1)
# this doesn't give us the result, because the quarter result shows
# in numerical value int, NOT dummy variable
plot(dur$dish, type = "b")
lines(model1$fitted.values)

# So we need to factor the variable quarter
dur$quarter <- factor(1:4)
# Now, it is a factor variable 
# Now we re-run the variable
model1 <- lm(dish ~ quarter, data = dur)
summary(model1)
# Last line: F-statistic: 0.2098 on 3 and 28 DF,  p-value: 0.8888
# This is the test for joint significance of the quarterly dummies
# Therefore, we conclude that seasonality is not significant, because
# the p-value is very large

plot(dur$dish, type = "b")
lines(model1$fitted.values)
# The result shows that now, we have the dummies for quarter from 1-4
# Because of multicollinearity, the regression result only shows 3 variables

# Now for fridges:
model2 <- lm(frig ~ quarter, data = dur)
summary(model2)
plot(dur$frig, type = "b")
lines(model2$fitted.values)

# Let's add (non-linear) trend
model3 <- lm(frig ~ quarter + year + I(year^2), data = dur) # if not creating new variable, use I() function to square
summary(model3)
plot(dur$frig, type = "b", xlab = "Quarter", ylab = "Sales of Fridges", main = "Seasonality of Fridges Sales")
lines(model3$fitted.values)

# Sometimes, we can download the detrend/seasonal adjusted data
# but how do we do this in the data
# If ever, you are asked to create series adjusted for seasonality and a quadratic trend
# this is simply the residuals
dur$frig_adjusted <- model3$residuals
plot(dur$frig_adjusted, type = "b")
# We then add the mean value of the original time series
dur$frig_adjusted <- model3$residuals + mean(dur$frig) # mean of the residuals is expected to be 0
plot(dur$frig_adjusted, type = "b")
lines(dur$frig) # add original line, and compare

# Is seasonality significant in model3?
# We need to run an F-test for quarter 2, quarter 3, and quarter 4 only
# The F-test we see in the summary of regression is the regression for all 4 quarters
# Here we onnly use 2,3 and 4
anova(model3, lm(frig ~ year + I(year^2), data = dur))

# Multiplicative seasonality model (before we had additive)
model4 <- lm(log(frig) ~ quarter, data = dur)
summary(model4)

# Result: quarter 0.18141, sales increasing in quarter 2, compared to quarter 1, grows by approximately 18%
# To get the accurate result, exp(quarter2 coefficient)-1
exp(model4$coefficients) # To exponentiate and get the factors of multiplication


# SEASONALITY IS DONE
###################################################################################################
# Lagged Variables
# Using fertility rates
dt <- read.csv("http://nb.vse.cz/~zouharj/econ/fertil3.csv")
desc <- read.csv("http://nb.vse.cz/~zouharj/econ/fertil3_desc.csv")
View(desc) # to view the description file
# take a look at the data
head(dt[,c("year","pe","pe_1","pe_2","pe_3")], n=10)
tail(dt[,c("year","pe","pe_1","pe_2","pe_3")], n=10)
plot(dt$ww2 ~ dt$year, type = "b", xlab="Year", ylab="WW2")

# Let's gradually add variables and see what happens
# gfr - general fertility rate
model5 <- lm(gfr ~ year, data = dt)
summary(model5)
plot(dt$gfr, type = "b", xlab = "Year", ylab = "General Fertility Rate", main = "Model 5 Fertility on Years")
lines(model5$fitted.values)

# Now we include ww2 and pill variables
model6 <- lm(gfr ~ year + ww2 + pill, data=dt)
summary(model6)
plot(dt$gfr, type = "b", xlab = "Year", ylab = "General Fertility Rate", main = "Model 6 Fertility on Years")
lines(model6$fitted.values)

# Finite distributed lag model (FDL), once we include the following variables
model7 <- lm(gfr ~ year + ww2 + pill + pe + pe_1 + pe_2, data=dt)
summary(model7)
plot(dt$gfr, type = "b", xlab = "Year", ylab = "General Fertility Rate", main = "Model 7 Fertility on Years")
lines(model7$fitted.values)

# Is the effect of the pe (personal exemption from taxes for adulsts because of kids) significant?
# we should test the joint significance on all three terms, pe, pe_1 and pe_2
anova(model6, model7) # F-test for pe, pe_1, and pe_2
# Because of missing values, NA, errors occurred beause models were not all fitted to the same size of dataset
# To fix it, we can either go back to Model6 and delete the first two rows because there are too many NAs
# dt[-1,] means the drop the first line
# To drop two lines, we sa -c(1,2)
model6_fix <- lm(gfr ~ year + ww2 + pill, data=dt[-c(1,2),])
summary(model6_fix)
plot(dt$gfr, type = "b", xlab = "Year", ylab = "General Fertility Rate", main = "Model 6_fix Fertility on Years")
lines(model6_fix$fitted.values)

# Now we re-run the ANOVA test
# F-test for pe, pe_1, pe_2
anova(model6_fix, model7)
# The joint significance is higly significant

# The same test can be done using the car packages
library(car)
linearHypothesis(model7, c("pe","pe_1","pe_2"))

