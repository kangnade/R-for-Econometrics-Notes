# import data and check it
dt <- read.csv("http://nb.vse.cz/~zouharj/econ/labour_market.csv")
head(dt)

# let's run regression from the presentation on interaction
# female:educ is the interaction expression
model1 <- lm(wage ~ female + educ + female:educ, data = dt)
summary(model1)
# Return to an additional year of education for woman, it is the
# sum of the two coefficients
1.8753 + 0.4822
# The gender gap wages for 0 years of education : 
-11.1136 # coefficient on female
# The gender gap wages for 25 years of education : 
# rescale the educ variable
-11.1136 + 25*0.4822 # intercept + years*interaction
# women with a phf are in fact better off

dt$educ_25 <- dt$educ - 25
model2 <- lm(wage ~ female + educ_25 + female:educ_25, data = dt)
summary(model2)

# To find the 95% CI for the gender gap at 25 years of education
confint(model2)
# now female coefficient means when educ_25 = 0, but here 0.9419 is not statistically significant

m1 <- lm(wage ~ female + educ + female:educ, data = dt)
summary(m1)
m2 <- lm(wage ~ female + educ + female:educ + metro + exper, data = dt)
summary(m2)

# What is the predicted wage for 
#---a male person 
#---with 15 years of education
#---25 years of exper
#---living in the country

# Mannual calculation can be done in different ways (NOT THE WAY WE DO IT)
-10.065991 - 11.92951*0 + 1.89380*15 + 3.53100*0 + 0.15422*25 + 0.52808*0*15

# Two ways to get the coefficients
m2$coefficients
coef(m2)
sum(c(1,0,15,0,25,0*15)*coef(m2)) # scalar product of the vector in sum
# in this way: c(1,0,15,0,25,0*15)%*%coef(m2)

# How about the 95% CI
# Create shifted versions of education and experience, so that the person of interest
# scores a 0 in all explanatory variables
dt$exper_25 <- dt$exper -25 # create a new variable shifting exper to 0 scale
dt$educ_15 <- dt$educ -15 # same thing for educ
m3 <- lm(wage ~ female + educ_15 + female:educ_15 + metro + exper_25, data = dt)
summary(m3)

# compare the two models
library(stargazer)
stargazer(m2, m3, type = "text")

# to get the confidence interval, just use the confidence interval function
# 95% CI for mean wage of mean living in the country with 15 educ and 25 exper
confint(m3)["(Intercept)",] # pick the row named Intercept, and take all the columns
# result is between 20 to 23 dollars

# if you want to provide a 95% CI interval that a specific/concrete individual
# first, obtain the standard error of prediction (vertical: se(theta-hat)from the model of transformed variables, 
# and horizontal: residual standard error sigma-hat, with a right angle, the diagnal is the prediction of std. error)
SE_prediction <- sqrt(0.89127^2 + 11.19^2) # Pythagoras, see the picture, use numbers from m3, standard error part
SE_prediction
PI_lower <- 22.19660 - 1.96*SE_prediction
PI_upper <- 22.19660 + 1.96*SE_prediction
c(PI_lower,PI_upper)
# Prediction Interval, for a concrete individual is quite large.
hist(dt$wage)
hist(m3$residuals)

# Ramsey RESET test
# Test for non-linearity
dt$yhat <- m3$fitted.values
dt$yhat2 <- dt$yhat^2
dt$yhat3 <- dt$yhat2^3

m3_ramseyRESET <- lm(wage ~ female + educ_15 + female:educ_15 + metro + exper_25 + 
                       yhat2 + yhat3, data = dt)
anova(m3, m3_ramseyRESET) # anova tests for joint significance of yhat2 and yhat3
# small p-value means that nonlinear terms have been omitted/ Nonlinearity exists
# you cannot type yhat^2 and yhat^3 inside the lm() function
# you have to use I(yhat^2) and I(yhat^3) to express the terms in lm() function if you do not
# generate new variables such as dt$yhat2, dt$yhat3

# let's try the same wih the log of wage instead of plain wage
m4 <- lm(log(wage) ~ female + educ_15 + female:educ_15 + metro + exper_25, data = dt)
summary(m4)
# to obtain the exponential of coefficients
exp(m4$coefficients)
# exp(beta-hat female) = 0.815, so the wage of a female with 15 years of education
# (because we are using education_15 in the interaction), is approximately by 18.5%
# lower than the wage of a comparable male person.

# Let's predict the wage of the person given above. From the output of our last line of code
# we have 
A <- 18.8376377 #In slides, A part, Intercept
# We need the B part from the slides in lecture
# two ways to obtain that
B1 <- exp(0.4998^2/2) # the residual std. error
B1
predicted_wage <- A*B1
predicted_wage

# the second way to do so, B is the E[exp(u)|x]
B2 <- mean(exp(m4$residuals)) # Duan's (1983) approach
B2
predicted_wage2 <- A*B2
predicted_wage2

# to get the confidence interval
exp(confint(m4))

hist(m4$residuals) # in PS2 we look at residuals, whether we think it is normally distributed
# we think whether we pick B1 or B2
# be advised to do a qqline
# we have to figure out
