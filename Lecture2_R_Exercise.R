 skoda <- read.csv("http://nb.vse.cz/~zouharj/econ/skoda.csv")
 
 #Read first 6 obs in dataset
 head(skoda)
 
 #Data eyeballing
 #Install the package that we are going to use
 
 #install.packages("GGally") This is done, and only needs to be done once
 
 #Load this package GGally so it exists in memory
 library(GGally)
 
 # ggpairs gives a nice pair of different plots regarding the dataset
 ggpairs(skoda)
 
 #We run a regression for price in terms of km, ~ means as a function of
 #Then we use the lm() function, with lm(price~km, data=skoda)
 lm(skoda$price~skoda$km, data = skoda)
 
 #ANOVA test in R, test price against categorical variable fuel
 regressPriceFuel <- lm(skoda$price~skoda$fuel, data = skoda)
 summary(regressPriceFuel)
 
 # log(price) = beta0 + beta1*kim + u -> log(price)~km
 lm(log(price)~km, data = skoda) # This is a log-level model
 
 # rescale the km variable because otherwise the result is really ugly
 skoda$km10000 <- skoda$km / 10000 # 1 unit = 10000 km
 # re-run the regression model using the km10000 variable
 lm(log(price)~km10000, data = skoda)
 
 # Another way to re-scale is to do so without creating a new variable
 # You have to do so by using I(km/10000).
 # lm(log(price)~I(km/10000), data=skoda)
 
 #log-log model (constant elasticity model)
 lm(log(price)~log(km), data = skoda)

 # Multi-regression
 lm(log(price)~log(km) + year, data = skoda) 
 # do not log year, because it means nothing to say if the year increases by 10% 
 # consider whether the interpretation is nice or not
 
 # Let's use age instead of the year of manufacture
 skoda$age <- 2004 - skoda$year # Because data were collected in 2004
 lm(log(price)~log(km) + age, data = skoda)
 
 # What is the exact effect of an additional year
 # as implied by the estimated equation
 exp(-0.22658) # This equals = 0.7972556
 
 #"New Price" = "Old Price" * 0.7972556
 # i.e. the price drops by approximately 20.3 per cent
 
 # Let's use all the variables to run a regression
 # age and year cannot be used simultaneously for multi-colinear relationship
 lm(log(price)~log(km) + age + model + fuel + combi, data = skoda)
 
 
 