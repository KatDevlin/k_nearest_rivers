# imputation, linear prediction, k-nearest neighbors
#
# This example uses a dataset containing several variables measuring
# rivers' CO2 emissions. I don't have the original documentation outlining
# what each variable actually is, but I'm guessing most are just what they
# sound like - temp is water temp in Celcius, depth is depth of the river,
# etc. Flow? Maybe how much water flows through it? Must be different than
# velocity, so this `flow` is the one where I'm least certain. Regardless,
# I think we know enough to do some analysis.

# setwd("~/Desktop/k_nearest_rivers")

y <- read.csv("RiverCO2.csv", as.is=TRUE)

# this chunk provided by my professor
y$temp.orig <- y$temp            # Saved for convenience (see below)
y$logco2 <- log(y$co2)
y$logslope <- log(y$slope)
y$logdepth <- log(y$depth)
y$logflow <- log(y$flow)
y$logvelocity <- log(y$velocity)
y$width[is.na(y$width)] <- "missing"
y$width.f <- factor(y$width)

lm.logflow <- lm(logflow ~ logdepth + logvelocity, data=y)
summary(lm.logflow)

y$logflow[is.na(y$logflow)] <- predict(lm.logflow,
                                       newdata=y[is.na(y$logflow),])
y$flow[is.na(y$flow)] <- exp(y$logflow[is.na(y$flow)])
# end of prof=provided code

# Fitting the best model:

lm.temp <- lm(temp ~ logslope + logdepth + logvelocity + logflow +
                width.f, data=y)
summary(lm.temp)

lm.temp2 <- lm(temp ~ logvelocity + logflow + width.f, data=y)
summary(lm.temp2)
anova(lm.temp2, lm.temp)

# lm.temp2 is my best model.


# With the new lm.temp2 model, I was asked to impute all the missing 
# temperatures.  Code below, assigning the final model to 'lm.temp' and 
# then using it to "fill in" the positions y$temp[is.na(y$temp.orig)].
# Intermediary models and exploration not included.

# install.packages("YaleToolkit")
library(YaleToolkit)
whatis(y)
# this is just showing me there are 67 missing temp values

# final model from above, renamed 
lm.temp <- lm(temp ~ logvelocity + logflow + width.f, data=y)
summary(lm.temp)

y$temp[is.na(y$temp)] <- predict(lm.temp,
                                 newdata=y[is.na(y$temp),])

summary(y$temp[is.na(y$temp.orig)])   
whatis(y)
# and now those 67 values of `temp` are no longer missing. 

## Next, I'll use a k-nearest-neighbor approach to imputation for
## the variable 'temp'.

## I'll start with a "reset" here. If you run the following it resets the
# temp variable while still preserving what we did above:

y$temp.model <- y$temp        
y$temp <- y$temp.orig 
##
## To make our lives a little easier, our prof only wanted us to include
## four variables to solve this part of the challenge. The row indices are 
## gathered corresponding to the missing values for temp:

z <- y[, c("logslope", "logdepth", "logflow", "logvelocity")]
notemp <- which(is.na(y$temp))
delta <- rep(NA, nrow(z))      # Not putting this as a column of z
# to avoid confusion. Careful!

# So the first row with missing temp is row 5 of y, corresponding to
# to observed information in row 5 of z.
#
# Now I was asked to calculate the "distance" between row five and every 
# other row, saving these distances in the new variable
# 'delta'.  PROF HINT: delta[5] should be 0 after you do the calculation!

for (i in 1:nrow(z)) {
  delta[i] <- sum((z[5, ] - z[i, ])^2) # distance of i-th row to 5th row
} 
head(delta)
delta[5]
summary(delta)          

#
# Next, I was asked to identify 5 rivers (or more than 5 if tied 
# distances in delta) "closest to" this one river (row 5, which I think is
# just a coincidence). Then I use the mean value of 'temp' from
# this group of "close" rivers to fill in the missing value of y$temp[5].
#

delta[is.na(y$temp)] <- 100000
delta[notemp] <- 100000

top5q <-  5/length(delta)
s <- quantile(delta, top5q)

#check
sum(delta <= s)
nbs <- delta <= s

y$temp[5] <- mean(y$temp[nbs])
y$temp[5] # so looks like that worked

# PROF CLARIFICATION 11/14. The 5 "closest" rivers might
# themselves having missing 'temp' -- so you're really looking for the
# nearest neighbors that have actual, original, temp measurements.
# END CLARIFICATION 11/14.
#
# Now to solve the full problem, I'll do this in a loop until I have imputed
# all 67 missing values of temp.
#
top5q <-  5/length(delta)
for (j in notemp) { # iterating over indices of missing values
  for (i in 1:nrow(z)) { 
    delta[i] <- sum((z[j, ] - z[i, ])^2) # distance of i-th row to missing row
  } 
  delta[notemp] <- 100000
  s <- quantile(delta, top5q)
  nbs <- delta <= s
  y$temp[j] <- mean(y$temp[nbs])  
}


y$temp[5]
y$temp[29]
y$temp[505]
y$temp[211]
y$temp[60]

# it's working correctly! These were all missing before. One more check:
whatis(y)

# Now we have no missing values for temp; they've all been imputed with
# values based off of their nearest neighbors.
 