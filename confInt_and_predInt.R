library(mvtnorm)
library(tidyverse)

N<- 1000 # number of simulated observations

# linear regression----
# https://stackoverflow.com/questions/25290619/geom-smooth-stat-smooth-confidence-interval-not-working
xbar<- 24 # mean or expected value of x - independent variable
xvar<- 3 # variance of x
ybar<- 15 # mean or expected value of y
yvar<- 2.5 # variance of y

# set up for generating random numbers
rsqu<- 0.65 # coefficient of determination
rescov<- array(NA, dim= c(2,2))
rescov[1,1]<- xvar
rescov[2,2]<- yvar
rescov[1,2]<- rescov[2,1]<- (rsqu * sqrt(rescov[1,1]) *sqrt(rescov[2,2]))

# create the correlated data
foo<- as.data.frame(rmvnorm(n = N,
               mean = c(xbar,ybar),
               sigma = rescov))
names(foo)<- c("x", "y")

# plot the result
p<- ggplot(foo, aes(x = x, y = y))+
  geom_point()+
  theme_bw()
p

# create model and some predictions
summary(mod<- lm(y ~ x, data = foo))
xnew <- with(foo, data.frame(x = seq(min(x), max(x), length = N))) # 'new' independent data
yhat<- as.vector(predict(mod, newdata = xnew)) # predictions from 'new' independent data
new.data<- as.data.frame(cbind(xnew$x, yhat))
names(new.data)<- c("xnew", "yhat")
head(new.data)

# Predict these data for
predx <- data.frame(x = seq(from = min(foo$x), to = max(foo$x), length = N))

# create confidence interval
conf.int <- cbind(predx, predict(mod, newdata = predx, interval = "confidence", level = 0.95))

# create prediction intervals
pred.int <- cbind(predx, predict(mod, newdata = predx, interval = "prediction", level = 0.95))
man <- predict(mod, newdata = predx, se = TRUE)

# Manual calculation of confidence interval, tolerance of 0.95 (1.96).
lvl <- qt(1-(1 - 0.95)/2, mod$df.residual) # Thank you, @Roland (http://chat.stackoverflow.com/transcript/message/10581408#10581408)
conf.int.man <- cbind(predx, fit = man$fit, lwr = man$fit - lvl * man$se.fit, upr = man$fit + lvl * man$se.fit)

ggplot(pred.int, aes(x = x, y = fit)) +
  theme_bw() +
  ggtitle("Prediction interval for future observations from predict()") +
  geom_point(data = foo, aes(x = x, y = y)) +
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") +
  geom_line(data = pred.int, aes(y = lwr, x = x), col = "red", line = "dashed")+
  geom_line(data = pred.int, aes(y = upr, x = x), col = "red", line = "dashed")


# logistic regression----
# https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression
# generate some data
N<- 1000
x<- c(rnorm(N*5/8, 2.4, .445), rnorm(N*3/8, 3.34, .45))
y<- c(rep(0, N*5/8), rep(1, N*3/8))
foo<- as.data.frame(cbind(x,y))
p <- ggplot(foo, aes(x = x, y = y))+
  geom_point()+
  theme_bw()
p

# create the model and some predictions
summary(mod <- glm(y ~ x, data = foo, family = binomial))
xnew <- with(foo, data.frame(xnew = seq(min(x), max(x), length = N)))
yhat <- predict(mod, newdata = xnew, type = "link", se.fit = TRUE)

foo<- cbind(foo, xnew$xnew, yhat$fit, yhat$se.fit)
names(foo)<- c("x", "y", "xnew", "yhat", "se.fit")

# find the Wald CI in eta space
critval <- qt(0.975, N-2) ## 95% CI
upr1 <- foo$yhat + (critval * foo$se.fit)
lwr1 <- foo$yhat - (critval * foo$se.fit)
fit1 <- foo$yhat

# convert Wald CI to logistic regression/real space
foo$fit <- mod$family$linkinv(fit1)
# exp(fit1[1])/(1+exp(fit1[1])) # explicit link function
# foo$fit[1]

foo$lwrci <- mod$family$linkinv(upr1)
foo$uprci <- mod$family$linkinv(lwr1)

# find the prediction interval values in eta space
# https://newonlinecourses.science.psu.edu/stat414/node/298/
MSEy<- sqrt(sum(y-mean(y))^2)




upr2 <- foo$fit + (critval * foo$se.fit) # prediction interval formula not the same as CI 
lwr2 <- foo$fit - (critval * foo$se.fit)

# convert to "real" space rather than eta space
fitp <- mod$family$linkinv(fit2)
uprp <- mod$family$linkinv(upr2)
lwrp <- mod$family$linkinv(lwr2)
new.data$lwrp <- lwrp 
new.data$uprp <- uprp


# chart the results
ggplot(foo, aes(x = x, y = fit)) +
  theme_bw() +
  ggtitle("Prediction interval for future observations from predict()") +
  geom_point( aes(x = x, y = y)) +
  geom_smooth( aes(ymin = lwrci, ymax = uprci), stat = "identity") 
  # geom_line(aes(y = lwr, x = x), col = "red", line = "dashed")+
  # geom_line(aes(y = upr, x = x), col = "red", line = "dashed")

# Poisson regression ----
# generate some data
# https://www.r-bloggers.com/easily-generate-correlated-variables-from-any-distribution-without-copulas/


x<- runif(N, 0.25, 1.12) # some measurement
x<- sort(x) # to ensure smaller goes with small
var(x)

ybar<- runif(N, 0, 150) # the mean count of some response at a measurement
ybar<-sort(ybar) #  to ensure smaller goes with small
plot(ybar)

plot(x, ybar) # check what we have so far

# set up for adding some errors
rsqu<- 0.99 # coef of det
rescov<- array(NA, dim= c(2,2))
y<- NA
xycov<- vector(mode = "numeric", N)

for(i in 1:N){
  # set up the variance-covariance matrix
  rescov[1,1]<- var(x) # variance of variable x
  rescov[2,2]<- ybar[i]# variance of variable 2
  xycov[i]<- rescov[1,2]<- rescov[2,1]<- (rsqu * sqrt(rescov[1,1]) *sqrt(rescov[2,2]) )
  
  
  
  # set.seed(12)
  y[i]<- rmvnorm(n = N,
                 mean = c(mean(x),ybar[i]),
                 sigma = rescov)
}
plot(x, y)
plot(xycov)

# these three need to go inside the for loop
rescov[1,1]<- xbar[i] # variance of variable 1
rescov[2,2]<-  # variance of variable 2
rescov[1,2]<- rescov[2,1]<- -(rsqu * sqrt(rescov[1,1]) *sqrt(rescov[2,2]) )
 rmvnorm




foo<- as.data.frame(cbind(x,y))
plot(x, y, data = foo)

# create the model and some predictions
summary(mod <- glm(y ~ x, data = foo, family = binomial))
preddata <- with(foo, data.frame(x = seq(min(x), max(x), length = 100)))
preds <- predict(mod, newdata = preddata, type = "link", se.fit = TRUE)

# find the relevant conf int values
critval <- qnorm(0.975) ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# convert to "real" space rather than eta space
fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(upr)
lwr2 <- mod$family$linkinv(lwr)
preddata$lwr <- lwr2 
preddata$upr <- upr2 

# chart the results
ggplot(data=foo, mapping=aes(x=x,y=y)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=preddata, mapping=aes(x=x, y=upr), col="red") + 
  geom_line(data=preddata, mapping=aes(x=x, y=lwr), col="red") +
  theme_minimal()
