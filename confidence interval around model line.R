x<- c(rnorm(55, 2.4, .65), rnorm(75, 3.34, .55))
y<- c(rep(0, 55), rep(1, 75))

require(stats); require(graphics)
library(splines)
# x_raw <- seq(1,10,0.1)
N <- length(x)
# y <- cos(x_raw)+rnorm(len_data,0,0.1)
# y[30] <- 1.4 # outlier point

summary(fm1 <-  glm(y ~ x, family = binomial))
x.new <-seq(min(x), max(x),len = N)
plot(x = x, y = y, type = 'p', ylim = c(-0.2, 1.2))
y.new <- predict(fm1, data.frame(x = x.new), type = "response")

length(y.new)
lines(x.new, y.new, col = "red")

phat <- mean(y.new)
marg_err<- 1.96*sqrt(phat*(1-phat)/N)
y.new_minus <- y.new - marg_err
y.new_plus <-y.new + marg_err

# plot(x = x_raw, y = y,type = 'n')
polygon(c(x.new,rev(x.new)),c(y.new_minus,rev(y.new_plus)),col = "grey80", border = NA)
points(x = x, y = y, type = 'p')
lines(x= x.new, y = y.new_plus, lty = 2, col = 'grey')
lines(x= x.new, y = y.new, col = "red")
lines(x= x.new, y = y.new_minus, lty = 2, col = 'grey')

logiplot(independ = x, depend = y, box = TRUE, dit = FALSE, hist = TRUE)

plot(x, y, ylim = c(-0.2, 1.2))

qnorm(((1-0.95)/2)+0.95)


# https://arxiv.org/pdf/1604.01242.pdf
##########################################
# https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression

foo <- mtcars[,c("mpg","vs")]; names(foo) <- c("x","y") ## Working example data
mod <- glm(y ~ x, data = foo, family = binomial)
preddata <- with(foo, data.frame(x = seq(min(x), max(x), length = 100)))
preds <- predict(mod, newdata = preddata, type = "link", se.fit = TRUE)


critval <- qnorm(0.975) ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit




fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(upr)
lwr2 <- mod$family$linkinv(lwr)


preddata$lwr <- lwr2 
preddata$upr <- upr2 
ggplot(data=foo, mapping=aes(x=x,y=y)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=preddata, mapping=aes(x=x, y=upr), col="red") + 
  geom_line(data=preddata, mapping=aes(x=x, y=lwr), col="red") 

