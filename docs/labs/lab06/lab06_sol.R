
shuttle <- read.csv("../datasets/shuttle.csv") # Change the path, if needed
head(shuttle)

# Add a column to the data for the total number of O-rings on each shuttle.
shuttle$n <- rep(6, nrow(shuttle))

# Make plots of proportion of O-rings damaged against `temp`, `pressure` and `orbiter`.
par(mfrow = c(2, 2))
plot(n_damaged / n ~ temp, data = shuttle, pch = 16, ylim = c(0, 1), ylab = "Proportion damaged")
plot(n_damaged / n ~ pressure, data = shuttle, pch = 16, ylim = c(0, 1), ylab = "Proportion damaged")
shuttle_props <- aggregate(n_damaged / n ~ orbiter, data = shuttle, FUN = mean)
barplot(shuttle_props$`n_damaged/n`, names.arg = shuttle_props$orbiter, ylim = c(0, 1), ylab = "Proportion damaged")
par(mfrow = c(1, 2))

# Fit a logistic regression of proportion of damaged O-rings against `temp`, `pressure` and `orbiter`.
shuttle_glm1 <- glm((n_damaged / n) ~ temp + pressure + orbiter, data = shuttle, family = binomial, weights = n)

summary(shuttle_glm1)

# Conduct likelihood ratio tests for the presence of each explanatory variable.
anova(shuttle_glm1, test = "LRT")

# So we should keep `temp` in the model, but we do not need other variables.

# Fit a logistic regression on `temp`.
shuttle_glm2 <- glm((n_damaged / n) ~ temp, data = shuttle, family = binomial, weights = n)
summary(shuttle_glm2)

# Fit a logistic regression with an linear predictor which is quadratic in `temp`.
shuttle_glm3 <- glm((n_damaged / n) ~ temp + I(temp^2), data = shuttle, family = binomial, weights = n)

summary(shuttle_glm3)

# The coefficient for `I(temp^2)` is not required in the model, so prefer `shuttle_glm2`.

# Predict the probability of O-ring failure at 31F.
newdata <- data.frame(temp = 31)
pred_glm2 <- predict(shuttle_glm2, newdata = newdata, type = "response", se.fit = TRUE)
pred_glm2
# The predicted probability of O-ring failure at 31F is 0.82.

# Find a asymptotic 95% confindence interval for the probability of O-ring failure at 31F.
CI <- c(pred_glm2$fit - qnorm(0.975) * pred_glm2$se.fit, 
        pred_glm2$fit + qnorm(0.975) * pred_glm2$se.fit)
CI

# The upper limit of the confidence interval is greater than 1, which is not sensible.

# Predict the linear predictor at 31F.
pred_glm2_eta <- predict(shuttle_glm2, newdata = newdata, se.fit = TRUE)
pred_glm2_eta

# Find a confidence interval for the linear predictor at 31F.
CI_eta <- c(pred_glm2_eta$fit - qnorm(0.975) * pred_glm2_eta$se.fit, 
            pred_glm2_eta$fit + qnorm(0.975) * pred_glm2_eta$se.fit)
CI_eta

# To find the predicted probability, apply the inverse of the logit link.
# We have g(x) = log(x / 1 - x)
# So g^{-1}(x) = exp(x) / (1 + exp(x)) 
exp(pred_glm2_eta$fit) / (1 + exp(pred_glm2_eta$fit))
# The predicted probability of O-ring failure is 0.82, as before.

exp(CI_eta) / (1 + exp(CI_eta))
# This is different from previous confidence interval.
# The new confidence interval is more sensible than the previous one, as the upper limit is less than one.

# Plot proportion of O-rings damaged against temp, with a fitted probability curve added.
newdata <- data.frame(temp = seq(30, 100, length = 30))
plot(n_damaged / n ~ temp, data = shuttle, ylim = c(0, 1), xlim = c(30, 100))
lines(newdata$temp, predict(shuttle_glm2, newdata = newdata, type = "response"), col = 2)
