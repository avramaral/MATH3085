
beetle <- read.csv("../datasets/beetle.csv") # Change the path, if needed
head(beetle)

# Add a column for the proportion of beetles killed.
beetle$prop_killed <- beetle$killed / beetle$exposed

plot(prop_killed ~ dose, data = beetle)

# Fit a linear model of prop_killed on dose
beetle_lm <- lm(prop_killed ~ dose , data = beetle, weights = exposed)

plot(prop_killed ~ dose, data = beetle)
abline(beetle_lm, col = 4)

# The fitted regression line does not look reasonable, as for large or small doses, it will give a predicted proportion of beetles killed which is greater than 1, or less than 0.

# Predict for `dose = 2`
newdata <- data.frame(dose = 2)
predict(beetle_lm, newdata = newdata)
# The predicted proportion of beetles killed is 1.7, which is greater than 1. This is not a sensible prediction.

# Fit a logistic regression model, using prop_killed as response.
beetle_glm <- glm(prop_killed ~ dose, data = beetle, family = binomial, weights = exposed)

# Fit a logistic regression model, with a two-column matrix, with columns (number of successes, and number of failures) as the response.

beetle_glm_2 <- glm(cbind(killed, exposed - killed) ~ dose, data = beetle, family = binomial)

# Summarise the fitted logistic regression.
# `dose` is highly significant.
summary(beetle_glm)

# Make predictions at `dose = 1.7` and `1.8`
# By default, in `predict`, type = "link", which means we predict `logit(p)`
newdata <- data.frame(dose = c(1.7, 1.8))
predict(beetle_glm, newdata = newdata)

?predict.glm

# To predict probabilities `p`, use `type = "response"`
predict(beetle_glm, newdata = newdata, type = "response")

# To predict at `dose = 2`, we can do as follows
newdata <- data.frame(dose = 2)
predict(beetle_glm, newdata = newdata, type = "response")
# This is very close to (but less than) 1, which is much more sensible than the predicted probability from the linear model (1.7).

# Add fitted regression curves from linear model and logistic GLM to a scatterplot of `prop_killed` against `dose`:
newdata <- data.frame(dose = seq(1.6, 2, length = 30))
plot(prop_killed ~ dose, data = beetle, ylim = c(-0.5, 1.5), xlim = c(1.6, 2))
abline(beetle_lm, col = 4)
lines(newdata$dose, predict(beetle_glm, newdata = newdata, type = "response"), col = 2)
abline(h = c(0,1), lty = 2)
legend("topleft", c("Linear model", "Logistic GLM"), lty = 1, col = c(4, 2))
