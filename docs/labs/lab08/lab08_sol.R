
accident <- read.csv("../datasets/accident.csv") # Change the path, if needed
head(accident)

# Fit a Poisson log-linear model to the data:
acc_glm <- glm(number ~ road + time + log(volume), family = poisson, data = accident)
summary(acc_glm)

# The output seems to say Mill road is more dangerous than Trumpington road. 
# The mornings and afternoons are about as dangerous as each other and each is quite a lot more dangerous than the middle of the day. 
# The accident rate has a strong dependence on the traffic volume.

# From the summary output, the scaled deviance is 1.88. 
# `acc_glm` has 5 free parameters, and there are `n = 6` data points, so if the model is correct, we expect the scaled deviance to have chi^2_1 distribution.
# The 95% point of a chi^2_1 is
qchisq(0.95, df = 1)
# i.e., 3.84. 
# Since the scaled deviance is less than this, there is no evidence of poor model fit from this measure.

# Consider model without `volume`.
acc_glm_rt <- glm(number ~ road + time, family  = poisson, data = accident)
anova(acc_glm_rt, acc_glm, test="LRT")
# We prefer the model with `volume`.

# Consider model without `time`.
acc_glm_rv <- glm(number ~ road + log(volume), family = poisson, data = accident)
anova(acc_glm_rv, acc_glm, test="LRT")
# It is borderline whether or not to include `time`.

# Consider model without `road`.
acc_glm_tv <- glm(number ~ time + log(volume), family = poisson, data = accident)
anova(acc_glm_tv, acc_glm, test="LRT")
# We prefer the model with `road`.


# Analysis of `hodgkins.csv` data
hodgkins <- read.csv("../datasets/hodgkins.csv") # Change the path, if needed
head(hodgkins)

# Display the data in contingency table form:
xtabs(count ~ type + rtreat, data = hodgkins)

# Fit a log-linear model with main effects only.
hod_main <- glm(count ~ type + rtreat, family = poisson, data = hodgkins)

# Fit a log-linear model with interaction between main effects.
hod_int <- glm(count ~ type * rtreat, family = poisson, data = hodgkins)

# Find estimated cell counts under the two models.
fitted(hod_main)
fitted(hod_int)

# For the saturated model, the estimated cell counts are equal to the observed cell counts.

# Compare the two models
anova(hod_main, hod_int, test = "LRT")

# There is strong evidence to prefer `hod_int`, and we conclude that response to treatment is dependent on the type of the disease.
