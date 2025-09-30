
heart <- read.csv("../datasets/heart.csv") # Change the path, if needed
head(heart)

# Plot the proportion of patients who survived against each of the explanatory variables
heart$proportion <- with(heart, n_survived / n_patients)
heart_agg <- apply(heart[, 4:7], 2, function(x) aggregate(proportion ~ x, data = heart, FUN = mean))
par(mfrow = c(2, 2))
invisible(lapply(1:4, function(x) barplot(heart_agg[[x]][, 2], ylim = c(0, 1), ylab = "Proportion", 
                                          names.arg = heart_agg[[x]][, 1], main = names(heart_agg)[x])))
par(mfrow = c(1, 1))

# Fit a logistic regression of proportion of patients who survive on site, treatment, and an interaction between site and treatment.
mod_SR <- glm(n_survived / n_patients  ~ site + treatment + site:treatment, family = binomial, weights = n_patients, data = heart)
summary(mod_SR)

# Alternatively, specify the main and interaction effects in one term (`site * treatment`)
mod_SR_2 <- glm(n_survived / n_patients  ~ site * treatment, family = binomial, weights = n_patients, data = heart)

# The same terms are included in both cases.
summary(mod_SR)
summary(mod_SR_2)

# Fit a three-factor interaction between `site`, `blocker`, and `treatment`.
mod_SBR <- glm(n_survived / n_patients  ~ site * blocker * treatment, family = binomial, weights = n_patients, data = heart)
summary(mod_SBR)

# Fit a logistic regression with all main effects, but no interactions:
mod_S_T_B_R <- glm(n_survived / n_patients  ~ site + time + blocker + treatment, family = binomial, weights = n_patients, data = heart)
summary(mod_S_T_B_R)

# Hypothesis test of each main effect.
anova(mod_S_T_B_R, test = "LRT")
# We should include `site`, `blocker` and `treatment`, but don't need to include `time`
mod_S_B_R <- glm(n_survived / n_patients ~ site + blocker + treatment, family = binomial, weights = n_patients, data = heart)
anova(mod_S_B_R, test = "LRT")
# We should keep all of the remaining main effects.

# We should now consider potential interactions between the main effects.

# Possible two-factor interactions: `site * blocker`, `site * treatment`, `blocker * treatment`.
mod_SB_R <- glm(n_survived / n_patients  ~ site * blocker + treatment, family = binomial, weights = n_patients, data = heart)
anova(mod_S_B_R, mod_SB_R, test = "LRT")
# Interaction `site:blocker` not needed.

mod_SR_B <- glm(n_survived / n_patients  ~ site * treatment + blocker, family = binomial, weights = n_patients, data = heart)
anova(mod_S_B_R, mod_SR_B, test = "LRT")
# Interaction `site:treatment` not needed.

mod_S_BR <- glm(n_survived / n_patients  ~ site + blocker * treatment, family = binomial, weights = n_patients, data = heart)
anova(mod_S_B_R, mod_S_BR, test = "LRT")
# Interaction `blocker:treatment` not needed.

# So we prefer `mod_S_B_R` as our final model.

mod_final <- glm(n_survived / n_patients  ~ treatment + blocker + site,
                 family = binomial, weights = n_patients, data = heart)
summary(mod_final)

# Fit the saturated model with all interaction terms.
mod_STBR <- glm(n_survived / n_patients  ~ site * time * blocker * treatment, family = binomial, weights = n_patients, data = heart)

# Test whether each term is needed in the model.
anova(mod_STBR, test = "LRT")

# The last row compares the models
# H_0: all interactions betweeen three variables: STB, STR, SBR, TBR against
# H_1: all possible interactions (STBR)
# Or mathematically: H_0: gamma_{STBR}(s, t, b, r) = 0 against
# H_1: gamma_{STBR}(s, t, b, r) is unrestricted.

# The sixth row compares the models
# H_0: all main effects only (S, T, B, R) against
# H_1: interaction between site and time (ST)
# Or mathematically: H_0: gamma_{ST}(s, t) = 0 against
# H_1: gamma_{ST}(s, t) is unrestricted.

# The preferred model is again one with main effects for `site`, `blocker` and `treatment`, and no interactions (as `mod_final` above)


# Make predictions for a patient with anterior infarction, no beta blockers.

newdata_active <- data.frame(site = "anterior", blocker = "no", treatment = "active")
p_hat_active   <- predict(mod_final, newdata = newdata_active, type = "response")
# Estimated probability of survival given active treatment is ~90%
p_hat_active

newdata_placebo <- data.frame(site = "anterior", blocker = "no", treatment = "placebo")
p_hat_placebo <- predict(mod_final, newdata = newdata_placebo, type = "response")
# Estimated probability of survival given placebo treatment is ~87%
p_hat_placebo

# Find a confidence interval for probability of survival given active treatment.
pred_eta_active <- predict(mod_final, newdata = newdata_active, se.fit = TRUE)
CI_eta_active <- c(pred_eta_active$fit - qnorm(0.975) * pred_eta_active$se.fit, 
                   pred_eta_active$fit + qnorm(0.975) * pred_eta_active$se.fit)
CI_p_active <- exp(CI_eta_active) / (1 + exp(CI_eta_active))
CI_p_active

# Find a confidence interval for probability of survival given placebo treatment.
pred_eta_placebo <- predict(mod_final, newdata = newdata_placebo, se.fit = TRUE)
CI_eta_placebo <- c(pred_eta_placebo$fit - qnorm(0.975) * pred_eta_placebo$se.fit, 
                   pred_eta_placebo$fit + qnorm(0.975) * pred_eta_placebo$se.fit)
CI_p_placebo <- exp(CI_eta_placebo) / (1 + exp(CI_eta_placebo))
CI_p_placebo
