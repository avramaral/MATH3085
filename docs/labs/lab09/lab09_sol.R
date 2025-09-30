
lymphoma <- read.csv("../datasets/lymphoma.csv") # Change the path, if needed
head(lymphoma)

# make a three-way contingency table for the data
xtabs(Count ~ Cell + Sex + Remis, data = lymphoma)

# Fit a saturated model:
ly_sat <- glm(Count ~ Cell * Remis * Sex, data = lymphoma, family = poisson)

# Fitted values
# The fitted values are the same as the observed counts, as the saturated model fits perfectly.
fitted(ly_sat)
data.frame(fitted = fitted(ly_sat), observed = lymphoma$Count)

# We can see the scaled deviance in the output from `summary()`.
# The scaled deviance (residual deviance) is 0.
summary(ly_sat)

# Drop the three factor interaction term.
ly_glm1 <- update(ly_sat, . ~ . - Cell:Remis:Sex)
summary(ly_glm1)

# To fit the same model directly, we could do use
ly_glm1_direct <- glm(Count ~ Cell + Remis + Sex + Cell:Remis + Cell:Sex + Remis:Sex, data = lymphoma, family = poisson)

# Model without the `Remis:Sex` interaction.
ly_glm2 <- update(ly_glm1, . ~ . - Remis:Sex)

# Log-likelihood ratio tests to drop terms.
anova(ly_glm2, ly_glm1, test = "LRT")

## # Do not reject null hypothesis of simpler model, prefer `ly_glm2`.
## 
## # Now consider dropping `Cell:Remis`.
## ly_glm3 <- update(ly_glm2, . ~ . - Cell:Remis)
## anova(ly_glm3, ly_glm2, test = "LRT")
## # The p-value is small, so we reject the simpler model, and still prefer `ly_glm2`.
## 
## # Now consider dropping `Cell:Sex`.
## ly_glm4 <- update(ly_glm2, . ~ . - Cell:Sex)
## anova(ly_glm4, ly_glm2, test = "LRT")
## # The p-value is small, so we reject the simpler model, and still prefer `ly_glm2`.
## 
## # We must then keep all the main effects in the model, because we include each of `Cell`, `Remis` and `Sex` in at least one interaction term.

# Find the expected cell counts under the model `ly_glm2`
fitted_count <- fitted(ly_glm2)
fitted_count

# Find the expected cell probabilities under the model `ly_glm2`
fitted_prob <- fitted_count/(sum(fitted_count))
fitted_prob

# We can add up the probabilities for each cell type to give the marginal probabilities as in Table 6.8.
# The odds of remission are (for female patients)
0.2208 / 0.1792
# and (for male patients)
0.1792 / 0.4208
# which are not the same.
# Female patients are much more likely to go into remission. Thus, `Remis` and `Sex` are not marginally independent.
