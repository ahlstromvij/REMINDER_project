set.seed(100)

# load packages
library(lmtest)
library(sandwich)
library(ggplot2)
library(tidyr)

# read in data
model_data <- read.csv("data/model_data_IRT_propscores.csv")

# make sure each variable is of the correct class
class(model_data$gender) # factor
class(model_data$age) # integer
class(model_data$nationality) # factor
model_data$education_ISCED <- factor(model_data$education_ISCED, levels=c(0,1,2,3,4,5,6,7,8), ordered=TRUE) # ordered factor
model_data$in_paid_work <- factor(model_data$in_paid_work) # factor
model_data$religion <- factor(model_data$religion) # factor
class(model_data$ideology_left_right) # integer (treating this as continuous)
class(model_data$gen_imm_escape) # integer
class(model_data$gen_imm_work) # integer
class(model_data$gen_imm_study) # integer
class(model_data$gen_imm_family_war) # integer
class(model_data$gen_imm_marriage) # integer
class(model_data$gen_imm_ess_welfare) # integer
class(model_data$gen_imm_ess_jobs) # integer
class(model_data$gen_imm_ess_welfare) # integer
class(model_data$gen_imm_ess_safety) # integer
class(model_data$imm_anger) # integer
class(model_data$imm_fear) # integer
class(model_data$imm_hope) # integer
class(model_data$imm_sympathy) # integer
class(model_data$imm_size) # integer
class(model_data$eu_nat_identity) # integer
class(model_data$free_move_protect_jobs) # integer
class(model_data$free_move_protect_services) # integer
class(model_data$gen_know_switzerland) # integer
class(model_data$gen_know_ep) # integer
class(model_data$gen_know_party) # integer
class(model_data$mig_know_free_move) # integer
class(model_data$mig_know_schengen) # integer
class(model_data$mig_know_asylum) # integer
class(model_data$mig_know_syrians) # integer

# subset by nationality
table(model_data$nationality)
model_data_germany <- subset(model_data, nationality=="germany")
model_data_hungary <- subset(model_data, nationality=="hungary")
model_data_poland <- subset(model_data, nationality=="poland")
model_data_romania <- subset(model_data, nationality=="romania")
model_data_spain <- subset(model_data, nationality=="spain")
model_data_sweden <- subset(model_data, nationality=="sweden")
model_data_uk <- subset(model_data, nationality=="uk")

# create df to collect knowledge effects and confidence intervals
df_knowledge <- data.frame("nationality" = rep(c("germany","hungary","poland","romania","spain","sweden","uk"),2),
                           "type" = c(rep("general",7),rep("immigration",7)),
                           "effect" = NA,
                           "lwr" = NA,
                           "upr" = NA)

# information effects on perceptions about impacts on economy
# higher = better for economy

# germany
m_econ_gen_germany <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_germany, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_germany)

# robust SEs
m_econ_gen_germany_vcov <- vcovHC(m_econ_gen_germany, type="HC1")
coeftest(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)
df_knowledge$effect[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,1]
coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)
df_knowledge$lwr[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="germany" & df_knowledge$type=="general"] <- coefci(m_econ_gen_germany, vcov = m_econ_gen_germany_vcov)[2,2]

plot(m_econ_gen_germany, 1)
# Used to check the linear relationship assumptions. A horizontal line, without distinct patterns 
# is an indication for a linear relationship, what is good.
# If the residual plot indicates a non-linear relationship in the data, then a simple approach is 
# to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the 
# regression model.

plot(m_econ_gen_germany, 2)
# Used to examine whether the residuals are normally distributed. It’s good if residuals points 
# follow the straight dashed line.

plot(m_econ_gen_germany, 3)
# Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line 
# with equally spread points is a good indication of homoscedasticity.
# A possible solution to reduce the heteroscedasticity problem is to use a log or square root 
# transformation of the outcome variable (y).

plot(m_econ_gen_germany, 5)
# Residuals vs Leverage. Used to identify influential cases, that is extreme values that might 
# influence the regression results when included or excluded from the analysis. 
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers.
# A data point has high leverage, if it has extreme predictor x values. This can be detected by 
# examining the leverage statistic or the hat-value. A value of this statistic above 2(p + 1)/n 
# indicates an observation with high leverage (P. Bruce and Bruce 2017); where, p is the number of 
# predictors and n is the number of observations.

m_econ_imm_germany <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_germany, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_germany)
m_econ_imm_germany_vcov <- vcovHC(m_econ_imm_germany, type="HC1")
coeftest(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)
df_knowledge$effect[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,1]
coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)
df_knowledge$lwr[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="germany" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_germany, vcov = m_econ_imm_germany_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_germany)

# hungary
m_econ_gen_hungary <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_hungary, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_hungary)
m_econ_gen_hungary_vcov <- vcovHC(m_econ_gen_hungary, type="HC1")
coeftest(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)
df_knowledge$effect[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,1]
coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)
df_knowledge$lwr[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="hungary" & df_knowledge$type=="general"] <- coefci(m_econ_gen_hungary, vcov = m_econ_gen_hungary_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_hungary)

m_econ_imm_hungary <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_hungary, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_hungary)
m_econ_imm_hungary_vcov <- vcovHC(m_econ_imm_hungary, type="HC1")
coeftest(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)
df_knowledge$effect[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,1]
coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)
df_knowledge$lwr[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="hungary" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_hungary, vcov = m_econ_imm_hungary_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_hungary)

# poland
m_econ_gen_poland <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_poland, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_poland)
m_econ_gen_poland_vcov <- vcovHC(m_econ_gen_poland, type="HC1")
coeftest(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)
df_knowledge$effect[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,1]
coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)
df_knowledge$lwr[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="poland" & df_knowledge$type=="general"] <- coefci(m_econ_gen_poland, vcov = m_econ_gen_poland_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_poland)

m_econ_imm_poland <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_poland, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_poland)
m_econ_imm_poland_vcov <- vcovHC(m_econ_imm_poland, type="HC1")
coeftest(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)
df_knowledge$effect[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,1]
coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)
df_knowledge$lwr[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="poland" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_poland, vcov = m_econ_imm_poland_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_poland)

# romania
m_econ_gen_romania <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                          gender +
                          age +
                          education_ISCED +
                          in_paid_work +
                          religion +
                          ideology_left_right,
                        data = model_data_romania, 
                        weights = (weight.ATE_general))
summary(m_econ_gen_romania)
m_econ_gen_romania_vcov <- vcovHC(m_econ_gen_romania, type="HC1")
coeftest(m_econ_gen_romania, vcov = m_econ_gen_romania_vcov)
df_knowledge$effect[df_knowledge$nationality=="romania" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_romania, vcov = m_econ_gen_romania_vcov)[2,1]
coefci(m_econ_gen_romania, vcov = m_econ_gen_romania_vcov)
df_knowledge$lwr[df_knowledge$nationality=="romania" & df_knowledge$type=="general"] <- coefci(m_econ_gen_romania, vcov = m_econ_gen_romania_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="romania" & df_knowledge$type=="general"] <- coefci(m_econ_gen_romania, vcov = m_econ_gen_romania_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_romania)

m_econ_imm_romania <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                          gender +
                          age +
                          education_ISCED +
                          in_paid_work +
                          religion +
                          ideology_left_right,
                        data = model_data_romania, 
                        weights = (weight.ATE_imm))
summary(m_econ_imm_romania)
m_econ_imm_romania_vcov <- vcovHC(m_econ_imm_romania, type="HC1")
coeftest(m_econ_imm_romania, vcov = m_econ_imm_romania_vcov)
df_knowledge$effect[df_knowledge$nationality=="romania" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_romania, vcov = m_econ_imm_romania_vcov)[2,1]
coefci(m_econ_imm_romania, vcov = m_econ_imm_romania_vcov)
df_knowledge$lwr[df_knowledge$nationality=="romania" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_romania, vcov = m_econ_imm_romania_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="romania" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_romania, vcov = m_econ_imm_romania_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_romania)

# spain
m_econ_gen_spain <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_spain, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_spain)
m_econ_gen_spain_vcov <- vcovHC(m_econ_gen_spain, type="HC1")
coeftest(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)
df_knowledge$effect[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,1]
coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)
df_knowledge$lwr[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="spain" & df_knowledge$type=="general"] <- coefci(m_econ_gen_spain, vcov = m_econ_gen_spain_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_spain)

m_econ_imm_spain <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_spain, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_spain)
m_econ_imm_spain_vcov <- vcovHC(m_econ_imm_spain, type="HC1")
coeftest(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)
df_knowledge$effect[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,1]
coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)
df_knowledge$lwr[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="spain" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_spain, vcov = m_econ_imm_spain_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_spain)

# sweden
m_econ_gen_sweden <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_sweden, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_sweden)
m_econ_gen_sweden_vcov <- vcovHC(m_econ_gen_sweden, type="HC1")
coeftest(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)
df_knowledge$effect[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,1]
coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)
df_knowledge$lwr[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="sweden" & df_knowledge$type=="general"] <- coefci(m_econ_gen_sweden, vcov = m_econ_gen_sweden_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_sweden)

m_econ_imm_sweden <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_sweden, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_sweden)
m_econ_imm_sweden_vcov <- vcovHC(m_econ_imm_sweden, type="HC1")
coeftest(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)
df_knowledge$effect[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,1]
coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)
df_knowledge$lwr[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="sweden" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_sweden, vcov = m_econ_imm_sweden_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_sweden)

# uk
m_econ_gen_uk <- lm(gen_imm_ess_welfare ~ know_score_general_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_uk, 
                         weights = (weight.ATE_general))
summary(m_econ_gen_uk)
m_econ_gen_uk_vcov <- vcovHC(m_econ_gen_uk, type="HC1")
coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coeftest(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="general"] <- coefci(m_econ_gen_uk, vcov = m_econ_gen_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_gen_uk)

m_econ_imm_uk <- lm(gen_imm_ess_welfare ~ know_score_imm_binary +
                           gender +
                           age +
                           education_ISCED +
                           in_paid_work +
                           religion +
                           ideology_left_right,
                         data = model_data_uk, 
                         weights = (weight.ATE_imm))
summary(m_econ_imm_uk)
m_econ_imm_uk_vcov <- vcovHC(m_econ_imm_uk, type="HC1")
coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$effect[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coeftest(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)
df_knowledge$lwr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,1]
df_knowledge$upr[df_knowledge$nationality=="uk" & df_knowledge$type=="immigration"] <- coefci(m_econ_imm_uk, vcov = m_econ_imm_uk_vcov)[2,2]
par(mfrow = c(2, 2))
plot(m_econ_imm_uk)

# plot knowledge effects
ggplot(df_knowledge, aes(nationality, effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2) +
  facet_grid(. ~ type) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  coord_flip()

# calculate information effects

# create df to collect information effects
df_full_information <- data.frame("nationality" = rep(c("germany","hungary","poland","romania","spain","sweden","uk"),2),
                                  "type" = c(rep("general",7),rep("immigration",7)),
                                  "actual" = NA,
                                  "informed" = NA)

# germany - general
# actual support for affirmitive
df_germany_effect <- data.frame(model_data_germany$gen_imm_ess_welfare, model_data_germany$WEIGHTEX1)
names(df_germany_effect)[1:2] <- c("variable","weight")

# weighted support for affirmative
df_germany_effect$variable_weighted <- df_germany_effect$variable * df_germany_effect$weight
df_full_information$actual[df_full_information$nationality=="germany" & df_full_information$type=="general"] <- sum(df_germany_effect$variable_weighted)/sum(df_germany_effect$weight)

# predicted support for affirmative
model_data_germany$know_score_general_binary_original <- model_data_germany$know_score_general_binary
model_data_germany$know_score_general_binary <- 1

# predicted weighted support for affirmitive
df_germany_effect$variable_pred <- predict(m_econ_gen_germany, type = "response", newdata = model_data_germany)
df_germany_effect$variable_pred_weighted <- df_germany_effect$variable_pred * df_germany_effect$weight
df_full_information$informed[df_full_information$nationality=="germany" & df_full_information$type=="general"] <- sum(df_germany_effect$variable_pred_weighted)/sum(df_germany_effect$weight)

# germany - immigration
df_germany_effect_imm <- data.frame(model_data_germany$gen_imm_ess_welfare, model_data_germany$WEIGHTEX1)
names(df_germany_effect_imm)[1:2] <- c("variable","weight")

# weighted support for affirmative
df_germany_effect_imm$variable_weighted <- df_germany_effect_imm$variable * df_germany_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="germany" & df_full_information$type=="immigration"] <- sum(df_germany_effect_imm$variable_weighted)/sum(df_germany_effect_imm$weight)

# predicted support for affirmative
model_data_germany$know_score_imm_binary_original <- model_data_germany$know_score_imm_binary
model_data_germany$know_score_imm_binary <- 1

# predicted weighted support for affirmitive
df_germany_effect_imm$variable_pred <- predict(m_econ_imm_germany, type = "response", newdata = model_data_germany)
df_germany_effect_imm$variable_pred_weighted <- df_germany_effect_imm$variable_pred * df_germany_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="germany" & df_full_information$type=="immigration"] <- sum(df_germany_effect_imm$variable_pred_weighted)/sum(df_germany_effect_imm$weight)

# hungary - general
df_hungary_effect <- data.frame(model_data_hungary$gen_imm_ess_welfare, model_data_hungary$WEIGHTEX1)
names(df_hungary_effect)[1:2] <- c("variable","weight")
df_hungary_effect$variable_weighted <- df_hungary_effect$variable * df_hungary_effect$weight
df_full_information$actual[df_full_information$nationality=="hungary" & df_full_information$type=="general"] <- sum(df_hungary_effect$variable_weighted)/sum(df_hungary_effect$weight)
model_data_hungary$know_score_general_binary_original <- model_data_hungary$know_score_general_binary
model_data_hungary$know_score_general_binary <- 1
df_hungary_effect$variable_pred <- predict(m_econ_gen_hungary, type = "response", newdata = model_data_hungary)
df_hungary_effect$variable_pred_weighted <- df_hungary_effect$variable_pred * df_hungary_effect$weight
df_full_information$informed[df_full_information$nationality=="hungary" & df_full_information$type=="general"] <- sum(df_hungary_effect$variable_pred_weighted)/sum(df_hungary_effect$weight)

# hungary - immigration
df_hungary_effect_imm <- data.frame(model_data_hungary$gen_imm_ess_welfare, model_data_hungary$WEIGHTEX1)
names(df_hungary_effect_imm)[1:2] <- c("variable","weight")
df_hungary_effect_imm$variable_weighted <- df_hungary_effect_imm$variable * df_hungary_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="hungary" & df_full_information$type=="immigration"] <- sum(df_hungary_effect_imm$variable_weighted)/sum(df_hungary_effect_imm$weight)
model_data_hungary$know_score_imm_binary_original <- model_data_hungary$know_score_imm_binary
model_data_hungary$know_score_imm_binary <- 1
df_hungary_effect_imm$variable_pred <- predict(m_econ_imm_hungary, type = "response", newdata = model_data_hungary)
df_hungary_effect_imm$variable_pred_weighted <- df_hungary_effect_imm$variable_pred * df_hungary_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="hungary" & df_full_information$type=="immigration"] <- sum(df_hungary_effect_imm$variable_pred_weighted)/sum(df_hungary_effect_imm$weight)

# poland - general
df_poland_effect <- data.frame(model_data_poland$gen_imm_ess_welfare, model_data_poland$WEIGHTEX1)
names(df_poland_effect)[1:2] <- c("variable","weight")
df_poland_effect$variable_weighted <- df_poland_effect$variable * df_poland_effect$weight
df_full_information$actual[df_full_information$nationality=="poland" & df_full_information$type=="general"] <- sum(df_poland_effect$variable_weighted)/sum(df_poland_effect$weight)
model_data_poland$know_score_general_binary_original <- model_data_poland$know_score_general_binary
model_data_poland$know_score_general_binary <- 1
df_poland_effect$variable_pred <- predict(m_econ_gen_poland, type = "response", newdata = model_data_poland)
df_poland_effect$variable_pred_weighted <- df_poland_effect$variable_pred * df_poland_effect$weight
df_full_information$informed[df_full_information$nationality=="poland" & df_full_information$type=="general"] <- sum(df_poland_effect$variable_pred_weighted)/sum(df_poland_effect$weight)

# poland - immigration
df_poland_effect_imm <- data.frame(model_data_poland$gen_imm_ess_welfare, model_data_poland$WEIGHTEX1)
names(df_poland_effect_imm)[1:2] <- c("variable","weight")
df_poland_effect_imm$variable_weighted <- df_poland_effect_imm$variable * df_poland_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="poland" & df_full_information$type=="immigration"] <- sum(df_poland_effect_imm$variable_weighted)/sum(df_poland_effect_imm$weight)
model_data_poland$know_score_imm_binary_original <- model_data_poland$know_score_imm_binary
model_data_poland$know_score_imm_binary <- 1
df_poland_effect_imm$variable_pred <- predict(m_econ_imm_poland, type = "response", newdata = model_data_poland)
df_poland_effect_imm$variable_pred_weighted <- df_poland_effect_imm$variable_pred * df_poland_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="poland" & df_full_information$type=="immigration"] <- sum(df_poland_effect_imm$variable_pred_weighted)/sum(df_poland_effect_imm$weight)

# romania - general
df_romania_effect <- data.frame(model_data_romania$gen_imm_ess_welfare, model_data_romania$WEIGHTEX1)
names(df_romania_effect)[1:2] <- c("variable","weight")
df_romania_effect$variable_weighted <- df_romania_effect$variable * df_romania_effect$weight
df_full_information$actual[df_full_information$nationality=="romania" & df_full_information$type=="general"] <- sum(df_romania_effect$variable_weighted)/sum(df_romania_effect$weight)
model_data_romania$know_score_general_binary_original <- model_data_romania$know_score_general_binary
model_data_romania$know_score_general_binary <- 1
df_romania_effect$variable_pred <- predict(m_econ_gen_romania, type = "response", newdata = model_data_romania)
df_romania_effect$variable_pred_weighted <- df_romania_effect$variable_pred * df_romania_effect$weight
df_full_information$informed[df_full_information$nationality=="romania" & df_full_information$type=="general"] <- sum(df_romania_effect$variable_pred_weighted)/sum(df_romania_effect$weight)

# romania - immigration
df_romania_effect_imm <- data.frame(model_data_romania$gen_imm_ess_welfare, model_data_romania$WEIGHTEX1)
names(df_romania_effect_imm)[1:2] <- c("variable","weight")
df_romania_effect_imm$variable_weighted <- df_romania_effect_imm$variable * df_romania_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="romania" & df_full_information$type=="immigration"] <- sum(df_romania_effect_imm$variable_weighted)/sum(df_romania_effect_imm$weight)
model_data_romania$know_score_imm_binary_original <- model_data_romania$know_score_imm_binary
model_data_romania$know_score_imm_binary <- 1
df_romania_effect_imm$variable_pred <- predict(m_econ_imm_romania, type = "response", newdata = model_data_romania)
df_romania_effect_imm$variable_pred_weighted <- df_romania_effect_imm$variable_pred * df_romania_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="romania" & df_full_information$type=="immigration"] <- sum(df_romania_effect_imm$variable_pred_weighted)/sum(df_romania_effect_imm$weight)

# spain - general
df_spain_effect <- data.frame(model_data_spain$gen_imm_ess_welfare, model_data_spain$WEIGHTEX1)
names(df_spain_effect)[1:2] <- c("variable","weight")
df_spain_effect$variable_weighted <- df_spain_effect$variable * df_spain_effect$weight
df_full_information$actual[df_full_information$nationality=="spain" & df_full_information$type=="general"] <- sum(df_spain_effect$variable_weighted)/sum(df_spain_effect$weight)
model_data_spain$know_score_general_binary_original <- model_data_spain$know_score_general_binary
model_data_spain$know_score_general_binary <- 1
df_spain_effect$variable_pred <- predict(m_econ_gen_spain, type = "response", newdata = model_data_spain)
df_spain_effect$variable_pred_weighted <- df_spain_effect$variable_pred * df_spain_effect$weight
df_full_information$informed[df_full_information$nationality=="spain" & df_full_information$type=="general"] <- sum(df_spain_effect$variable_pred_weighted)/sum(df_spain_effect$weight)

# spain - immigration
df_spain_effect_imm <- data.frame(model_data_spain$gen_imm_ess_welfare, model_data_spain$WEIGHTEX1)
names(df_spain_effect_imm)[1:2] <- c("variable","weight")
df_spain_effect_imm$variable_weighted <- df_spain_effect_imm$variable * df_spain_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="spain" & df_full_information$type=="immigration"] <- sum(df_spain_effect_imm$variable_weighted)/sum(df_spain_effect_imm$weight)
model_data_spain$know_score_imm_binary_original <- model_data_spain$know_score_imm_binary
model_data_spain$know_score_imm_binary <- 1
df_spain_effect_imm$variable_pred <- predict(m_econ_imm_spain, type = "response", newdata = model_data_spain)
df_spain_effect_imm$variable_pred_weighted <- df_spain_effect_imm$variable_pred * df_spain_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="spain" & df_full_information$type=="immigration"] <- sum(df_spain_effect_imm$variable_pred_weighted)/sum(df_spain_effect_imm$weight)

# sweden - general
df_sweden_effect <- data.frame(model_data_sweden$gen_imm_ess_welfare, model_data_sweden$WEIGHTEX1)
names(df_sweden_effect)[1:2] <- c("variable","weight")
df_sweden_effect$variable_weighted <- df_sweden_effect$variable * df_sweden_effect$weight
df_full_information$actual[df_full_information$nationality=="sweden" & df_full_information$type=="general"] <- sum(df_sweden_effect$variable_weighted)/sum(df_sweden_effect$weight)
model_data_sweden$know_score_general_binary_original <- model_data_sweden$know_score_general_binary
model_data_sweden$know_score_general_binary <- 1
df_sweden_effect$variable_pred <- predict(m_econ_gen_sweden, type = "response", newdata = model_data_sweden)
df_sweden_effect$variable_pred_weighted <- df_sweden_effect$variable_pred * df_sweden_effect$weight
df_full_information$informed[df_full_information$nationality=="sweden" & df_full_information$type=="general"] <- sum(df_sweden_effect$variable_pred_weighted)/sum(df_sweden_effect$weight)

# sweden - immigration
df_sweden_effect_imm <- data.frame(model_data_sweden$gen_imm_ess_welfare, model_data_sweden$WEIGHTEX1)
names(df_sweden_effect_imm)[1:2] <- c("variable","weight")
df_sweden_effect_imm$variable_weighted <- df_sweden_effect_imm$variable * df_sweden_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="sweden" & df_full_information$type=="immigration"] <- sum(df_sweden_effect_imm$variable_weighted)/sum(df_sweden_effect_imm$weight)
model_data_sweden$know_score_imm_binary_original <- model_data_sweden$know_score_imm_binary
model_data_sweden$know_score_imm_binary <- 1
df_sweden_effect_imm$variable_pred <- predict(m_econ_imm_sweden, type = "response", newdata = model_data_sweden)
df_sweden_effect_imm$variable_pred_weighted <- df_sweden_effect_imm$variable_pred * df_sweden_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="sweden" & df_full_information$type=="immigration"] <- sum(df_sweden_effect_imm$variable_pred_weighted)/sum(df_sweden_effect_imm$weight)

# uk - general
df_uk_effect <- data.frame(model_data_uk$gen_imm_ess_welfare, model_data_uk$WEIGHTEX1)
names(df_uk_effect)[1:2] <- c("variable","weight")
df_uk_effect$variable_weighted <- df_uk_effect$variable * df_uk_effect$weight
df_full_information$actual[df_full_information$nationality=="uk" & df_full_information$type=="general"] <- sum(df_uk_effect$variable_weighted)/sum(df_uk_effect$weight)
model_data_uk$know_score_general_binary_original <- model_data_uk$know_score_general_binary
model_data_uk$know_score_general_binary <- 1
df_uk_effect$variable_pred <- predict(m_econ_gen_uk, type = "response", newdata = model_data_uk)
df_uk_effect$variable_pred_weighted <- df_uk_effect$variable_pred * df_uk_effect$weight
df_full_information$informed[df_full_information$nationality=="uk" & df_full_information$type=="general"] <- sum(df_uk_effect$variable_pred_weighted)/sum(df_uk_effect$weight)

# uk - immigration
df_uk_effect_imm <- data.frame(model_data_uk$gen_imm_ess_welfare, model_data_uk$WEIGHTEX1)
names(df_uk_effect_imm)[1:2] <- c("variable","weight")
df_uk_effect_imm$variable_weighted <- df_uk_effect_imm$variable * df_uk_effect_imm$weight
df_full_information$actual[df_full_information$nationality=="uk" & df_full_information$type=="immigration"] <- sum(df_uk_effect_imm$variable_weighted)/sum(df_uk_effect_imm$weight)
model_data_uk$know_score_imm_binary_original <- model_data_uk$know_score_imm_binary
model_data_uk$know_score_imm_binary <- 1
df_uk_effect_imm$variable_pred <- predict(m_econ_imm_uk, type = "response", newdata = model_data_uk)
df_uk_effect_imm$variable_pred_weighted <- df_uk_effect_imm$variable_pred * df_uk_effect_imm$weight
df_full_information$informed[df_full_information$nationality=="uk" & df_full_information$type=="immigration"] <- sum(df_uk_effect_imm$variable_pred_weighted)/sum(df_uk_effect_imm$weight)

# plot effects
df_plot <- gather(df_full_information, variable, effect, actual:informed, factor_key=TRUE)
ggplot(data=df_plot, aes(x=nationality, y=effect)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  facet_grid(. ~ type)
